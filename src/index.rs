use std::collections::hash_map::Entry;

use cargo::core::dependency::DepKind;
use cargo::core::{ActivationKey, Dependency, PackageId, SourceId, Summary};
use cargo::util::interning::InternedString;
use crates_index::{DependencyKind, GitIndex, Version as IndexVersion};
use pubgrub::{FxIndexMap, FxIndexSet};
use rustc_hash::FxHashMap;
use semver::Version;

fn make_summary(v: &IndexVersion, source_id: SourceId) -> anyhow::Result<Summary> {
    let pkg_id = PackageId::new(v.name().into(), v.version().parse()?, source_id);

    let dependencies = v
        .dependencies()
        .iter()
        .filter(|d| {
            if let Some(registry) = d.registry() {
                if SourceId::from_url(registry).ok() != Some(source_id) {
                    return false;
                }
            }
            true
        })
        .map(|d| {
            let mut dep = Dependency::parse(d.crate_name(), Some(d.requirement()), source_id)?;

            if d.package().is_some() {
                dep.set_explicit_name_in_toml(d.name());
            }

            match d.kind() {
                DependencyKind::Normal => dep.set_kind(DepKind::Normal),
                DependencyKind::Dev => dep.set_kind(DepKind::Development),
                DependencyKind::Build => dep.set_kind(DepKind::Build),
            };

            if let Some(target) = d.target() {
                dep.set_platform(Some(target.parse()?));
            }

            dep.set_optional(d.is_optional());
            dep.set_default_features(d.has_default_features());
            dep.set_features(d.features());

            Ok(dep)
        })
        .filter_map(|res: anyhow::Result<_>| res.ok())
        .collect();

    let features = v
        .features()
        .iter()
        .map(|(k, v)| (k.into(), v.iter().map(|f| f.into()).collect()))
        .collect();

    Summary::new(pkg_id, dependencies, &features, v.links(), None)
}

pub struct Crate {
    versions_by_priority: FxIndexMap<Version, Summary>,
    activation_keys_by_priority: FxIndexSet<ActivationKey>,
}

impl Crate {
    pub fn versions(&self) -> &FxIndexMap<Version, Summary> {
        &self.versions_by_priority
    }

    pub fn activation_keys(&self) -> &FxIndexSet<ActivationKey> {
        &self.activation_keys_by_priority
    }
}

pub struct Index {
    source_id: SourceId,
    git: GitIndex,
    cache: FxHashMap<InternedString, Crate>,
}

impl Index {
    pub fn new(source_id: SourceId, git: GitIndex) -> Self {
        Self {
            source_id,
            git,
            cache: FxHashMap::default(),
        }
    }

    pub fn get_ref(&self, name: InternedString) -> Option<&Crate> {
        self.cache.get(&name)
    }

    pub fn get(&mut self, name: InternedString) -> Option<&Crate> {
        match self.cache.entry(name) {
            Entry::Occupied(entry) => Some(entry.into_mut()),
            Entry::Vacant(entry) => {
                let mut versions_by_priority = self
                    .git
                    .crate_(&name)?
                    .versions()
                    .iter()
                    .filter(|v| !v.is_yanked())
                    .filter_map(|v| make_summary(v, self.source_id).ok())
                    .map(|s| (s.version().clone(), s))
                    .collect::<FxIndexMap<_, _>>();

                versions_by_priority.sort_unstable_keys();

                let mut activation_keys_by_priority = versions_by_priority
                    .values()
                    .map(|s| s.package_id().activation_key())
                    .collect::<FxIndexSet<_>>();

                activation_keys_by_priority.sort_unstable_by(|k1, k2| {
                    k1.semver_compatibility().cmp(&k2.semver_compatibility())
                });

                let cached_crate = Crate {
                    versions_by_priority,
                    activation_keys_by_priority,
                };

                Some(entry.insert(cached_crate))
            }
        }
    }

    pub fn versions(&mut self, name: InternedString) -> Option<&FxIndexMap<Version, Summary>> {
        Some(&self.get(name)?.versions_by_priority)
    }

    pub fn activation_keys(&mut self, name: InternedString) -> Option<&FxIndexSet<ActivationKey>> {
        Some(&self.get(name)?.activation_keys_by_priority)
    }
}
