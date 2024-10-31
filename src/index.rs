use std::collections::hash_map::Entry;
use std::sync::{Arc, Mutex, MutexGuard};

use cargo::core::dependency::DepKind;
use cargo::core::{ActivationKey, Dependency, PackageId, SourceId, Summary};
use cargo::util::interning::InternedString;
use crates_index::{DependencyKind, GitIndex, Version as IndexVersion};
use pubgrub::FxIndexSet;
use rayon::prelude::*;
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

#[derive(Debug, Default)]
pub struct Crate {
    versions_by_priority: Vec<(Version, Summary)>,
    activation_keys_by_priority: FxIndexSet<ActivationKey>,
}

impl Crate {
    pub fn versions(&self) -> &[(Version, Summary)] {
        &self.versions_by_priority
    }

    pub fn activation_keys(&self) -> &FxIndexSet<ActivationKey> {
        &self.activation_keys_by_priority
    }
}

pub struct GitRepo {
    source_id: SourceId,
    git: GitIndex,
}

impl GitRepo {
    pub fn new(source_id: SourceId, git: GitIndex) -> Self {
        Self { source_id, git }
    }

    pub fn load_one(&self, name: InternedString) -> Option<Crate> {
        Some(Self::load(self.source_id, self.git.crate_(&name)?))
    }

    pub fn load_all(&self) -> FxHashMap<InternedString, Crate> {
        let source_id = self.source_id;
        self.git
            .crates_parallel()
            .filter_map(|k| k.ok())
            .map(|k| (k.name().into(), Self::load(source_id, k)))
            .collect()
    }

    pub fn load(source_id: SourceId, git_crate: crates_index::Crate) -> Crate {
        let mut versions_by_priority = git_crate
            .versions()
            .iter()
            .filter(|v| !v.is_yanked())
            .filter_map(|v| make_summary(v, source_id).ok())
            .map(|s| (s.version().clone(), s))
            .collect::<Vec<_>>();

        versions_by_priority.sort_unstable_by(|(v1, _), (v2, _)| v1.cmp(v2));

        let mut activation_keys_by_priority = versions_by_priority
            .iter()
            .map(|(_, s)| s.package_id().activation_key())
            .collect::<FxIndexSet<_>>();

        activation_keys_by_priority
            .sort_unstable_by(|k1, k2| k1.semver_compatibility().cmp(&k2.semver_compatibility()));

        Crate {
            versions_by_priority,
            activation_keys_by_priority,
        }
    }
}

pub trait Index {
    type Guard<'a>: IndexGuard
    where
        Self: 'a;

    fn lock(&self) -> Self::Guard<'_>;
}

pub trait IndexGuard {
    fn get(&mut self, name: InternedString) -> Option<&Crate>;

    fn versions(&mut self, name: InternedString) -> Option<&[(Version, Summary)]> {
        Some(&self.get(name)?.versions_by_priority)
    }

    fn activation_keys(&mut self, name: InternedString) -> Option<&FxIndexSet<ActivationKey>> {
        Some(&self.get(name)?.activation_keys_by_priority)
    }
}

pub struct OnDemandIndex {
    git_repo: GitRepo,
    cache: Arc<Mutex<FxHashMap<InternedString, Crate>>>,
}

impl OnDemandIndex {
    pub fn new(git_repo: GitRepo) -> Self {
        Self {
            git_repo,
            cache: Arc::default(),
        }
    }
}

impl Index for OnDemandIndex {
    type Guard<'a> = OnDemandIndexGuard<'a>;

    fn lock(&self) -> Self::Guard<'_> {
        OnDemandIndexGuard {
            git_repo: &self.git_repo,
            cache: self.cache.lock().unwrap(),
        }
    }
}

pub struct OnDemandIndexGuard<'a> {
    git_repo: &'a GitRepo,
    cache: MutexGuard<'a, FxHashMap<InternedString, Crate>>,
}

impl IndexGuard for OnDemandIndexGuard<'_> {
    fn get(&mut self, name: InternedString) -> Option<&Crate> {
        match self.cache.entry(name) {
            Entry::Occupied(entry) => Some(entry.into_mut()),
            Entry::Vacant(entry) => Some(entry.insert(self.git_repo.load_one(name)?)),
        }
    }
}

#[derive(Default, Clone, Copy, Eq, PartialEq)]
pub enum VersionOrdering {
    #[default]
    MaximumVersionsFirst,
    MinimumVersionsFirst,
}

pub struct PreloadedIndex {
    registry: FxHashMap<InternedString, Crate>,
}

impl PreloadedIndex {
    pub fn from_registry(pkgs: Vec<Summary>, version_ordering: VersionOrdering) -> Self {
        let mut registry = FxHashMap::<_, Crate>::default();

        for pkg in pkgs {
            let c = registry.entry(pkg.name()).or_default();

            c.activation_keys_by_priority
                .insert(pkg.package_id().activation_key());

            c.versions_by_priority.push((pkg.version().clone(), pkg));
        }

        for c in registry.values_mut() {
            match version_ordering {
                VersionOrdering::MaximumVersionsFirst => {
                    c.versions_by_priority
                        .sort_unstable_by(|(v1, _), (v2, _)| v1.cmp(v2));

                    c.activation_keys_by_priority.sort_unstable_by(|k1, k2| {
                        k1.semver_compatibility().cmp(&k2.semver_compatibility())
                    });
                }
                VersionOrdering::MinimumVersionsFirst => {
                    c.versions_by_priority
                        .sort_unstable_by(|(v1, _), (v2, _)| v2.cmp(v1));

                    c.activation_keys_by_priority.sort_unstable_by(|k1, k2| {
                        k2.semver_compatibility().cmp(&k1.semver_compatibility())
                    });
                }
            }
        }

        Self { registry }
    }

    pub fn from_git(git_repo: GitRepo) -> Self {
        Self {
            registry: git_repo.load_all(),
        }
    }

    pub fn registry(&self) -> &FxHashMap<InternedString, Crate> {
        &self.registry
    }
}

impl Index for PreloadedIndex {
    type Guard<'a> = PreloadedIndexGuard<'a>;

    fn lock(&self) -> Self::Guard<'_> {
        PreloadedIndexGuard {
            registry: &self.registry,
        }
    }
}

pub struct PreloadedIndexGuard<'a> {
    registry: &'a FxHashMap<InternedString, Crate>,
}

impl IndexGuard for PreloadedIndexGuard<'_> {
    fn get(&mut self, name: InternedString) -> Option<&Crate> {
        self.registry.get(&name)
    }
}
