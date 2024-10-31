use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::error::Error;
use std::fmt::{self, Display};

use cargo::core::dependency::DepKind;
use cargo::core::{ActivationKey, Dependency, FeatureValue, SourceId};
use cargo::util::interning::{InternedString, INTERNED_DEFAULT};
use cargo::util::OptVersionReq;
use pubgrub::{
    helpers::PackageVersionWrapper, Dependencies, DependencyProvider, FxIndexMap, FxIndexSet, Map,
    PackageArena, PackageId, VersionIndex, VersionSet,
};
use rustc_hash::{FxHashMap, FxHashSet};
use semver::{Op, VersionReq};

use crate::{
    index::{Index, IndexGuard},
    utils::{PackageVersionDisplay, PackageVersionSetDisplay, RcString},
};

const MAX_LINKS_VERSIONS: u64 = VersionIndex::MAX.pow(3);

type Arena = PackageArena<PackageVersionWrapper<PackageKind>>;

fn add_dep_package(
    d: PackageVersionWrapper<PackageKind>,
    vs: VersionSet,
    dep_map: &mut Map<PackageId, VersionSet>,
    arena: &mut Arena,
) {
    dep_map.insert(arena.insert(d), vs);
}

fn add_singleton_dep(
    pkg: impl Into<PackageKind>,
    true_version_index: u64,
    version_count: u64,
    dep_map: &mut Map<PackageId, VersionSet>,
    arena: &mut Arena,
) {
    let (d, vs) =
        PackageVersionWrapper::new_singleton_dep(pkg.into(), true_version_index, version_count);

    add_dep_package(d, vs, dep_map, arena);
}

fn add_dep(
    pkg: impl Into<PackageKind>,
    true_version_indices: impl IntoIterator<Item = u64>,
    version_count: u64,
    dep_map: &mut Map<PackageId, VersionSet>,
    arena: &mut Arena,
) {
    let (d, vs) = PackageVersionWrapper::new_dep(pkg.into(), true_version_indices, version_count);
    add_dep_package(d, vs, dep_map, arena);
}

fn is_dep_one_semver(dep: &Dependency) -> bool {
    let version_req = match dep.version_req() {
        OptVersionReq::Any => &VersionReq::STAR,
        OptVersionReq::Req(version_req) => version_req,
        OptVersionReq::Locked(_, version_req) => version_req,
        OptVersionReq::Precise(_, version_req) => version_req,
    };

    let [cmp] = version_req.comparators.as_slice() else {
        return false;
    };

    match cmp.op {
        Op::Caret => true,
        Op::Exact => {
            cmp.major != 0 || matches!(cmp.minor, Some(minor) if minor != 0) || cmp.patch.is_some()
        }
        Op::Tilde => cmp.major != 0 || matches!(cmp.minor, Some(minor) if minor != 0),
        _ => false,
    }
}

#[allow(clippy::too_many_arguments)]
fn add_dep_dependencies(
    parent_key: ActivationKey,
    parent_true_version_index: u64,
    dep_index: usize,
    dep: &Dependency,
    dep_feature_list: Option<&FxHashSet<InternedString>>,
    index: &mut impl IndexGuard,
    dep_map: &mut Map<PackageId, VersionSet>,
    arena: &mut Arena,
) -> Result<(), RcString> {
    let dep_package_name = dep.package_name();

    let Some(dep_crate) = index.get(dep_package_name) else {
        return Err("no matching dependency".into());
    };

    let mut compatible_summaries_iter =
        (dep_crate.versions().iter().enumerate()).filter(|&(_, (_, s))| {
            dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
        });

    let Some(first) = compatible_summaries_iter.next() else {
        return Err("no compatible version for dependency".into());
    };

    let mut compatible_activation_keys =
        FxIndexSet::from_iter([(first.1).1.package_id().activation_key()]);

    if dep_crate.activation_keys().len() > 1 && !is_dep_one_semver(dep) {
        compatible_activation_keys.extend(
            compatible_summaries_iter
                .clone()
                .map(|(_, (_, s))| s.package_id().activation_key()),
        );
    }

    if compatible_activation_keys.len() > 1 {
        let wide_version_indices = compatible_activation_keys
            .iter()
            .flat_map(|key| dep_crate.activation_keys().get_index_of(key))
            .map(|i| i as u64);

        let wide_count = dep_crate.activation_keys().len() as u64;

        let (wide_pkg, vs) = PackageVersionWrapper::new_dep(
            PackageKind::from(Wide {
                package_name: dep_package_name,
                parent_key,
                parent_true_version_index,
                parent_dep_index: dep_index,
            }),
            wide_version_indices,
            wide_count,
        );

        let mut added = false;

        if dep.uses_default_features() {
            let p = WideDefaultFeature {
                package_name: dep_package_name,
                parent_key,
                parent_true_version_index,
                parent_dep_index: dep_index,
            };
            add_dep_package(wide_pkg.replace_pkg(p.into()), vs, dep_map, arena);
            added = true;
        };

        for &feature_name in dep_feature_list.into_iter().flatten().chain(dep.features()) {
            let p = WideFeature {
                package_name: dep_package_name,
                feature_name,
                parent_key,
                parent_true_version_index,
                parent_dep_index: dep_index,
            };
            add_dep_package(wide_pkg.replace_pkg(p.into()), vs, dep_map, arena);
            added = true;
        }

        if !added {
            add_dep_package(wide_pkg, vs, dep_map, arena);
        }
    } else {
        let true_version_indices = compatible_summaries_iter
            .chain([first])
            .map(|(i, _)| i as u64);

        let version_count = dep_crate.versions().len() as u64;
        let key = (first.1).1.package_id().activation_key();

        let (dep_pkg, vs) = PackageVersionWrapper::new_dep(
            PackageKind::from(Pkg { key, root: false }),
            true_version_indices,
            version_count,
        );

        let mut added = false;

        if dep.uses_default_features() {
            let p = PkgDefaultFeature { key };
            add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
            added = true;
        };

        for &feature_name in dep_feature_list.into_iter().flatten().chain(dep.features()) {
            let p = PkgFeature { key, feature_name };
            add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
            added = true;
        }

        if !added {
            add_dep_package(dep_pkg, vs, dep_map, arena);
        }
    }

    Ok(())
}

#[derive(Debug, Copy, Clone)]
enum WideKind {
    Normal,
    Feature(InternedString),
    DefaultFeature,
}

#[allow(clippy::too_many_arguments)]
fn add_wide_dependencies(
    wide_kind: WideKind,
    package_name: InternedString,
    parent_key: ActivationKey,
    parent_true_version_index: u64,
    parent_dep_index: usize,
    true_version_index: u64,
    dep_map: &mut Map<PackageId, VersionSet>,
    index: &mut impl IndexGuard,
    arena: &mut Arena,
) -> Result<Result<(), RcString>, ResolveError> {
    let key = *index
        .activation_keys(package_name)
        .and_then(|map| map.get_index(true_version_index as usize))
        .ok_or(ResolveError)?;

    let version_count = index.versions(package_name).ok_or(ResolveError)?.len();

    let parent_summary = index
        .versions(parent_key.name())
        .and_then(|map| map.get(parent_true_version_index as usize))
        .map(|(_, s)| s.clone())
        .ok_or(ResolveError)?;

    let dep = parent_summary
        .dependencies()
        .get(parent_dep_index)
        .ok_or(ResolveError)?;

    let matching_versions_iter = index
        .versions(dep.package_name())
        .ok_or(ResolveError)?
        .iter()
        .enumerate()
        .filter(|&(_, (_, s))| {
            dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
        })
        .filter(|&(_, (_, s))| s.package_id().activation_key() == key);

    match wide_kind {
        WideKind::Normal => add_dep(
            Pkg { key, root: false },
            matching_versions_iter.map(|(i, _)| i as u64),
            version_count as u64,
            dep_map,
            arena,
        ),
        WideKind::Feature(feature_name) => add_dep(
            PkgFeature { key, feature_name },
            matching_versions_iter
                .filter(|&(_, (_, s))| s.features().contains_key(&feature_name))
                .map(|(i, _)| i as u64),
            version_count as u64,
            dep_map,
            arena,
        ),
        WideKind::DefaultFeature => add_dep(
            PkgDefaultFeature { key },
            matching_versions_iter.map(|(i, _)| i as u64),
            version_count as u64,
            dep_map,
            arena,
        ),
    };

    Ok(Ok(()))
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Pkg {
    pub key: ActivationKey,
    pub root: bool,
}

impl Display for Pkg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if self.root { "PkgRoot" } else { "Pkg" };
        let name = self.key.name();
        let compat = self.key.semver_compatibility();
        write!(f, "{prefix}:{name}#{compat}")
    }
}

impl Pkg {
    pub fn new_root(key: ActivationKey) -> Self {
        Self { key, root: true }
    }

    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        next_links_true_version_indices: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        let summary = index
            .versions(self.key.name())
            .and_then(|map| map.get(true_version_index as usize))
            .map(|(_, s)| s.clone())
            .ok_or(ResolveError)?;

        if let Some(links) = summary.links() {
            let next = next_links_true_version_indices.entry(links).or_default();
            if *next == MAX_LINKS_VERSIONS {
                return Err(ResolveError);
            }

            let links_true_version_index = *next;
            *next += 1;

            add_singleton_dep(
                Links { name: links },
                links_true_version_index,
                MAX_LINKS_VERSIONS,
                dep_map,
                arena,
            );
        }

        let mut features_deps = FxHashMap::<_, FxHashSet<_>>::default();

        if self.root {
            let mut weak_features_deps = FxHashMap::<_, FxHashSet<_>>::default();

            for feature_values in summary.features().values() {
                for feature_value in feature_values {
                    match *feature_value {
                        FeatureValue::Feature(_) => (),
                        FeatureValue::Dep { dep_name } => {
                            if let Entry::Vacant(entry) = features_deps.entry(dep_name) {
                                entry.insert(FxHashSet::default());
                            }
                        }
                        FeatureValue::DepFeature {
                            dep_name,
                            dep_feature: dep_feature_name,
                            weak,
                        } => {
                            if weak {
                                weak_features_deps
                                    .entry(dep_name)
                                    .or_default()
                                    .insert(dep_feature_name);
                            } else {
                                features_deps
                                    .entry(dep_name)
                                    .or_default()
                                    .insert(dep_feature_name);
                            }
                        }
                    }
                }
            }

            for (dep_name, dep_feature_names) in weak_features_deps {
                if let Some(dep_feature_list) = features_deps.get_mut(&dep_name) {
                    dep_feature_list.extend(dep_feature_names);
                }
            }
        }

        for (dep_index, dep) in summary.dependencies().iter().enumerate() {
            if dep.kind() == DepKind::Development && !self.root {
                continue;
            }

            let dep_feature_list = features_deps.get(&dep.name_in_toml());

            if dep.is_optional() && dep_feature_list.is_none() {
                continue;
            }

            if let Err(error) = add_dep_dependencies(
                self.key,
                true_version_index,
                dep_index,
                dep,
                dep_feature_list,
                index,
                dep_map,
                arena,
            ) {
                return Ok(Err(format!("{error}: {}", dep.package_name()).into()));
            }
        }

        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PkgFeature {
    pub key: ActivationKey,
    pub feature_name: InternedString,
}

impl Display for PkgFeature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.key.name();
        let compat = self.key.semver_compatibility();
        let feature_name = self.feature_name;
        write!(f, "Pkg:{name}#{compat}/{feature_name}")
    }
}

impl PkgFeature {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        let (summary, version_count) = index
            .versions(self.key.name())
            .and_then(|map| Some((map.get(true_version_index as usize)?, map.len())))
            .map(|((_, s), count)| (s.clone(), count as u64))
            .ok_or(ResolveError)?;

        let Some(feature_values) = summary.features().get(&self.feature_name) else {
            return Ok(Err("no matching feature".into()));
        };

        let (dep_pkg, vs) = PackageVersionWrapper::new_singleton_dep(
            PackageKind::from(Pkg {
                key: self.key,
                root: false,
            }),
            true_version_index,
            version_count,
        );

        for feature_value in feature_values {
            match *feature_value {
                FeatureValue::Feature(other_feature_name) => {
                    let p = PkgFeature {
                        key: self.key,
                        feature_name: other_feature_name,
                    };
                    add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                }
                FeatureValue::Dep { dep_name } => add_singleton_dep(
                    PkgDep {
                        key: self.key,
                        dep_name,
                    },
                    true_version_index,
                    version_count + 1,
                    dep_map,
                    arena,
                ),
                FeatureValue::DepFeature {
                    dep_name,
                    dep_feature: dep_feature_name,
                    weak,
                } => {
                    let pkg = PkgDepFeature {
                        key: self.key,
                        dep_name,
                        dep_feature_name,
                    };
                    if weak {
                        let true_version_indices = [true_version_index, version_count];
                        add_dep(pkg, true_version_indices, version_count + 1, dep_map, arena);
                    } else {
                        add_singleton_dep(
                            pkg,
                            true_version_index,
                            version_count + 1,
                            dep_map,
                            arena,
                        );

                        if dep_name != self.feature_name
                            && summary.features().contains_key(&dep_name)
                            && summary
                                .dependencies()
                                .iter()
                                .any(|dep| dep.name_in_toml() == dep_name && dep.is_optional())
                        {
                            let p = PkgFeature {
                                key: self.key,
                                feature_name: dep_name,
                            };
                            add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                        }
                    }
                }
            }
        }

        add_dep_package(dep_pkg, vs, dep_map, arena);

        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PkgDefaultFeature {
    pub key: ActivationKey,
}

impl Display for PkgDefaultFeature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.key.name();
        let compat = self.key.semver_compatibility();
        write!(f, "Pkg:{name}#{compat}/default=true")
    }
}

impl PkgDefaultFeature {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        let (summary, version_count) = index
            .versions(self.key.name())
            .and_then(|map| Some((map.get(true_version_index as usize)?, map.len())))
            .map(|((_, s), count)| (s.clone(), count as u64))
            .ok_or(ResolveError)?;

        let p = if summary.features().contains_key(&INTERNED_DEFAULT) {
            PackageKind::from(PkgFeature {
                key: self.key,
                feature_name: INTERNED_DEFAULT,
            })
        } else {
            PackageKind::from(Pkg {
                key: self.key,
                root: false,
            })
        };

        let (dep_pkg, vs) =
            PackageVersionWrapper::new_singleton_dep(p, true_version_index, version_count);

        add_dep_package(dep_pkg, vs, dep_map, arena);

        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PkgDep {
    pub key: ActivationKey,
    pub dep_name: InternedString,
}

impl Display for PkgDep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.key.name();
        let compat = self.key.semver_compatibility();
        let dep_name = self.dep_name;
        write!(f, "PkgDep:{name}#{compat}->{dep_name}")
    }
}

impl PkgDep {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        let index_versions = index.versions(self.key.name()).ok_or(ResolveError)?;

        // Special value to disable dependency, used for weak features
        if true_version_index as usize == index_versions.len() {
            return Ok(Ok(()));
        }

        let summary = index_versions
            .get(true_version_index as usize)
            .map(|(_, s)| s.clone())
            .ok_or(ResolveError)?;

        for (dep_index, dep) in summary.dependencies().iter().enumerate() {
            if dep.name_in_toml() != self.dep_name {
                continue;
            }
            if dep.kind() == DepKind::Development {
                continue;
            }
            if !dep.is_optional() {
                continue;
            }

            if let Err(error) = add_dep_dependencies(
                self.key,
                true_version_index,
                dep_index,
                dep,
                None,
                index,
                dep_map,
                arena,
            ) {
                return Ok(Err(error));
            }
        }

        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PkgDepFeature {
    pub key: ActivationKey,
    pub dep_name: InternedString,
    pub dep_feature_name: InternedString,
}

impl Display for PkgDepFeature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.key.name();
        let compat = self.key.semver_compatibility();
        let dep_name = self.dep_name;
        let dep_feature_name = self.dep_feature_name;
        write!(f, "PkgDep:{name}#{compat}->{dep_name}/{dep_feature_name}")
    }
}

impl PkgDepFeature {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        let index_versions = index.versions(self.key.name()).ok_or(ResolveError)?;

        add_singleton_dep(
            PkgDep {
                key: self.key,
                dep_name: self.dep_name,
            },
            true_version_index,
            index_versions.len() as u64 + 1,
            dep_map,
            arena,
        );

        // Special value to disable dependency, used for weak features
        if true_version_index as usize == index_versions.len() {
            return Ok(Ok(()));
        }

        let summary = index_versions
            .get(true_version_index as usize)
            .map(|(_, s)| s.clone())
            .ok_or(ResolveError)?;

        for (dep_index, dep) in summary.dependencies().iter().enumerate() {
            if dep.name_in_toml() != self.dep_name {
                continue;
            }
            if dep.kind() == DepKind::Development {
                continue;
            }

            let dep_package_name = dep.package_name();

            let Some(dep_crate) = index.get(dep_package_name) else {
                return Ok(Err("no matching dependency".into()));
            };

            let mut compatible_summaries_iter = (dep_crate.versions().iter().enumerate())
                .filter(|&(_, (_, s))| {
                    dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
                })
                .filter(|&(_, (_, s))| s.features().contains_key(&self.dep_feature_name));

            let Some(first) = compatible_summaries_iter.next() else {
                return Ok(Err("no compatible version for dependency".into()));
            };

            let mut compatible_activation_keys =
                FxIndexSet::from_iter([(first.1).1.package_id().activation_key()]);

            if dep_crate.activation_keys().len() > 1 && !is_dep_one_semver(dep) {
                compatible_activation_keys.extend(
                    compatible_summaries_iter
                        .clone()
                        .map(|(_, (_, s))| s.package_id().activation_key()),
                );
            }

            if compatible_activation_keys.len() > 1 {
                let wide_version_indices = compatible_activation_keys
                    .iter()
                    .flat_map(|key| dep_crate.activation_keys().get_index_of(key))
                    .map(|i| i as u64);

                let wide_count = dep_crate.activation_keys().len() as u64;

                add_dep(
                    WideFeature {
                        package_name: dep_package_name,
                        feature_name: self.dep_feature_name,
                        parent_key: self.key,
                        parent_true_version_index: true_version_index,
                        parent_dep_index: dep_index,
                    },
                    wide_version_indices,
                    wide_count,
                    dep_map,
                    arena,
                );
            } else {
                let true_version_indices = compatible_summaries_iter
                    .chain([first])
                    .map(|(i, _)| i as u64);

                let version_count = dep_crate.versions().len() as u64;
                let key = (first.1).1.package_id().activation_key();

                add_dep(
                    PkgFeature {
                        key,
                        feature_name: self.dep_feature_name,
                    },
                    true_version_indices,
                    version_count,
                    dep_map,
                    arena,
                );
            }
        }

        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Wide {
    pub package_name: InternedString,
    pub parent_key: ActivationKey,
    pub parent_true_version_index: u64,
    pub parent_dep_index: usize,
}

impl Display for Wide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.package_name;
        write!(f, "Wide:{name}")
    }
}

impl Wide {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        add_wide_dependencies(
            WideKind::Normal,
            self.package_name,
            self.parent_key,
            self.parent_true_version_index,
            self.parent_dep_index,
            true_version_index,
            dep_map,
            index,
            arena,
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WideFeature {
    pub package_name: InternedString,
    pub feature_name: InternedString,
    pub parent_key: ActivationKey,
    pub parent_true_version_index: u64,
    pub parent_dep_index: usize,
}

impl Display for WideFeature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.package_name;
        let feature_name = self.feature_name;
        write!(f, "Wide:{name}/{feature_name}")
    }
}

impl WideFeature {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        add_wide_dependencies(
            WideKind::Feature(self.feature_name),
            self.package_name,
            self.parent_key,
            self.parent_true_version_index,
            self.parent_dep_index,
            true_version_index,
            dep_map,
            index,
            arena,
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WideDefaultFeature {
    pub package_name: InternedString,
    pub parent_key: ActivationKey,
    pub parent_true_version_index: u64,
    pub parent_dep_index: usize,
}

impl Display for WideDefaultFeature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.package_name;
        write!(f, "Wide:{name}/default=true")
    }
}

impl WideDefaultFeature {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        add_wide_dependencies(
            WideKind::DefaultFeature,
            self.package_name,
            self.parent_key,
            self.parent_true_version_index,
            self.parent_dep_index,
            true_version_index,
            dep_map,
            index,
            arena,
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Links {
    pub name: InternedString,
}

impl Display for Links {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Links:{}", self.name)
    }
}

impl Links {
    fn add_dependencies(
        &self,
        _: u64,
        _: &mut Map<PackageId, VersionSet>,
        _: &mut impl IndexGuard,
        _: &mut Arena,
        _: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum PackageKind {
    Pkg(Pkg),
    PkgFeature(PkgFeature),
    PkgDefaultFeature(PkgDefaultFeature),
    PkgDep(PkgDep),
    PkgDepFeature(PkgDepFeature),
    Wide(Wide),
    WideFeature(WideFeature),
    WideDefaultFeature(WideDefaultFeature),
    Links(Links),
}

macro_rules! impl_from {
    ($($name:ident),* $(,)?) => {
        $(
            impl From<$name> for PackageKind {
                fn from(value: $name) -> Self {
                    Self::$name(value)
                }
            }
        )*
    };
}

impl_from!(
    Pkg,
    PkgFeature,
    PkgDefaultFeature,
    PkgDep,
    PkgDepFeature,
    Wide,
    WideFeature,
    WideDefaultFeature,
    Links,
);

impl Display for PackageKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PackageKind::Pkg(p) => write!(f, "{p}"),
            PackageKind::PkgFeature(p) => write!(f, "{p}"),
            PackageKind::PkgDefaultFeature(p) => write!(f, "{p}"),
            PackageKind::PkgDep(p) => write!(f, "{p}"),
            PackageKind::PkgDepFeature(p) => write!(f, "{p}"),
            PackageKind::Wide(p) => write!(f, "{p}"),
            PackageKind::WideFeature(p) => write!(f, "{p}"),
            PackageKind::WideDefaultFeature(p) => write!(f, "{p}"),
            PackageKind::Links(p) => write!(f, "{p}"),
        }
    }
}

impl PackageKind {
    fn add_dependencies(
        &self,
        true_version_index: u64,
        dep_map: &mut Map<PackageId, VersionSet>,
        index: &mut impl IndexGuard,
        arena: &mut Arena,
        next_links_true_version: &mut FxIndexMap<InternedString, u64>,
    ) -> Result<Result<(), RcString>, ResolveError> {
        macro_rules! add_dependencies {
            ($($name:ident),* $(,)?) => {
                match self {
                    $(
                        PackageKind::$name(p) => {
                            p.add_dependencies(
                                true_version_index,
                                dep_map,
                                index,
                                arena,
                                next_links_true_version,
                            )
                        }
                    )*
                }
            };
        }

        add_dependencies!(
            Pkg,
            PkgFeature,
            PkgDefaultFeature,
            PkgDep,
            PkgDepFeature,
            Wide,
            WideFeature,
            WideDefaultFeature,
            Links,
        )
    }
}

#[derive(Debug)]
pub struct ResolveError;

impl Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ResolveError")
    }
}

impl Error for ResolveError {}

pub struct Provider<'a, I: Index> {
    index: &'a I,
    conflicts: FxHashMap<InternedString, u64>,
    next_links_true_versions: FxIndexMap<InternedString, u64>,
}

impl<'a, I: Index> Provider<'a, I> {
    pub fn new(index: &'a I) -> Self {
        Self {
            index,
            conflicts: FxHashMap::default(),
            next_links_true_versions: FxIndexMap::default(),
        }
    }

    pub fn get_root(
        &mut self,
        root_name: InternedString,
        root_version: &semver::Version,
        root_source_id: SourceId,
    ) -> Option<(PackageVersionWrapper<PackageKind>, VersionIndex)> {
        let mut index = self.index.lock();
        let root_versions = index.versions(root_name)?;

        let true_version_index = root_versions
            .iter()
            .enumerate()
            .find(|&(_, (v, _))| v == root_version)
            .map(|(i, _)| i as u64)?;

        let (root_pkg, root_version_index) = PackageVersionWrapper::new_pkg(
            PackageKind::from(Pkg::new_root(
                (root_name, root_source_id, root_version.into()).into(),
            )),
            true_version_index,
            (root_versions.len() as u64).try_into().ok()?,
        );

        Some((root_pkg, root_version_index))
    }

    pub fn index(&self) -> &'a I {
        self.index
    }
}

impl<I: Index> DependencyProvider for Provider<'_, I> {
    type P = PackageVersionWrapper<PackageKind>;
    type M = RcString;
    type Priority = Reverse<u64>;
    type Err = ResolveError;

    fn prioritize(
        &mut self,
        package_id: PackageId,
        set: VersionSet,
        arena: &PackageArena<Self::P>,
    ) -> Self::Priority {
        let version_count = set.count();
        if version_count == 0 {
            return Reverse(0);
        }
        let pkg = match arena.pkg(package_id).unwrap() {
            PackageVersionWrapper::Pkg(p) => p.pkg(),
            PackageVersionWrapper::VirtualPkg(p) => p.pkg(),
            PackageVersionWrapper::VirtualDep(p) => p.pkg(),
        };
        let name = match pkg {
            PackageKind::Pkg(p) => p.key.name(),
            PackageKind::PkgFeature(p) => p.key.name(),
            PackageKind::PkgDefaultFeature(p) => p.key.name(),
            PackageKind::PkgDep(p) => p.key.name(),
            PackageKind::PkgDepFeature(p) => p.key.name(),
            PackageKind::Wide(p) => p.package_name,
            PackageKind::WideFeature(p) => p.package_name,
            PackageKind::WideDefaultFeature(p) => p.package_name,
            PackageKind::Links(p) => p.name,
        };
        let conflict_count = self.conflicts.get(&name).copied().unwrap_or_default();

        Reverse(((u32::MAX as u64).saturating_sub(conflict_count) << 6) + version_count as u64)
    }

    fn choose_version(
        &mut self,
        _: PackageId,
        set: VersionSet,
        _: &PackageArena<Self::P>,
    ) -> Result<Option<VersionIndex>, Self::Err> {
        Ok(set.last())
    }

    fn get_dependencies(
        &mut self,
        package_id: PackageId,
        version_index: VersionIndex,
        arena: &mut Arena,
    ) -> Result<pubgrub::Dependencies<Self::M>, Self::Err> {
        let mut dep_map = Map::default();

        let wrapper = arena.pkg(package_id).ok_or(ResolveError)?;
        let inner = wrapper.inner(version_index).map(|(p, v)| (p.clone(), v));

        if let Some((d, vs)) = wrapper.dependency(version_index) {
            dep_map.insert(arena.insert(d), vs);
        }

        if let Some((pkg, true_version_index)) = inner {
            if let Err(error) = pkg.add_dependencies(
                true_version_index,
                &mut dep_map,
                &mut self.index.lock(),
                arena,
                &mut self.next_links_true_versions,
            )? {
                return Ok(Dependencies::Unavailable(error));
            }
        }

        Ok(Dependencies::Available(dep_map))
    }

    fn package_version_display<'a>(
        &'a self,
        pkg: &'a PackageVersionWrapper<PackageKind>,
        version_index: VersionIndex,
    ) -> impl Display + 'a {
        PackageVersionDisplay {
            provider: self,
            pkg,
            version_index,
        }
    }

    fn package_version_set_display<'a>(
        &'a self,
        pkg: &'a Self::P,
        version_set: VersionSet,
    ) -> impl Display + 'a {
        PackageVersionSetDisplay {
            provider: self,
            pkg,
            version_set,
        }
    }

    fn register_conflict(
        &mut self,
        package_ids: impl Iterator<Item = PackageId>,
        arena: &PackageArena<Self::P>,
    ) {
        for package_id in package_ids {
            let pkg = match arena.pkg(package_id).unwrap() {
                PackageVersionWrapper::Pkg(p) => p.pkg(),
                PackageVersionWrapper::VirtualPkg(p) => p.pkg(),
                PackageVersionWrapper::VirtualDep(p) => p.pkg(),
            };
            let name = match pkg {
                PackageKind::Pkg(p) => p.key.name(),
                PackageKind::PkgFeature(p) => p.key.name(),
                PackageKind::PkgDefaultFeature(p) => p.key.name(),
                PackageKind::PkgDep(p) => p.key.name(),
                PackageKind::PkgDepFeature(p) => p.key.name(),
                PackageKind::Wide(p) => p.package_name,
                PackageKind::WideFeature(p) => p.package_name,
                PackageKind::WideDefaultFeature(p) => p.package_name,
                PackageKind::Links(p) => p.name,
            };
            *self.conflicts.entry(name).or_default() += 1;
        }
    }
}
