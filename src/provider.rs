use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::rc::Rc;

use cargo::core::dependency::DepKind;
use cargo::core::{ActivationKey, FeatureValue, SourceId};
use cargo::util::interning::{InternedString, INTERNED_DEFAULT};
use cargo::util::OptVersionReq;
use pubgrub::{
    Dependencies, DependencyProvider, FxIndexSet, Map, Package, PackageVersionWrapper, Version,
    VersionSet,
};
use rustc_hash::{FxHashMap, FxHashSet};
use semver::{Comparator, Op, VersionReq};

use crate::index::Index;
use crate::utils::{PackageVersionDisplay, PackageVersionSetDisplay};

const MAX_LINKS_VERSIONS: u64 = Version::MAX.pow(3);

type Arena = FxIndexSet<PackageVersionWrapper<PackageKind>>;

fn add_dep_package(
    d: PackageVersionWrapper<PackageKind>,
    vs: VersionSet,
    dep_map: &mut Map<Package, VersionSet>,
    arena: &mut Arena,
) {
    let p = Package(arena.insert_full(d).0 as u32);
    dep_map.insert(p, vs);
}

fn add_singleton_dep(
    pkg: impl Into<PackageKind>,
    true_version: u64,
    version_count: u64,
    dep_map: &mut Map<Package, VersionSet>,
    arena: &mut Arena,
) {
    let (d, vs) = PackageVersionWrapper::new_singleton_dep(pkg.into(), true_version, version_count);
    add_dep_package(d, vs, dep_map, arena);
}

fn add_dep(
    pkg: impl Into<PackageKind>,
    true_versions: impl IntoIterator<Item = u64>,
    version_count: u64,
    dep_map: &mut Map<Package, VersionSet>,
    arena: &mut Arena,
) {
    let (d, vs) = PackageVersionWrapper::new_dep(pkg.into(), true_versions, version_count);
    add_dep_package(d, vs, dep_map, arena);
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        next_links_true_version: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let summary = index
            .versions(self.key.name())
            .and_then(|map| map.get_index(true_version as usize))
            .map(|(_, s)| s.clone())
            .ok_or(ResolveError)?;

        if let Some(links) = summary.links() {
            if *next_links_true_version == MAX_LINKS_VERSIONS {
                return Err(ResolveError);
            }

            let links_true_version = *next_links_true_version;
            *next_links_true_version += 1;

            add_singleton_dep(
                Links { name: links },
                links_true_version,
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

            let dep_package_name = dep.package_name();

            let Some(dep_crate) = index.get(dep_package_name) else {
                let msg = format!("no matching dependency: {dep_package_name}");
                return Ok(Err(msg.into()));
            };

            let mut compatible_summaries_iter =
                dep_crate.versions().values().enumerate().filter(|&(_, s)| {
                    dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
                });

            let Some(first) = compatible_summaries_iter.next() else {
                let msg = format!("no compatible version for dependency: {dep_package_name}");
                return Ok(Err(msg.into()));
            };

            let version_req = match dep.version_req() {
                OptVersionReq::Any => &VersionReq::STAR,
                OptVersionReq::Req(version_req) => version_req,
                OptVersionReq::Locked(_, version_req) => version_req,
                OptVersionReq::Precise(_, version_req) => version_req,
            };

            let mut compatible_activation_keys =
                FxIndexSet::from_iter([first.1.package_id().activation_key()]);

            if dep_crate.activation_keys().len() > 1
                && !matches!(
                    version_req.comparators.as_slice(),
                    [Comparator {
                        op: Op::Exact | Op::Caret,
                        ..
                    }]
                )
            {
                compatible_activation_keys.extend(
                    compatible_summaries_iter
                        .clone()
                        .map(|(_, s)| s.package_id().activation_key()),
                );
            }

            if compatible_activation_keys.len() > 1 {
                let wide_versions = compatible_activation_keys
                    .iter()
                    .flat_map(|key| dep_crate.activation_keys().get_index_of(key))
                    .map(|i| i as u64);

                let wide_count = dep_crate.activation_keys().len() as u64;

                let (wide_pkg, vs) = PackageVersionWrapper::new_dep(
                    PackageKind::from(Wide {
                        package_name: dep_package_name,
                        parent_key: self.key,
                        parent_true_version: true_version,
                        parent_dep_index: dep_index,
                    }),
                    wide_versions,
                    wide_count,
                );

                if dep.uses_default_features() {
                    let p = WideDefaultFeature {
                        package_name: dep_package_name,
                        parent_key: self.key,
                        parent_true_version: true_version,
                        parent_dep_index: dep_index,
                    };
                    add_dep_package(wide_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                };

                for &feature_name in dep_feature_list.into_iter().flatten().chain(dep.features()) {
                    let p = WideFeature {
                        package_name: dep_package_name,
                        feature_name,
                        parent_key: self.key,
                        parent_true_version: true_version,
                        parent_dep_index: dep_index,
                    };
                    add_dep_package(wide_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                }

                add_dep_package(wide_pkg, vs, dep_map, arena);
            } else {
                let true_versions = compatible_summaries_iter
                    .chain([first])
                    .map(|(i, _)| i as u64);

                let version_count = dep_crate.versions().len() as u64;
                let key = first.1.package_id().activation_key();

                let (dep_pkg, vs) = PackageVersionWrapper::new_dep(
                    PackageKind::from(Pkg { key, root: false }),
                    true_versions,
                    version_count,
                );

                if dep.uses_default_features() {
                    let p = PkgDefaultFeature { key };
                    add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                };

                for &feature_name in dep_feature_list.into_iter().flatten().chain(dep.features()) {
                    let p = PkgFeature { key, feature_name };
                    add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                }

                add_dep_package(dep_pkg, vs, dep_map, arena);
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let (summary, version_count) = index
            .versions(self.key.name())
            .and_then(|map| Some((map.get_index(true_version as usize)?, map.len())))
            .map(|((_, s), count)| (s.clone(), count as u64))
            .ok_or(ResolveError)?;

        let Some(feature_values) = summary.features().get(&self.feature_name) else {
            return Ok(Err("no matching feature".into()));
        };

        let key = summary.package_id().activation_key();

        let (dep_pkg, vs) = PackageVersionWrapper::new_singleton_dep(
            PackageKind::from(Pkg { key, root: false }),
            true_version,
            version_count,
        );

        for feature_value in feature_values {
            match *feature_value {
                FeatureValue::Feature(other_feature_name) => {
                    let p = PkgFeature {
                        key,
                        feature_name: other_feature_name,
                    };
                    add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                }
                FeatureValue::Dep { dep_name } => add_singleton_dep(
                    PkgDep { key, dep_name },
                    true_version,
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
                        key,
                        dep_name,
                        dep_feature_name,
                    };
                    if weak {
                        let true_versions = [true_version, version_count];
                        add_dep(pkg, true_versions, version_count + 1, dep_map, arena);
                    } else {
                        add_singleton_dep(pkg, true_version, version_count + 1, dep_map, arena);

                        if dep_name != self.feature_name
                            && summary
                                .dependencies()
                                .iter()
                                .any(|dep| dep.name_in_toml() == dep_name && dep.is_optional())
                        {
                            let p = PkgFeature {
                                key,
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let (summary, version_count) = index
            .versions(self.key.name())
            .and_then(|map| Some((map.get_index(true_version as usize)?, map.len())))
            .map(|((_, s), count)| (s.clone(), count as u64))
            .ok_or(ResolveError)?;

        let key = summary.package_id().activation_key();

        let (dep_pkg, vs) = PackageVersionWrapper::new_singleton_dep(
            PackageKind::from(Pkg { key, root: false }),
            true_version,
            version_count,
        );

        if summary.features().contains_key(&INTERNED_DEFAULT) {
            let p = PkgFeature {
                key,
                feature_name: INTERNED_DEFAULT,
            };
            add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
        }

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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let index_versions = index.versions(self.key.name()).ok_or(ResolveError)?;

        // Special value to disable dependency, used for weak features
        if true_version as usize == index_versions.len() {
            return Ok(Ok(()));
        }

        let summary = index_versions
            .get_index(true_version as usize)
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

            let dep_package_name = dep.package_name();

            let Some(dep_crate) = index.get(dep_package_name) else {
                return Ok(Err("no matching dependency".into()));
            };

            let mut compatible_summaries_iter =
                dep_crate.versions().values().enumerate().filter(|&(_, s)| {
                    dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
                });

            let Some(first) = compatible_summaries_iter.next() else {
                return Ok(Err("no compatible version for dependency".into()));
            };

            let version_req = match dep.version_req() {
                OptVersionReq::Any => &VersionReq::STAR,
                OptVersionReq::Req(version_req) => version_req,
                OptVersionReq::Locked(_, version_req) => version_req,
                OptVersionReq::Precise(_, version_req) => version_req,
            };

            let mut compatible_activation_keys =
                FxIndexSet::from_iter([first.1.package_id().activation_key()]);

            if dep_crate.activation_keys().len() > 1
                && !matches!(
                    version_req.comparators.as_slice(),
                    [Comparator {
                        op: Op::Exact | Op::Caret,
                        ..
                    }]
                )
            {
                compatible_activation_keys.extend(
                    compatible_summaries_iter
                        .clone()
                        .map(|(_, s)| s.package_id().activation_key()),
                );
            }

            if compatible_activation_keys.len() > 1 {
                let wide_versions = compatible_activation_keys
                    .iter()
                    .flat_map(|key| dep_crate.activation_keys().get_index_of(key))
                    .map(|i| i as u64);

                let wide_count = dep_crate.activation_keys().len() as u64;

                let (wide_pkg, vs) = PackageVersionWrapper::new_dep(
                    PackageKind::from(Wide {
                        package_name: dep_package_name,
                        parent_key: self.key,
                        parent_true_version: true_version,
                        parent_dep_index: dep_index,
                    }),
                    wide_versions,
                    wide_count,
                );

                if dep.uses_default_features() {
                    let p = WideDefaultFeature {
                        package_name: dep_package_name,
                        parent_key: self.key,
                        parent_true_version: true_version,
                        parent_dep_index: dep_index,
                    };
                    add_dep_package(wide_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                };

                for &feature_name in dep.features() {
                    let p = WideFeature {
                        package_name: dep_package_name,
                        feature_name,
                        parent_key: self.key,
                        parent_true_version: true_version,
                        parent_dep_index: dep_index,
                    };
                    add_dep_package(wide_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                }

                add_dep_package(wide_pkg, vs, dep_map, arena);
            } else {
                let true_versions = compatible_summaries_iter
                    .chain([first])
                    .map(|(i, _)| i as u64);

                let version_count = dep_crate.versions().len() as u64;
                let key = first.1.package_id().activation_key();

                let (dep_pkg, vs) = PackageVersionWrapper::new_dep(
                    PackageKind::from(Pkg { key, root: false }),
                    true_versions,
                    version_count,
                );

                if dep.uses_default_features() {
                    let p = PkgDefaultFeature { key };
                    add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                };

                for &feature_name in dep.features() {
                    let p = PkgFeature { key, feature_name };
                    add_dep_package(dep_pkg.replace_pkg(p.into()), vs, dep_map, arena);
                }

                add_dep_package(dep_pkg, vs, dep_map, arena);
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let index_versions = index.versions(self.key.name()).ok_or(ResolveError)?;

        add_singleton_dep(
            PkgDep {
                key: self.key,
                dep_name: self.dep_name,
            },
            true_version,
            index_versions.len() as u64 + 1,
            dep_map,
            arena,
        );

        // Special value to disable dependency, used for weak features
        if true_version as usize == index_versions.len() {
            return Ok(Ok(()));
        }

        let summary = index_versions
            .get_index(true_version as usize)
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

            let mut compatible_summaries_iter = dep_crate
                .versions()
                .values()
                .enumerate()
                .filter(|&(_, s)| {
                    dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
                })
                .filter(|&(_, s)| s.features().contains_key(&self.dep_feature_name));

            let Some(first) = compatible_summaries_iter.next() else {
                return Ok(Err("no compatible version for dependency".into()));
            };

            let version_req = match dep.version_req() {
                OptVersionReq::Any => &VersionReq::STAR,
                OptVersionReq::Req(version_req) => version_req,
                OptVersionReq::Locked(_, version_req) => version_req,
                OptVersionReq::Precise(_, version_req) => version_req,
            };

            let mut compatible_activation_keys =
                FxIndexSet::from_iter([first.1.package_id().activation_key()]);

            if dep_crate.activation_keys().len() > 1
                && !matches!(
                    version_req.comparators.as_slice(),
                    [Comparator {
                        op: Op::Exact | Op::Caret,
                        ..
                    }]
                )
            {
                compatible_activation_keys.extend(
                    compatible_summaries_iter
                        .clone()
                        .map(|(_, s)| s.package_id().activation_key()),
                );
            }

            if compatible_activation_keys.len() > 1 {
                let wide_versions = compatible_activation_keys
                    .iter()
                    .flat_map(|key| dep_crate.activation_keys().get_index_of(key))
                    .map(|i| i as u64);

                let wide_count = dep_crate.activation_keys().len() as u64;

                add_dep(
                    WideFeature {
                        package_name: dep_package_name,
                        feature_name: self.dep_feature_name,
                        parent_key: self.key,
                        parent_true_version: true_version,
                        parent_dep_index: dep_index,
                    },
                    wide_versions,
                    wide_count,
                    dep_map,
                    arena,
                );
            } else {
                let true_versions = compatible_summaries_iter
                    .chain([first])
                    .map(|(i, _)| i as u64);

                let version_count = dep_crate.versions().len() as u64;
                let key = first.1.package_id().activation_key();

                add_dep(
                    PkgFeature {
                        key,
                        feature_name: self.dep_feature_name,
                    },
                    true_versions,
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
    pub parent_true_version: u64,
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let key = *index
            .activation_keys(self.package_name)
            .and_then(|map| map.get_index(true_version as usize))
            .ok_or(ResolveError)?;

        let version_count = index.versions(self.package_name).ok_or(ResolveError)?.len();

        let parent_summary = index
            .versions(self.parent_key.name())
            .and_then(|map| map.get_index(self.parent_true_version as usize))
            .map(|(_, s)| s.clone())
            .ok_or(ResolveError)?;

        let dep = parent_summary
            .dependencies()
            .get(self.parent_dep_index)
            .ok_or(ResolveError)?;

        let true_versions = index
            .versions(dep.package_name())
            .ok_or(ResolveError)?
            .values()
            .enumerate()
            .filter(|&(_, s)| {
                dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
            })
            .filter(|&(_, s)| s.package_id().activation_key() == key)
            .map(|(i, _)| i as u64);

        add_dep(
            Pkg { key, root: false },
            true_versions,
            version_count as u64,
            dep_map,
            arena,
        );

        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WideFeature {
    pub package_name: InternedString,
    pub feature_name: InternedString,
    pub parent_key: ActivationKey,
    pub parent_true_version: u64,
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let index_activation_keys = index
            .activation_keys(self.package_name)
            .ok_or(ResolveError)?;

        add_singleton_dep(
            Wide {
                package_name: self.package_name,
                parent_key: self.parent_key,
                parent_true_version: self.parent_true_version,
                parent_dep_index: self.parent_dep_index,
            },
            true_version,
            index_activation_keys.len() as u64,
            dep_map,
            arena,
        );

        let key = *index_activation_keys
            .get_index(true_version as usize)
            .ok_or(ResolveError)?;

        let version_count = index.versions(self.package_name).ok_or(ResolveError)?.len();

        let parent_summary = index
            .versions(self.parent_key.name())
            .and_then(|map| map.get_index(self.parent_true_version as usize))
            .map(|(_, s)| s.clone())
            .ok_or(ResolveError)?;

        let dep = parent_summary
            .dependencies()
            .get(self.parent_dep_index)
            .ok_or(ResolveError)?;

        let true_versions = index
            .versions(dep.package_name())
            .ok_or(ResolveError)?
            .values()
            .enumerate()
            .filter(|&(_, s)| {
                dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
            })
            .filter(|&(_, s)| s.features().contains_key(&self.feature_name))
            .filter(|&(_, s)| s.package_id().activation_key() == key)
            .map(|(i, _)| i as u64);

        add_dep(
            PkgFeature {
                key,
                feature_name: self.feature_name,
            },
            true_versions,
            version_count as u64,
            dep_map,
            arena,
        );

        Ok(Ok(()))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WideDefaultFeature {
    pub package_name: InternedString,
    pub parent_key: ActivationKey,
    pub parent_true_version: u64,
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        let index_activation_keys = index
            .activation_keys(self.package_name)
            .ok_or(ResolveError)?;

        add_singleton_dep(
            Wide {
                package_name: self.package_name,
                parent_key: self.parent_key,
                parent_true_version: self.parent_true_version,
                parent_dep_index: self.parent_dep_index,
            },
            true_version,
            index_activation_keys.len() as u64,
            dep_map,
            arena,
        );

        let key = *index_activation_keys
            .get_index(true_version as usize)
            .ok_or(ResolveError)?;

        let version_count = index.versions(self.package_name).ok_or(ResolveError)?.len();

        let parent_summary = index
            .versions(self.parent_key.name())
            .and_then(|map| map.get_index(self.parent_true_version as usize))
            .map(|(_, s)| s.clone())
            .ok_or(ResolveError)?;

        let dep = parent_summary
            .dependencies()
            .get(self.parent_dep_index)
            .ok_or(ResolveError)?;

        let true_versions = index
            .versions(dep.package_name())
            .ok_or(ResolveError)?
            .values()
            .enumerate()
            .filter(|&(_, s)| {
                dep.matches(s) && dep.features().iter().all(|f| s.features().contains_key(f))
            })
            .filter(|&(_, s)| s.package_id().activation_key() == key)
            .map(|(i, _)| i as u64);

        add_dep(
            PkgDefaultFeature { key },
            true_versions,
            version_count as u64,
            dep_map,
            arena,
        );

        Ok(Ok(()))
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
        _: &mut Map<Package, VersionSet>,
        _: &mut Index,
        _: &mut Arena,
        _: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
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
        true_version: u64,
        dep_map: &mut Map<Package, VersionSet>,
        index: &mut Index,
        arena: &mut Arena,
        next_links_true_version: &mut u64,
    ) -> Result<Result<(), Rc<str>>, ResolveError> {
        macro_rules! add_dependencies {
            ($($name:ident),* $(,)?) => {
                match self {
                    $(
                        PackageKind::$name(p) => {
                            p.add_dependencies(
                                true_version,
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

pub struct Provider<'a> {
    index: &'a mut Index,
    arena: FxIndexSet<PackageVersionWrapper<PackageKind>>,
    next_links_true_version: u64,
}

impl<'a> Provider<'a> {
    pub fn new(index: &'a mut Index) -> Self {
        Self {
            index,
            arena: FxIndexSet::default(),
            next_links_true_version: 0,
        }
    }

    pub fn insert_root(
        &mut self,
        root_name: InternedString,
        root_version: &semver::Version,
        root_source_id: SourceId,
    ) -> Option<(PackageVersionWrapper<PackageKind>, Version)> {
        let root_versions = self.index.versions(root_name)?;

        let true_version = root_versions
            .keys()
            .enumerate()
            .find(|&(_, v)| v == root_version)
            .map(|(i, _)| i as u64)?;

        let (root_pkg, root_version) = PackageVersionWrapper::new_pkg(
            PackageKind::from(Pkg::new_root(
                (root_name, root_source_id, root_version.into()).into(),
            )),
            true_version,
            root_versions.len() as u64,
        );

        self.arena.insert(root_pkg.clone());

        Some((root_pkg, root_version))
    }

    pub fn index(&self) -> &Index {
        self.index
    }
}

impl<'a> DependencyProvider for Provider<'a> {
    type P = PackageVersionWrapper<PackageKind>;
    type M = Rc<str>;
    type Priority = Reverse<u64>;
    type Err = ResolveError;

    fn prioritize(&mut self, _: Package, range: VersionSet) -> Self::Priority {
        Reverse(range.count() as u64)
    }

    fn choose_version(
        &mut self,
        _: Package,
        range: VersionSet,
    ) -> Result<Option<Version>, Self::Err> {
        Ok(range.last())
    }

    fn get_dependencies(
        &mut self,
        package: Package,
        version: Version,
    ) -> Result<pubgrub::Dependencies<Self::M>, Self::Err> {
        let mut dep_map = Map::default();

        let wrapper = self.package_to_name(package).ok_or(ResolveError)?;
        let inner = wrapper.inner(version).map(|(p, v)| (p.clone(), v));

        if let Some((d, vs)) = wrapper.dependency(version) {
            let p = Package(self.arena.insert_full(d).0 as u32);
            dep_map.insert(p, vs);
        }

        if let Some((pkg, true_version)) = inner {
            if let Err(error) = pkg.add_dependencies(
                true_version,
                &mut dep_map,
                self.index,
                &mut self.arena,
                &mut self.next_links_true_version,
            )? {
                return Ok(Dependencies::Unavailable(error));
            }
        }

        Ok(Dependencies::Available(dep_map))
    }

    fn package_to_name(&self, package: Package) -> Option<&Self::P> {
        self.arena.get_index(package.0 as usize)
    }

    fn name_to_package(&self, package_name: &Self::P) -> Option<Package> {
        Some(Package(self.arena.get_index_of(package_name)? as u32))
    }

    fn package_version_repr(&self, package: Package, version: Version) -> impl Display + '_ {
        PackageVersionDisplay {
            provider: self,
            package,
            version,
        }
    }

    fn package_version_set_repr(
        &self,
        package: Package,
        version_set: VersionSet,
    ) -> impl Display + '_ {
        PackageVersionSetDisplay {
            provider: self,
            package,
            version_set,
        }
    }
}
