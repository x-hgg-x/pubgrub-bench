//! > This crate is maintained by the Cargo team, primarily for use by Cargo
//! > and not intended for external use (except as a transitive dependency). This
//! > crate may make major changes to its APIs or be deprecated without warning.

#![allow(clippy::print_stderr)]

pub mod helpers;
pub mod sat;

use std::cmp::{max, min};
use std::collections::BTreeMap;
use std::fmt;

use cargo::{
    core::{dependency::DepKind, Dependency, PackageId, Summary},
    util::{CargoResult, GlobalContext},
};
use proptest::{
    collection::{btree_map, vec},
    prelude::*,
    sample::Index,
    string::string_regex,
};
use pubgrub_bench::index::{PreloadedIndex, VersionOrdering};
use pubgrub_bench::Resolved;

use crate::resolve::{
    helpers::{dep_req, dep_req_kind, pkg_dep, pkg_id, ToPkgId},
    sat::SatResolver,
};

pub fn resolve(deps: Vec<Dependency>, registry: Vec<Summary>) -> CargoResult<Vec<PackageId>> {
    Ok(
        resolve_with_global_context(deps, registry, &GlobalContext::default().unwrap())?
            .pkgs
            .into_keys()
            .collect(),
    )
}

pub fn resolve_and_validated(
    deps: Vec<Dependency>,
    registry: Vec<Summary>,
    sat_resolver: &mut SatResolver,
) -> CargoResult<Resolved> {
    resolve_and_validated_raw(deps, registry, pkg_id("root"), sat_resolver)
}

// Verify that the resolution of cargo resolver can pass the verification of SAT
pub fn resolve_and_validated_raw(
    deps: Vec<Dependency>,
    registry: Vec<Summary>,
    root_pkg_id: PackageId,
    sat_resolver: &mut SatResolver,
) -> CargoResult<Resolved> {
    let resolved = resolve_with_global_context_raw(
        deps.clone(),
        registry,
        root_pkg_id,
        &GlobalContext::default().unwrap(),
    );

    match resolved {
        Err(e) => {
            if sat_resolver.sat_resolve(&deps) {
                panic!(
                    "`resolve()` returned an error but the sat resolver thinks this will work:\n{}",
                    sat_resolver.used_packages().unwrap()
                );
            }
            Err(e)
        }
        Ok(resolved) => {
            if !sat_resolver.sat_is_valid_solution(&resolved.pkgs) {
                panic!(
                    "`resolve()` thinks this will work, but the solution is \
                     invalid according to the sat resolver:\n{resolved:?}",
                );
            }
            Ok(resolved)
        }
    }
}

pub fn resolve_with_global_context(
    deps: Vec<Dependency>,
    registry: Vec<Summary>,
    gctx: &GlobalContext,
) -> CargoResult<Resolved> {
    resolve_with_global_context_raw(deps, registry, pkg_id("root"), gctx)
}

pub fn resolve_with_global_context_raw(
    deps: Vec<Dependency>,
    mut registry: Vec<Summary>,
    root_pkg_id: PackageId,
    gctx: &GlobalContext,
) -> CargoResult<Resolved> {
    let root = Summary::new(root_pkg_id, deps, &BTreeMap::new(), None::<&String>, None)?;
    registry.push(root);

    let version_ordering = if gctx.cli_unstable().minimal_versions {
        VersionOrdering::MinimumVersionsFirst
    } else {
        VersionOrdering::MaximumVersionsFirst
    };

    pubgrub_bench::resolve(
        &PreloadedIndex::from_registry(registry, version_ordering),
        root_pkg_id.source_id(),
        root_pkg_id.name(),
        root_pkg_id.version().clone(),
    )
}

/// By default `Summary` and `Dependency` have a very verbose `Debug` representation.
/// This replaces with a representation that uses constructors from this file.
///
/// If `registry_strategy` is improved to modify more fields
/// then this needs to update to display the corresponding constructor.
pub struct PrettyPrintRegistry(pub Vec<Summary>);

impl fmt::Debug for PrettyPrintRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec![")?;
        for s in &self.0 {
            if s.dependencies().is_empty() {
                write!(f, "pkg!((\"{}\", \"{}\")),", s.name(), s.version())?;
            } else {
                write!(f, "pkg!((\"{}\", \"{}\") => [", s.name(), s.version())?;
                for d in s.dependencies() {
                    if d.kind() == DepKind::Normal
                        && &d.version_req().to_string() == "*"
                        && !d.is_public()
                    {
                        write!(f, "dep(\"{}\"),", d.name_in_toml())?;
                    } else if d.kind() == DepKind::Normal && !d.is_public() {
                        write!(
                            f,
                            "dep_req(\"{}\", \"{}\"),",
                            d.name_in_toml(),
                            d.version_req()
                        )?;
                    } else {
                        write!(
                            f,
                            "dep_req_kind(\"{}\", \"{}\", {}, {}),",
                            d.name_in_toml(),
                            d.version_req(),
                            match d.kind() {
                                DepKind::Development => "DepKind::Development",
                                DepKind::Build => "DepKind::Build",
                                DepKind::Normal => "DepKind::Normal",
                            },
                            d.is_public()
                        )?;
                    }
                }
                write!(f, "]),")?;
            }
        }
        write!(f, "]")
    }
}

/// This generates a random registry index.
/// Unlike `vec((Name, Ver, vec((Name, VerRq), ..), ..)`,
/// this strategy has a high probability of having valid dependencies.
pub fn registry_strategy(
    max_crates: usize,
    max_versions: usize,
    shrinkage: usize,
) -> impl Strategy<Value = PrettyPrintRegistry> {
    let name = string_regex("[A-Za-z][A-Za-z0-9_-]*(-sys)?").unwrap();

    let raw_version = ..max_versions.pow(3);
    let version_from_raw = move |r: usize| {
        let major = ((r / max_versions) / max_versions) % max_versions;
        let minor = (r / max_versions) % max_versions;
        let patch = r % max_versions;
        format!("{}.{}.{}", major, minor, patch)
    };

    // If this is false then the crate will depend on the nonexistent "bad"
    // instead of the complex set we generated for it.
    let allow_deps = prop::bool::weighted(0.99);

    let list_of_versions =
        btree_map(raw_version, allow_deps, 1..=max_versions).prop_map(move |ver| {
            ver.into_iter()
                .map(|a| (version_from_raw(a.0), a.1))
                .collect::<Vec<_>>()
        });

    let list_of_crates_with_versions =
        btree_map(name, list_of_versions, 1..=max_crates).prop_map(|mut vers| {
            // root is the name of the thing being compiled
            // so it would be confusing to have it in the index
            vers.remove("root");
            // bad is a name reserved for a dep that won't work
            vers.remove("bad");
            vers
        });

    // each version of each crate can depend on each crate smaller than it.
    // In theory shrinkage should be 2, but in practice we get better trees with a larger value.
    let max_deps = max_versions * (max_crates * (max_crates - 1)) / shrinkage;

    let raw_version_range = (any::<Index>(), any::<Index>());
    let raw_dependency = (any::<Index>(), any::<Index>(), raw_version_range, 0..=1);

    fn order_index(a: Index, b: Index, size: usize) -> (usize, usize) {
        let (a, b) = (a.index(size), b.index(size));
        (min(a, b), max(a, b))
    }

    let list_of_raw_dependency = vec(raw_dependency, ..=max_deps);

    // By default a package depends only on other packages that have a smaller name,
    // this helps make sure that all things in the resulting index are DAGs.
    // If this is true then the DAG is maintained with grater instead.
    let reverse_alphabetical = any::<bool>().no_shrink();

    (
        list_of_crates_with_versions,
        list_of_raw_dependency,
        reverse_alphabetical,
    )
        .prop_map(
            |(crate_vers_by_name, raw_dependencies, reverse_alphabetical)| {
                let list_of_pkgid: Vec<_> = crate_vers_by_name
                    .iter()
                    .flat_map(|(name, vers)| vers.iter().map(move |x| ((name.as_str(), &x.0), x.1)))
                    .collect();
                let len_all_pkgid = list_of_pkgid.len();
                let mut dependency_by_pkgid = vec![vec![]; len_all_pkgid];
                for (a, b, (c, d), k) in raw_dependencies {
                    let (a, b) = order_index(a, b, len_all_pkgid);
                    let (a, b) = if reverse_alphabetical { (b, a) } else { (a, b) };
                    let ((dep_name, _), _) = list_of_pkgid[a];
                    if (list_of_pkgid[b].0).0 == dep_name {
                        continue;
                    }
                    let s = &crate_vers_by_name[dep_name];
                    let s_last_index = s.len() - 1;
                    let (c, d) = order_index(c, d, s.len());

                    dependency_by_pkgid[b].push(dep_req_kind(
                        dep_name,
                        &if c == 0 && d == s_last_index {
                            "*".to_string()
                        } else if c == 0 {
                            format!("<={}", s[d].0)
                        } else if d == s_last_index {
                            format!(">={}", s[c].0)
                        } else if c == d {
                            format!("={}", s[c].0)
                        } else {
                            format!(">={}, <={}", s[c].0, s[d].0)
                        },
                        match k {
                            0 => DepKind::Normal,
                            1 => DepKind::Build,
                            // => DepKind::Development, // Development has no impact so don't gen
                            _ => panic!("bad index for DepKind"),
                        },
                    ))
                }

                let mut out: Vec<Summary> = list_of_pkgid
                    .into_iter()
                    .zip(dependency_by_pkgid)
                    .map(|(((name, ver), allow_deps), deps)| {
                        pkg_dep(
                            (name, ver).to_pkgid(),
                            if !allow_deps {
                                vec![dep_req("bad", "*")]
                            } else {
                                let mut deps = deps;
                                deps.sort_by_key(|d| d.name_in_toml());
                                deps.dedup_by_key(|d| d.name_in_toml());
                                deps
                            },
                        )
                    })
                    .collect();

                if reverse_alphabetical {
                    // make sure the complicated cases are at the end
                    out.reverse();
                }

                PrettyPrintRegistry(out)
            },
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolve::helpers::{pkg, registry};

    #[test]
    fn meta_test_deep_pretty_print_registry() {
        assert_eq!(
        &format!(
            "{:?}",
            PrettyPrintRegistry(vec![
                pkg!(("foo", "1.0.1") => [dep_req("bar", "1")]),
                pkg!(("foo", "1.0.0") => [dep_req("bar", "2")]),
                pkg!(("foo", "2.0.0") => [dep_req("bar", "*")]),
                pkg!(("bar", "1.0.0") => [dep_req("baz", "=1.0.2"),
                                          dep_req("other", "1")]),
                pkg!(("bar", "2.0.0") => [dep_req("baz", "=1.0.1")]),
                pkg!(("baz", "1.0.2") => [dep_req("other", "2")]),
                pkg!(("baz", "1.0.1")),
                pkg!(("cat", "1.0.2") => [dep_req_kind("other", "2", DepKind::Build)]),
                pkg!(("cat", "1.0.3") => [dep_req_kind("other", "2", DepKind::Development)]),
                pkg!(("dep_req", "1.0.0")),
                pkg!(("dep_req", "2.0.0")),
            ])
        ),
        "vec![pkg!((\"foo\", \"1.0.1\") => [dep_req(\"bar\", \"^1\"),]),\
         pkg!((\"foo\", \"1.0.0\") => [dep_req(\"bar\", \"^2\"),]),\
         pkg!((\"foo\", \"2.0.0\") => [dep(\"bar\"),]),\
         pkg!((\"bar\", \"1.0.0\") => [dep_req(\"baz\", \"=1.0.2\"),dep_req(\"other\", \"^1\"),]),\
         pkg!((\"bar\", \"2.0.0\") => [dep_req(\"baz\", \"=1.0.1\"),]),\
         pkg!((\"baz\", \"1.0.2\") => [dep_req(\"other\", \"^2\"),]),\
         pkg!((\"baz\", \"1.0.1\")),\
         pkg!((\"cat\", \"1.0.2\") => [dep_req_kind(\"other\", \"^2\", DepKind::Build, false),]),\
         pkg!((\"cat\", \"1.0.3\") => [dep_req_kind(\"other\", \"^2\", DepKind::Development, false),]),\
         pkg!((\"dep_req\", \"1.0.0\")),\
         pkg!((\"dep_req\", \"2.0.0\")),]"
    )
    }

    /// This test is to test the generator to ensure
    /// that it makes registries with large dependency trees
    #[test]
    fn meta_test_deep_trees_from_strategy() {
        use proptest::strategy::ValueTree;
        use proptest::test_runner::TestRunner;

        let mut dis = [0; 21];

        let strategy = registry_strategy(50, 20, 60);
        let mut test_runner = TestRunner::deterministic();
        for _ in 0..128 {
            let PrettyPrintRegistry(input) = strategy
                .new_tree(&mut TestRunner::new_with_rng(
                    Default::default(),
                    test_runner.new_rng(),
                ))
                .unwrap()
                .current();
            let reg = registry(input.clone());
            for this in input.iter().rev().take(10) {
                let res = resolve(
                    vec![dep_req(&this.name(), &format!("={}", this.version()))],
                    reg.clone(),
                );
                dis[res
                    .as_ref()
                    .map(|x| min(x.len(), dis.len()) - 1)
                    .unwrap_or(0)] += 1;
                if dis.iter().all(|&x| x > 0) {
                    return;
                }
            }
        }

        panic!(
            "In 1280 tries we did not see a wide enough distribution \
             of dependency trees! dis: {dis:?}"
        );
    }

    /// This test is to test the generator to ensure
    /// that it makes registries that include multiple versions of the same library
    #[test]
    fn meta_test_multiple_versions_strategy() {
        use proptest::strategy::ValueTree;
        use proptest::test_runner::TestRunner;

        let mut dis = [0; 10];

        let strategy = registry_strategy(50, 20, 60);
        let mut test_runner = TestRunner::deterministic();
        for _ in 0..128 {
            let PrettyPrintRegistry(input) = strategy
                .new_tree(&mut TestRunner::new_with_rng(
                    Default::default(),
                    test_runner.new_rng(),
                ))
                .unwrap()
                .current();
            let reg = registry(input.clone());
            for this in input.iter().rev().take(10) {
                let res = resolve(
                    vec![dep_req(&this.name(), &format!("={}", this.version()))],
                    reg.clone(),
                );
                if let Ok(mut res) = res {
                    let res_len = res.len();
                    res.sort_by_key(|s| s.name());
                    res.dedup_by_key(|s| s.name());
                    dis[min(res_len - res.len(), dis.len() - 1)] += 1;
                }
                if dis.iter().all(|&x| x > 0) {
                    return;
                }
            }
        }
        panic!(
            "In 1280 tries we did not see a wide enough distribution \
             of multiple versions of the same library! dis: {dis:?}"
        );
    }
}
