pub mod index;
pub mod provider;
pub mod utils;

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;

use anyhow::{bail, Context};
use cargo::core::{PackageId, SourceId};
use cargo::util::interning::InternedString;
use provider::PackageKind;
use pubgrub::{DefaultStringReporter, Map, PubGrubError, Reporter, Set};
use semver::Version;

use crate::{
    index::{Index, IndexGuard},
    provider::Provider,
};

#[derive(Debug)]
pub struct Resolved {
    pub pkgs: Map<PackageId, BTreeSet<InternedString>>,
    pub links: Set<InternedString>,
}

pub fn resolve(
    index: &impl Index,
    root_source_id: SourceId,
    root_name: InternedString,
    root_version: Version,
) -> anyhow::Result<Resolved> {
    let mut provider = Provider::new(index);

    let (root_pkg, root_version_index) = provider
        .get_root(root_name, &root_version, root_source_id)
        .context("unknown package")?;

    match pubgrub::resolve(&mut provider, root_pkg, root_version_index) {
        Ok(solution) => {
            let mut pkgs = Map::default();
            let mut links = Set::default();
            let mut index = provider.index().lock();

            for (p, v) in solution {
                match p.inner(v) {
                    Some((PackageKind::Pkg(pkg), idx)) => {
                        let c = index.get(pkg.key.name()).unwrap();
                        let pkg_id = c.versions()[idx as usize].1.package_id();
                        if let Entry::Vacant(entry) = pkgs.entry(pkg_id) {
                            entry.insert(BTreeSet::default());
                        }
                    }
                    Some((PackageKind::PkgFeature(pkg), idx)) => {
                        let c = index.get(pkg.key.name()).unwrap();
                        let pkg_id = c.versions()[idx as usize].1.package_id();
                        pkgs.entry(pkg_id).or_default().insert(pkg.feature_name);
                    }
                    Some((PackageKind::Links(pkg), _)) => {
                        assert!(links.insert(pkg.name));
                    }
                    _ => (),
                };
            }

            Ok(Resolved { pkgs, links })
        }
        Err(PubGrubError::NoSolution(error)) => {
            bail!("{}", DefaultStringReporter::report(&error, &provider))
        }
        Err(err) => bail!("{err:?}"),
    }
}
