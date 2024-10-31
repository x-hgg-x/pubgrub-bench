use std::fmt::{self, Debug, Display};

use pubgrub::{DependencyProvider, Package, PackageVersionWrapper, Version, VersionSet};

use crate::provider::{
    PackageKind, Pkg, PkgDefaultFeature, PkgDep, PkgDepFeature, PkgFeature, Provider, Wide,
    WideDefaultFeature, WideFeature,
};

struct DisplayAsDebug<T>(T);

impl<T: Display> Debug for DisplayAsDebug<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

pub struct PackageVersionDisplay<'a> {
    pub provider: &'a Provider<'a>,
    pub package: Package,
    pub version: Version,
}

impl Display for PackageVersionDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(package) = self.provider.package_to_name(self.package) else {
            return write!(f, "<unknown>");
        };

        match package.inner(self.version) {
            None => write!(f, "{package} @ {}", self.version.get()),
            Some((inner, true_version)) => {
                let true_version = true_version as usize;
                match *inner {
                    PackageKind::Pkg(Pkg { key, .. })
                    | PackageKind::PkgFeature(PkgFeature { key, .. })
                    | PackageKind::PkgDefaultFeature(PkgDefaultFeature { key, .. }) => {
                        match self
                            .provider
                            .index()
                            .get_ref(key.name())
                            .into_iter()
                            .flat_map(|c| c.versions().keys())
                            .nth(true_version)
                        {
                            Some(v) => write!(f, "{inner} @ {v}"),
                            None => write!(f, "<unknown>"),
                        }
                    }
                    PackageKind::PkgDep(PkgDep { key, .. })
                    | PackageKind::PkgDepFeature(PkgDepFeature { key, .. }) => {
                        let Some(index_versions) = self
                            .provider
                            .index()
                            .get_ref(key.name())
                            .map(|c| c.versions())
                        else {
                            return write!(f, "<unknown>");
                        };

                        if true_version == index_versions.len() {
                            write!(f, "{inner} @ off")
                        } else if let Some((v, _)) = index_versions.get_index(true_version) {
                            write!(f, "{inner} @ {v}")
                        } else {
                            write!(f, "<unknown>")
                        }
                    }
                    PackageKind::Wide(Wide { package_name, .. })
                    | PackageKind::WideFeature(WideFeature { package_name, .. })
                    | PackageKind::WideDefaultFeature(WideDefaultFeature {
                        package_name, ..
                    }) => {
                        match self
                            .provider
                            .index()
                            .get_ref(package_name)
                            .into_iter()
                            .flat_map(|c| c.activation_keys())
                            .nth(true_version)
                            .map(|key| key.semver_compatibility())
                        {
                            Some(v) => write!(f, "{inner} @ {v}"),
                            None => write!(f, "<unknown>"),
                        }
                    }
                    PackageKind::Links(_) => write!(f, "{inner} @ {true_version}"),
                }
            }
        }
    }
}

pub struct PackageVersionSetDisplay<'a> {
    pub provider: &'a Provider<'a>,
    pub package: Package,
    pub version_set: VersionSet,
}

impl Display for PackageVersionSetDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(package) = self.provider.package_to_name(self.package) else {
            return write!(f, "<unknown>");
        };

        match package {
            PackageVersionWrapper::Pkg { pkg, .. } => write!(f, "{pkg} @ ")?,
            _ => write!(f, "{package} @ ")?,
        }

        let mut list = f.debug_list();
        for v in self.version_set.iter() {
            match package.inner(v) {
                Some((inner, true_version)) => {
                    let true_version = true_version as usize;
                    match *inner {
                        PackageKind::Pkg(Pkg { key, .. })
                        | PackageKind::PkgFeature(PkgFeature { key, .. })
                        | PackageKind::PkgDefaultFeature(PkgDefaultFeature { key, .. }) => {
                            match self
                                .provider
                                .index()
                                .get_ref(key.name())
                                .into_iter()
                                .flat_map(|c| c.versions().keys())
                                .nth(true_version)
                            {
                                Some(v) => list.entry(&DisplayAsDebug(v)),
                                None => list.entry(&"<unknown>"),
                            }
                        }
                        PackageKind::PkgDep(PkgDep { key, .. })
                        | PackageKind::PkgDepFeature(PkgDepFeature { key, .. }) => {
                            let Some(index_versions) = self
                                .provider
                                .index()
                                .get_ref(key.name())
                                .map(|c| c.versions())
                            else {
                                return write!(f, "<unknown>");
                            };

                            if true_version == index_versions.len() {
                                list.entry(&"off")
                            } else if let Some((v, _)) = index_versions.get_index(true_version) {
                                list.entry(&DisplayAsDebug(v))
                            } else {
                                list.entry(&"<unknown>")
                            }
                        }
                        PackageKind::Wide(Wide { package_name, .. })
                        | PackageKind::WideFeature(WideFeature { package_name, .. })
                        | PackageKind::WideDefaultFeature(WideDefaultFeature {
                            package_name,
                            ..
                        }) => {
                            match self
                                .provider
                                .index()
                                .get_ref(package_name)
                                .into_iter()
                                .flat_map(|c| c.activation_keys())
                                .nth(true_version)
                                .map(|key| key.semver_compatibility())
                            {
                                Some(v) => list.entry(&DisplayAsDebug(v)),
                                None => list.entry(&"<unknown>"),
                            }
                        }
                        PackageKind::Links(_) => list.entry(&true_version),
                    }
                }
                None => list.entry(&v.get()),
            };
        }
        list.finish()?;

        Ok(())
    }
}
