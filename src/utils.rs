use std::fmt::{self, Debug, Display};
use std::rc::Rc;

use pubgrub::{helpers::PackageVersionWrapper, VersionIndex, VersionSet};

use crate::{
    index::{Index, IndexGuard},
    provider::{
        PackageKind, Pkg, PkgDefaultFeature, PkgDep, PkgDepFeature, PkgFeature, Provider, Wide,
        WideDefaultFeature, WideFeature,
    },
};

struct DisplayAsDebug<T>(T);

impl<T: Display> Debug for DisplayAsDebug<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

pub struct PackageVersionDisplay<'a, I: Index> {
    pub provider: &'a Provider<'a, I>,
    pub pkg: &'a PackageVersionWrapper<PackageKind>,
    pub version_index: VersionIndex,
}

impl<I: Index> Display for PackageVersionDisplay<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pkg.inner(self.version_index) {
            None => write!(f, "{} @ {}", self.pkg, self.version_index.get()),
            Some((inner, true_version_index)) => {
                let mut index = self.provider.index().lock();
                let true_version_index = true_version_index as usize;
                match *inner {
                    PackageKind::Pkg(Pkg { key, .. })
                    | PackageKind::PkgFeature(PkgFeature { key, .. })
                    | PackageKind::PkgDefaultFeature(PkgDefaultFeature { key, .. }) => {
                        match index
                            .get(key.name())
                            .into_iter()
                            .flat_map(|c| c.versions().iter().map(|(v, _)| v))
                            .nth(true_version_index)
                        {
                            Some(v) => write!(f, "{inner} @ {v}"),
                            None => write!(f, "<unknown>"),
                        }
                    }
                    PackageKind::PkgDep(PkgDep { key, .. })
                    | PackageKind::PkgDepFeature(PkgDepFeature { key, .. }) => {
                        let Some(index_versions) = index.get(key.name()).map(|c| c.versions())
                        else {
                            return write!(f, "<unknown>");
                        };

                        if true_version_index == index_versions.len() {
                            write!(f, "{inner} @ off")
                        } else if let Some((v, _)) = index_versions.get(true_version_index) {
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
                        match index
                            .get(package_name)
                            .into_iter()
                            .flat_map(|c| c.activation_keys())
                            .nth(true_version_index)
                            .map(|key| key.semver_compatibility())
                        {
                            Some(v) => write!(f, "{inner} @ {v}"),
                            None => write!(f, "<unknown>"),
                        }
                    }
                    PackageKind::Links(_) => write!(f, "{inner} @ {true_version_index}"),
                }
            }
        }
    }
}

pub struct PackageVersionSetDisplay<'a, I: Index> {
    pub provider: &'a Provider<'a, I>,
    pub pkg: &'a PackageVersionWrapper<PackageKind>,
    pub version_set: VersionSet,
}

impl<I: Index> Display for PackageVersionSetDisplay<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pkg.inner_pkg() {
            Some(pkg) => write!(f, "{pkg} @ ")?,
            None => write!(f, "{} @ ", self.pkg)?,
        }

        let mut list = f.debug_list();
        for v in self.version_set.iter() {
            match self.pkg.inner(v) {
                Some((inner, true_version_index)) => {
                    let mut index = self.provider.index().lock();
                    let true_version_index = true_version_index as usize;
                    match *inner {
                        PackageKind::Pkg(Pkg { key, .. })
                        | PackageKind::PkgFeature(PkgFeature { key, .. })
                        | PackageKind::PkgDefaultFeature(PkgDefaultFeature { key, .. }) => {
                            match index
                                .get(key.name())
                                .into_iter()
                                .flat_map(|c| c.versions().iter().map(|(v, _)| v))
                                .nth(true_version_index)
                            {
                                Some(v) => list.entry(&DisplayAsDebug(v)),
                                None => list.entry(&"<unknown>"),
                            }
                        }
                        PackageKind::PkgDep(PkgDep { key, .. })
                        | PackageKind::PkgDepFeature(PkgDepFeature { key, .. }) => {
                            let Some(index_versions) = index.get(key.name()).map(|c| c.versions())
                            else {
                                return write!(f, "<unknown>");
                            };

                            if true_version_index == index_versions.len() {
                                list.entry(&"off")
                            } else if let Some((v, _)) = index_versions.get(true_version_index) {
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
                            match index
                                .get(package_name)
                                .into_iter()
                                .flat_map(|c| c.activation_keys())
                                .nth(true_version_index)
                                .map(|key| key.semver_compatibility())
                            {
                                Some(v) => list.entry(&DisplayAsDebug(v)),
                                None => list.entry(&"<unknown>"),
                            }
                        }
                        PackageKind::Links(_) => list.entry(&true_version_index),
                    }
                }
                None => list.entry(&v.get()),
            };
        }
        list.finish()?;

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RcString {
    Static(&'static str),
    Rc(Rc<String>),
}

impl From<&'static str> for RcString {
    fn from(s: &'static str) -> Self {
        Self::Static(s)
    }
}

impl From<String> for RcString {
    fn from(s: String) -> Self {
        Self::Rc(s.into())
    }
}

impl Display for RcString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_ref(), f)
    }
}

impl AsRef<str> for RcString {
    fn as_ref(&self) -> &str {
        match self {
            Self::Static(s) => s,
            Self::Rc(s) => s,
        }
    }
}
