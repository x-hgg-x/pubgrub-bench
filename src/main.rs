mod index;
mod provider;
mod utils;

use std::str::FromStr;

use anyhow::{bail, Context};
use cargo::core::SourceId;
use cargo::util::interning::InternedString;
use cargo::GlobalContext;
use crates_index::{git, GitIndex};
use pubgrub::{DefaultStringReporter, DependencyProvider, PubGrubError, Reporter};
use semver::Version;
use tikv_jemallocator::Jemalloc;
use tracing_subscriber::fmt::time::Uptime;
use tracing_subscriber::EnvFilter;

use crate::index::Index;
use crate::provider::Provider;

#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

fn main() -> anyhow::Result<()> {
    let env = EnvFilter::from_env("CARGO_LOG");

    tracing_subscriber::fmt()
        .with_timer(Uptime::default())
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .with_env_filter(env)
        .init();

    let source_id = SourceId::crates_io(&GlobalContext::default()?)?;
    let root_source_id = SourceId::for_registry(&"https://example.com".parse()?)?;

    let mut index = Index::new(source_id, GitIndex::with_path("crates.io-index", git::URL)?);
    let mut provider = Provider::new(&mut index);

    let root_name = InternedString::from("solana-archiver-lib");
    let root_version = Version::from_str("1.1.12")?;

    let (root_pkg, root_version) = provider
        .insert_root(root_name, &root_version, root_source_id)
        .context("unknown package")?;

    match pubgrub::resolve(&mut provider, &root_pkg, root_version) {
        Ok(solution) => {
            for (p, v) in solution {
                let pv = provider.package_version_repr(provider.name_to_package(&p).unwrap(), v);
                println!("{pv}");
            }
            Ok(())
        }
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!(
                "{}",
                DefaultStringReporter::report(&derivation_tree, &provider)
            );
            std::process::exit(1);
        }
        Err(err) => bail!("{err:?}"),
    }
}
