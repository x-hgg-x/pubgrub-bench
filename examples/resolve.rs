use std::str::FromStr;

use cargo::core::SourceId;
use cargo::util::interning::InternedString;
use cargo::GlobalContext;
use crates_index::{git, GitIndex};
use pubgrub_bench::index::{GitRepo, OnDemandIndex};
use semver::Version;
use tikv_jemallocator::Jemalloc;
use tracing_subscriber::fmt::time::Uptime;
use tracing_subscriber::EnvFilter;

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

    let git_path = format!("{}/crates.io-index", env!("CARGO_MANIFEST_DIR"));
    let git_repo = GitRepo::new(source_id, GitIndex::with_path(git_path, git::URL)?);
    let index = OnDemandIndex::new(git_repo);

    let root_name = InternedString::from("solana-archiver-lib");
    let root_version = Version::from_str("1.1.12")?;

    let resolved = pubgrub_bench::resolve(&index, root_source_id, root_name, root_version)?;

    let mut pkgs = resolved.pkgs.into_iter().collect::<Vec<_>>();
    pkgs.sort_unstable_by_key(|&(pkg_id, _)| pkg_id);

    for (pkg_id, feature_names) in &pkgs {
        println!("{pkg_id}");
        for &feature_name in feature_names {
            println!("    + {feature_name}");
        }
    }

    Ok(())
}
