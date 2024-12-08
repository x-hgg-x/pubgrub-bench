use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

use cargo::core::SourceId;
use cargo::util::interning::InternedString;
use cargo::GlobalContext;
use crates_index::{git, GitIndex};
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use pubgrub_bench::index::{GitRepo, PreloadedIndex};
use rayon::prelude::*;
use semver::Version;
use serde::Serialize;
use tikv_jemallocator::Jemalloc;
use tracing_subscriber::fmt::time::Uptime;
use tracing_subscriber::EnvFilter;

#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Serialize)]
pub struct OutputSummary<'a> {
    pub name: InternedString,
    pub version: &'a Version,
    pub time: f32,
    pub succeeded: bool,
}

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

    let start = Instant::now();

    let index = PreloadedIndex::from_git(git_repo);

    let end = Instant::now();
    println!("Index loaded in {:?}", end - start);

    let template = "Pubgrub: [Time: {elapsed_precise}, Rate: {per_sec}, Remaining: {eta}] {wide_bar} {pos:>6}/{len:6}: {percent:>3}%";

    let len = index
        .registry()
        .values()
        .map(|c| c.versions().len())
        .sum::<usize>() as u64;

    let progress_bar = ProgressBar::new(len)
        .with_style(ProgressStyle::with_template(template)?)
        .with_finish(ProgressFinish::AndLeave);

    let progress = AtomicU64::new(0);

    let mut output_summaries = Vec::new();

    std::thread::scope(|s| {
        s.spawn(|| loop {
            let progress = progress.load(Ordering::Relaxed);
            if progress == u64::MAX {
                progress_bar.set_position(len);
                break;
            }
            progress_bar.set_position(progress);
            std::thread::sleep(Duration::from_millis(100));
        });

        output_summaries.par_extend(
            index
                .registry()
                .par_iter()
                .flat_map(|(&name, c)| c.versions().par_iter().map(move |(v, _)| (name, v)))
                .map(|(name, version)| {
                    let start = Instant::now();
                    let res = pubgrub_bench::resolve(&index, root_source_id, name, version.clone());
                    let end = Instant::now();
                    progress.fetch_add(1, Ordering::Relaxed);

                    OutputSummary {
                        name,
                        version,
                        time: (end - start).as_secs_f32(),
                        succeeded: res.is_ok(),
                    }
                }),
        );

        progress.store(u64::MAX, Ordering::Relaxed);
    });

    let elapsed = progress_bar.elapsed();
    drop(progress_bar);

    let cpu_time = output_summaries
        .iter()
        .map(|row| Duration::from_secs_f32(row.time))
        .sum::<Duration>();

    println!("Wall time: {elapsed:?}");
    println!("CPU time: {cpu_time:?}");

    output_summaries.sort_unstable_by_key(|s| (s.name, s.version));

    let csv_dir = format!("{}/out", env!("CARGO_MANIFEST_DIR"));
    std::fs::create_dir_all(&csv_dir)?;
    let csv_path = format!("{csv_dir}/summary.csv");
    let mut out_file = csv::Writer::from_path(csv_path)?;
    for row in output_summaries {
        out_file.serialize(row)?;
    }

    Ok(())
}
