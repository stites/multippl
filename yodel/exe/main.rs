extern crate tracing;
extern crate yodel;

use clap::Parser;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use tracing::*;
use tracing_subscriber::fmt;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long, value_parser)]
    file: String,

    #[clap(short, long, value_parser)]
    steps: usize,

    #[clap(short, long, value_parser)]
    rng: Option<u64>,

    #[clap(long, value_parser)]
    debug: bool,

    #[clap(long)]
    stats: bool,
    // #[clap(short, long, value_parser)]
    // opt: bool,         // use optimizations

    // #[clap(short, long, value_parser)]
    // opt: bool,         // use optimizations

    // #[clap(short, long, value_parser)]
    // stats_window: u64, // use optimizations
}

fn setup_tracing(lvl: Level) {
    let format = fmt::format()
        .with_level(true)
        .with_target(false)
        .with_thread_ids(false)
        .with_thread_names(false)
        .without_time()
        .compact();

    tracing_subscriber::fmt()
        .with_max_level(lvl)
        .event_format(format)
        .init();
}
fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    setup_tracing(if args.debug {
        Level::TRACE
    } else {
        Level::WARN
    });
    info!("cli arguments: {:?}", args);

    let pth = PathBuf::from(args.file);
    debug!("loading file: {:?}", fs::canonicalize(&pth));

    let src = fs::read_to_string(pth)?;
    debug!("program:\n{}\n", src);
    let options = yodel::pipeline::Options::new(args.rng, false, false, false, 0);

    debug!("compilation options: {:?}", options);
    let (query, stats) = yodel::inference::importance_weighting_h(args.steps, &src, &options);
    if args.stats {
        println!("{:?}", stats);
    }
    println!("{:?}", query);
    Ok(())
}
