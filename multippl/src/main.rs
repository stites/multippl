extern crate multippl;
extern crate tracing;

use clap::Parser;
use clap_verbosity_flag::LevelFilter as VerbLevel;
use clap_verbosity_flag::Verbosity;
use itertools::*;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::time::*;
use tracing::*;
use tracing_subscriber::fmt;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long, value_parser)]
    file: String,

    #[clap(short, long, value_parser)]
    samples: usize,

    #[clap(short, long, value_parser)]
    rng: Option<u64>,

    // verbosity flags provided by clap-verbosity-flag
    //
    //   -q silences output
    //   -v show warnings
    //   -vv show info
    //   -vvv show debug
    //   -vvvv show trace
    //
    // see https://docs.rs/clap-verbosity-flag/latest/clap_verbosity_flag/ for more
    #[command(flatten)]
    verbose: Verbosity,

    #[clap(long)]
    stats: bool,

    #[clap(short, long)]
    reverse_compilation: bool,
    // #[clap(short, long, value_parser)]
    // opt: bool,         // use optimizations

    // #[clap(short, long, value_parser)]
    // opt: bool,         // use optimizations

    // #[clap(short, long, value_parser)]
    // stats_window: u64, // use optimizations
}

fn verbosity_to_tracing(lvl: VerbLevel) -> Level {
    match lvl {
        VerbLevel::Off => Level::WARN,
        VerbLevel::Error => Level::WARN,
        VerbLevel::Warn => Level::WARN, // this is the default tracing level, you can't get any lower
        VerbLevel::Info => Level::INFO,
        VerbLevel::Debug => Level::DEBUG,
        VerbLevel::Trace => Level::TRACE,
    }
}
fn setup_tracing(lvl: Level) {
    let format = fmt::format()
        .with_level(true)
        .with_target(true)
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

    let strstep = args
        .samples
        .to_string()
        .as_bytes()
        .rchunks(3)
        .rev()
        .map(std::str::from_utf8)
        .collect::<Result<Vec<&str>, _>>()
        .unwrap()
        .join(",");
    let lvl = verbosity_to_tracing(args.verbose.log_level_filter());

    info!("          /\\                                       ");
    info!("         /**\\           The MultiPPL compiler      ");
    info!("        /****\\   /\\                                ");
    info!("       /      \\ /**\\                               ");
    info!("      /  /\\    /    \\        /\\    /\\  /\\      /\\  ");
    info!("     /  /  \\  /      \\      /  \\/\\/  \\/  \\  /\\/  \\ ");
    info!("    /  /    \\/ /\\     \\    /    \\ \\  /    \\/ /   / ");
    info!("   /  /      \\/  \\/\\   \\  /      \\    /   /    \\   ");
    info!("__/__/_______/___/__\\___\\______________(art by jgs)");
    info!("---------------------------------------------------");
    info!("       File: {}", args.file);
    info!("  # Samples: {}", strstep);
    info!("       Seed: {:?}", args.rng);
    info!("Debug level: {:?}", lvl);
    info!(" report BDD: {}", args.stats);
    info!(" rev. compl: {}", args.reverse_compilation);
    setup_tracing(lvl);

    let pth = PathBuf::from(args.file);
    debug!("loading file: {:?}", fs::canonicalize(&pth));

    let src = fs::read_to_string(pth)?;
    debug!("program:\n{}\n", src);

    let options =
        multippl::pipeline::Options::new(args.rng, false, false, args.reverse_compilation, 0);

    debug!("compilation options: {:?}", options);
    let now = Instant::now();
    let (query, stats) = multippl::inference::importance_weighting_h(args.samples, &src, &options);
    let elapsed_time = now.elapsed();
    if args.stats {
        println!("{:?}", stats);
    }

    match query {
        Either::Left(qs) => println!("{}", qs.into_iter().map(|f| f.to_string()).join(" ")),
        Either::Right(qss) => {
            for qs in qss {
                println!("{}", qs.into_iter().map(|f| f.to_string()).join(" "));
            }
        }
    }
    println!("{}ms", elapsed_time.as_millis());
    Ok(())
}
