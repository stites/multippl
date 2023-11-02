extern crate tracing;
extern crate yodel;

use clap::Parser;
use clap_verbosity_flag::LevelFilter as VerbLevel;
use clap_verbosity_flag::Verbosity;
use serde_json::Value;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::time::*;
use tracing::*;
use yodel::pipeline::{Datum, DataPoints};
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
    // #[clap(short, long, value_parser)]
    // opt: bool,         // use optimizations

    // #[clap(short, long, value_parser)]
    // opt: bool,         // use optimizations

    // #[clap(short, long, value_parser)]
    // stats_window: u64, // use optimizations

    // string delimited list of variables to append query that are not in the return
    #[clap(short, long, value_parser)]
    post: Option<String>,

    // placeholder
    #[clap(short, long, value_parser)]
    data: Option<String>,
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

fn read_data(ofp: Option<String>) -> DataPoints {
    ofp.and_then(|fp| {
        let datafile = fs::read_to_string(fp).ok()?;
        let parsed: Value = serde_json::from_str(&datafile).ok()?;
        parsed
            .as_object()?
            .into_iter()
            .map(|(k, v)| {
                let v = serde_json::from_value::<Vec<Value>>(v.clone())
                    .ok()?
                    .into_iter()
                    .map(|v| match &v {
                        Value::Bool(x) => Some(Datum::Bool(*x)),
                        Value::Number(x) => Some(Datum::Float(x.as_f64()?)),
                        _ => None,
                    })
                    .collect::<Option<_>>()?;
                Some((k.clone(), v))
            })
            .collect::<Option<_>>()
    })
    .unwrap_or_else(|| Default::default())
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let strstep = args
        .steps
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
    info!("         /**\\           The Yodel compiler         ");
    info!("        /****\\   /\\                                ");
    info!("       /      \\ /**\\                               ");
    info!("      /  /\\    /    \\        /\\    /\\  /\\      /\\  ");
    info!("     /  /  \\  /      \\      /  \\/\\/  \\/  \\  /\\/  \\ ");
    info!("    /  /    \\/ /\\     \\    /    \\ \\  /    \\/ /   / ");
    info!("   /  /      \\/  \\/\\   \\  /      \\    /   /    \\   ");
    info!("__/__/_______/___/__\\___\\______________(art by jgs)");
    info!("---------------------------------------------------");
    info!("       File: {}", args.file);
    info!("    # Steps: {}", strstep);
    info!("       Seed: {:?}", args.rng);
    info!("Debug level: {:?}", lvl);
    info!(" report BDD: {}", args.stats);
    info!("!!   +query: {:?}", args.post); // FIXME: unused
    info!("!!data file: {:?}", args.data);
    setup_tracing(lvl);

    let pth = PathBuf::from(args.file);
    debug!("loading file: {:?}", fs::canonicalize(&pth));

    let src = fs::read_to_string(pth)?;
    debug!("program:\n{}\n", src);

    let options = yodel::pipeline::Options::new(args.rng, false, false, false, 0);
    let datapoints = read_data(args.data); // FIXME unused

    debug!("compilation options: {:?}", options);
    let now = Instant::now();
    let (query, stats) = yodel::inference::importance_weighting_h_h(args.steps, &src, &options, datapoints);
    let elapsed_time = now.elapsed();
    if args.stats {
        println!("{:?}", stats);
    }
    if format!("{:?}", query).len() < 80 {
        println!("   Computed: {:?}", query);
    } else {
        println!("Computed:\n{:?}", query);
    }

    // if elapsed_time.as_secs() > 10 {
    //     println!("Took {}s", elapsed_time.as_secs());
    // } else {
    println!("Took {}ms", elapsed_time.as_millis());
    // }
    Ok(())
}
