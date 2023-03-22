use crate::*;
use std::time::Instant;
use yodel::inference::*;

pub struct QueryRet(Vec<f64>);

fn runner(
    gridsize: usize,
    comptype: CompileType,
    determinism: Det,
    runs: usize,
    runchecks: usize,
    seed: Option<u64>,
) -> (
    SummaryKey,
    SummaryData,
    Expectations,
    Vec<Importance>,
    Vec<QueryRet>,
) {
    debug!("begin running");
    use CompileType::*;
    let synthesize_seed = 5;
    let prg = generate::program(
        gridsize,
        comptype.use_sampled(),
        synthesize_seed,
        determinism.to_f64(),
    );
    let opts = yodel::Options {
        seed,
        ..Default::default()
    };
    let start = Instant::now();
    let (qs, stats) = match comptype {
        Exact => panic!("exact compile type not supported for 'variance' task"),
        Approx => importance_weighting_h_h(runs, &prg, &opts),
        OptApx => {
            panic!("optimized approximation is on hold until it actually does useful optimizations")
        }
    };
    let stop = Instant::now();
    let duration = stop.duration_since(start);
    let key = SummaryKey {
        comptype,
        gridsize,
        determinism,
    };

    let data = SummaryData {
        duration: duration_to_usize(&duration),
        acceptsize: stats.accept,
        distsize: stats.dist,
        numsize: stats.dist_accept,
        calls: stats.mgr_recursive_calls,
        nsamples: runs,
    };
    info!("{:?} {:?}", key, data);
    (key, data, todo!(), todo!(), todo!())
}

pub fn stats(path: String) {
    let paths = fs::read_dir(path).unwrap();
    todo!()
}

#[derive(Parser, Debug, Clone)]
pub struct RunArgs {
    #[arg(long, short)]
    pub gridsize: usize,
    #[arg(long, default_value = None)]
    pub csv: Option<String>,
    #[arg(long, short)]
    pub comptype: CompileType,
    #[arg(long, short)]
    pub determinism: Det,
    #[arg(long, short, default_value = None)]
    pub seed: Option<u64>,
    #[arg(long, short, default_value_t = 10000)]
    pub runs: usize,
    #[arg(long, short, default_value_t = 100)]
    pub runchecks: usize,
    #[arg(long, short, default_value_t = false)]
    pub debug: bool,
}

pub fn main(path: String, args: RunArgs) {
    let csv = args
        .csv
        .clone()
        .unwrap_or_else(|| String::from("grids.csv"));
    let seed: u64 = args.seed.unwrap_or_else(|| (args.gridsize as u64) * 100);

    let runs;
    let runchecks;
    if args.debug {
        info!(",<><><><><><><><><>.");
        info!("| debug    : true  |");
        info!("`<><><><><><><><><>'");
        runs = 4;
        runchecks = 2;
    } else {
        runs = args.runs;
        runchecks = args.runchecks;
    }
    info!("gridsize   : {:?}x{:?}", args.gridsize, args.gridsize);
    info!("comptype   : {:?}", args.comptype);
    info!("determinism: {:?}", args.determinism);
    info!("runs       : {:?}", runs);
    info!("run checks : {:?}", runchecks);
    info!("start seed : {:?}", seed);
    info!("csv        : {}", csv);
    let _ = fs::create_dir_all(path.clone());
    let csvpath = &(path + &csv);
    let _ = write_csv_header(csvpath);
    let (key, data, expectations, ws, result) = runner(
        args.gridsize,
        args.comptype,
        args.determinism,
        args.runs,
        args.runchecks,
        Some(seed),
    );
    for (w, r) in izip!(ws, result) {
        // let _ = write_csv_row(csvpath, &r);
    }
}
