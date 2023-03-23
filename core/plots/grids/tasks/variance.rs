use crate::*;
use std::time::Instant;
use yodel::inference::*;

pub struct QueryRet(Vec<f64>);

pub struct VarianceDataPoint {
    step: usize,
    stats: WmcStats,
    expectations: Expectations,
    duration: Duration,
}
fn l1_distance(x0: &[f64], x1: &[f64]) -> f64 {
    izip!(x0, x1).fold(0.0, |dist, (l, r)| dist + (l - r).abs())
}
fn runner(
    gridsize: usize,
    comptype: CompileType,
    determinism: Det,
    runs: usize,
    runchecks: usize,
    seed: Option<u64>,
    l1: bool,
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
    let key = SummaryKey {
        comptype,
        gridsize,
        determinism,
    };
    info!("{:?}", key);
    let expected = if l1 { Some(exact(&prg)) } else { None };
    match comptype {
        Exact => panic!("exact compile type not supported for 'variance' task"),
        OptApx => panic!("optimized approx on hold"),
        Approx => {
            for res in SamplingIter::new(runs, &prg, &opts) {
                if (res.step > runs - 10) || (res.step < 10) {
                    println!("{}: {:?}", res.step, res.stats);
                    let query = res.expectations.query();
                    println!("{}: {:?} {:?}", res.step, res.weight, res.duration);
                    match &expected {
                        None => println!("{}: {:?}", res.step, query),
                        Some(q) => {
                            println!("{}: {:?} @ {}", res.step, query, l1_distance(&q, &query));
                            println!("{}: {:?} ", res.step, res.expectations.var());
                        }
                    }
                }
            }
        }
    }

    todo!()
    // (key, todo!(), todo!(), todo!(), todo!())
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
        runs = 1000;
        runchecks = 4;
    } else {
        runs = args.runs;
        runchecks = args.runchecks;
        assert!(runchecks < runs);
        let ratio = (runs as f64 / runchecks as f64) as usize;
        assert!(ratio < 4);
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
        runs,
        runchecks,
        None,
        true,
    );
    for (w, r) in izip!(ws, result) {
        // let _ = write_csv_row(csvpath, &r);
    }
}
