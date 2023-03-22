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
        Approx => importance_weighting_h(runs, &prg, &opts),
        OptApx => importance_weighting_h(runs, &prg, &yodel::Options { opt: true, ..opts }),
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
    let mut data = vec![];
    for path in paths {
        let p = path.unwrap();
        if p.metadata().unwrap().is_file() {
            let cpth = p.path().canonicalize().unwrap();
            let pth = cpth.as_path();
            let ostr = pth.to_str();
            let pstr = ostr.unwrap();
            println!("Processing... {}", pstr);
            let rows = read_csv(pstr).unwrap();
            data.extend(rows.clone());
        }
    }
    let mut summary = HashMap::new();
    for d in data {
        let k = SummaryKey::from_data(&d);
        let v = SummaryData::from_data(&d);
        match summary.get(&k) {
            None => summary.insert(k, v),
            Some(v0) => summary.insert(k, v0.plus(&v)),
        };
    }
    let summary: HashMap<SummaryKey, SummaryData> =
        summary.into_iter().map(|(k, v)| (k, v.avg())).collect();

    println!("{}\t{}", SummaryKey::to_header(), SummaryData::to_header(),);
    for g in [3, 6, 9, 12, 15] {
        for det in [0.75, 0.5, 0.25, 0.0_f64] {
            let d = Det::from_f64(det);
            for ctype in [CompileType::Exact, CompileType::Approx] {
                let key = SummaryKey::new(ctype, g, d);
                match summary.get(&key) {
                    None => println!(
                        "{}\t----------------------------------------------------------------------------------------------",
                        key.to_string()
                    ),
                    Some(v) => println!("{}\t{}", key.to_string(), v.to_string(),),
                }
            }
            println!("");
        }
    }
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
}

pub fn main(path: String, args: RunArgs) {
    let csv = args
        .csv
        .clone()
        .unwrap_or_else(|| String::from("grids.csv"));
    let seed: u64 = args.seed.unwrap_or_else(|| (args.gridsize as u64) * 100);
    info!("gridsize   : {:?}x{:?}", args.gridsize, args.gridsize);
    info!("comptype   : {:?}", args.comptype);
    info!("determinism: {:?}", args.determinism);
    info!("runs       : {:?}", args.runs);
    info!("run checks : {:?}", args.runchecks);
    info!("start seed : {:?}", seed);
    info!("path       : {:?}", path);
    info!("csv        : {:?}", csv);
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
