use crate::csv_rw::*;
use crate::*;
use std::time::Instant;
use yodel::inference::*;

fn runner(
    gridsize: usize,
    comptype: CompileType,
    ix: u64,
    determinism: f64,
    seed: Option<u64>,
) -> Row {
    debug!("begin running");
    use CompileType::*;
    let synthesize_seed = 5;
    let prg = generate::program(
        gridsize,
        comptype.use_sampled(),
        synthesize_seed,
        determinism,
    );
    let start = Instant::now();
    let opts = yodel::Options::new(seed, false, false, 0);
    let stats = match comptype {
        Exact => exact_with(&prg).1,
        Approx => importance_weighting_h(1, &prg, &opts).1,
        OptApx => importance_weighting_h(1, &prg, &yodel::Options { opt: true, ..opts }).1,
    };
    let stop = Instant::now();
    let duration = stop.duration_since(start);
    let row = Row {
        gridsize,
        comptype,
        duration,
        determinism,
        ix,
        seed,
        acceptsize: stats.accept,
        distsize: stats.dist,
        numsize: stats.dist_accept,
        calls: stats.mgr_recursive_calls,
    };
    info!("computed: {:?}", row);
    row
}

pub fn run_all_grids(path: &str) -> Vec<Row> {
    debug!("begin running grid");
    use CompileType::*;
    let _ = write_csv_header(path);

    let mut all_answers = vec![];
    for determinism in [0.75, 0.25, 0.5, 0.0_f64] {
        for comptype in [Approx, Exact] {
            for gridsize in [3, 6, 9, 12, 15_u64] {
                for ix in 1..=10_u64 {
                    let row = runner(
                        gridsize as usize,
                        comptype,
                        ix,
                        determinism,
                        Some(gridsize * 100 + ix),
                    );
                    let _ = write_csv_row(path, &row);
                    all_answers.push(row);
                }
            }
        }
    }
    all_answers
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
            let rows = crate::csv_rw::read_csv(pstr).unwrap();
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
    #[arg(long, short, default_value_t = 10)]
    pub runs: u64,
}

pub fn main(path: String, args: RunArgs) {
    let csv = args.csv.unwrap_or_else(|| String::from("grids.csv"));
    let seed: u64 = args.seed.unwrap_or_else(|| (args.gridsize as u64) * 100);
    info!("gridsize   : {:?}x{:?}", args.gridsize, args.gridsize);
    info!("comptype   : {:?}", args.comptype);
    info!("determinism: {:?}", args.determinism);
    info!("runs       : {:?}", args.runs);
    info!("start seed : {:?}", seed);
    info!("csv        : {:?}", csv);
    let _ = fs::create_dir_all(path.clone());
    let csvpath = &(path + &csv);
    let _ = write_csv_header(csvpath);
    for ix in 0..args.runs {
        let row = runner(
            args.gridsize,
            args.comptype,
            ix,
            args.determinism.to_f64(),
            Some(seed + ix),
        );
        let _ = write_csv_row(csvpath, &row);
    }
}
