use crate::*;
use ::csv::{ReaderBuilder, WriterBuilder};
use std::time::Instant;
use yodel::inference::*;

pub struct QueryRet(Vec<f64>);

#[derive(Debug, Clone, serde::Deserialize)]
pub struct DataPoint {
    pub step: usize,
    pub l1: f64,
}
impl DataPoint {
    pub fn to_header() -> String {
        format!("step   \t     l1")
    }
    pub fn to_string(&self) -> String {
        format!("{}   \t{}", self.step, self.l1)
    }
    pub fn csv_array(&self) -> [String; 2] {
        let c1 = format!("{}", self.step);
        let c2 = format!("{:.8}", self.l1);
        [c1, c2]
    }
}
pub fn write_csv_row(path: &str, row: &DataPoint) -> MyResult<()> {
    let file = fs::OpenOptions::new()
        .write(true)
        .append(true)
        .open(path)
        .unwrap();
    let mut wtr = WriterBuilder::new().delimiter(b'\t').from_writer(file);
    wtr.write_record(&row.csv_array())?;
    wtr.flush()?;
    Ok(())
}

fn l1_distance(x0: &[f64], x1: &[f64]) -> f64 {
    izip!(x0, x1).fold(0.0, |dist, (l, r)| dist + (l - r).abs())
}
fn ess(ws: &[Importance]) -> f64 {
    let ss = ws.iter().map(Importance::weight).sum::<f64>();
    println!("{}", ss);
    // 1.0 / ws.iter().map(Importance::weight).sum::<f64>()
    todo!()
}
fn runner(
    gridsize: usize,
    comptype: CompileType,
    determinism: Det,
    runs: usize,
    runchecks: usize,
    seed: Option<u64>,
    l1: bool,
    csvpath: (&str, bool),
) -> (
    SummaryKey,
    SummaryData,
    Expectations,
    Vec<Importance>,
    Vec<QueryRet>,
) {
    if fs::metadata(csvpath.0).is_ok() && !csvpath.1 {
        error!("csv file {} exists! Refusing to run.", csvpath.0);
        std::process::exit(0x0001);
    }
    let csvpath = csvpath.0;
    let _ = write_csv_header(csvpath);

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
    let exp = expected.clone().expect("only l1 for the moment");
    match comptype {
        Exact => panic!("exact compile type not supported for 'variance' task"),
        OptApx => panic!("optimized approx on hold"),
        Approx => {
            // let mut ws = vec![];
            // let mut l1s = vec![];
            // let mut qs = vec![];
            // let mut q_expectation = QueryRet(vec![]);
            for res in SamplingIter::new(runs, &prg, &opts) {
                let (query, weight) = (res.expectations.query(), res.weight.clone());
                // ws.push(weight);
                let l1 = l1_distance(&exp, &query);
                // l1s.push(l1);
                let d = DataPoint { step: res.step, l1 };
                write_csv_row(csvpath, &d);
                if (res.step > runs - 10) || (res.step < 10) {
                    // println!("{}: {:?} @ {}", res.step, query, l1);
                    println!("{}: {:.8}", res.step, l1);
                }
                // qs.push(query);
            }
            println!("------------------------------");
            // println!("final ess: {:?}", ess(&ws));
            println!("------------------------------");
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
    #[arg(long, short, default_value_t = true)]
    pub l1: bool,
    #[arg(long, short, default_value_t = false)]
    pub overwrite_csv: bool,
    #[arg(long, short, default_value_t = false)]
    pub debug: bool,
}

pub fn main(path: String, args: RunArgs) {
    let seed: u64 = args.seed.unwrap_or_else(|| (args.gridsize as u64) * 100);

    let runs;
    let runchecks;
    if args.debug {
        info!(",<><><><><><><><><>.");
        info!("| debug    : true  |");
        info!("`<><><><><><><><><>'");
        runs = 10;
        runchecks = 3;
    } else {
        runs = args.runs;
        runchecks = args.runchecks;
        // assert!(runchecks < runs);
        // let ratio = (runs as f64 / runchecks as f64) as usize;
        // assert!(ratio < 4);
    }
    let mut csvname = String::from("");
    info!("checking l1  : {:?}", args.l1);
    info!("overwrite csv: {:?}", args.overwrite_csv);
    info!("gridsize     : {:?}x{:?}", args.gridsize, args.gridsize);
    csvname += &format!("grid{:?}x{:?}-", args.gridsize, args.gridsize);
    info!("comptype     : {:?}", args.comptype);
    csvname += "approx-"; // must be approx
    info!("determinism  : {:?}", args.determinism);
    csvname += &format!("d{:?}-", args.determinism);
    info!("runs         : {:?}", runs);
    csvname += &format!("n{:?}-", runs);
    info!("run checks   : {:?}", runchecks);
    csvname += &format!("c{:?}-", runchecks);
    info!("start seed   : {:?}", seed);
    csvname += &format!("s{:?}", seed);
    csvname += ".csv";
    let csv = args.csv.clone().unwrap_or_else(|| csvname);
    info!("csv        : {}", csv);
    let _ = fs::create_dir_all(path.clone());
    let csvpath = &(path + &csv);

    let (key, data, expectations, ws, result) = runner(
        args.gridsize,
        args.comptype,
        args.determinism,
        runs,
        runchecks,
        None,
        args.l1,
        (csvpath, args.overwrite_csv),
    );
    for (w, r) in izip!(ws, result) {
        // let _ = write_csv_row(csvpath, &r);
    }
}
