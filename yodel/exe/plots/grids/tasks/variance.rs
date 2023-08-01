use crate::*;
use csv::{ReaderBuilder, WriterBuilder};
use itertools::*;
use std::time::Instant;
use yodel::grids::generate;
use yodel::inference::*;

pub struct QueryRet(Vec<f64>);

#[derive(Debug, Clone, serde::Deserialize)]
pub struct DataPoint {
    pub key: SummaryKey,
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
    pub fn csv_array(&self) -> [String; 5] {
        let c0 = format!("{}", self.key.gridsize);
        let c1 = format!("{:?}", self.key.comptype);
        let c2 = format!("{:?}", self.key.determinism);
        let c3 = format!("{}", self.step);
        let c4 = format!("{:.8}", self.l1);
        [c0, c1, c2, c3, c4]
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
fn runner_h(
    prg: ProgramInferable,
    gridsize: usize,
    comptype: CompileType,
    determinism: Det,
    runs: usize,
    _runchecks: usize,
    seed: Option<u64>,
    l1: bool,
    csvpath: &str,
) {
    // ) -> (
    //     SummaryKey,
    //     SummaryData,
    //     Expectations,
    //     Vec<Importance>,
    //     Vec<QueryRet>,
    // ) {
    debug!("begin running");
    use CompileType::*;

    let opts = yodel::Options::new(seed, false, false, 0);
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
                let (query, _weight) = (res.expectations.query(), res.weight.clone());
                // ws.push(weight);
                let l1 = l1_distance(&exp, &query);
                // l1s.push(l1);
                if (res.step > runs - 10) || (res.step < 10) {
                    // println!("{}: {:?} @ {}", res.step, query, l1);
                    println!("{}: {:.8}", res.step, l1);
                }
                let d = DataPoint {
                    key,
                    step: res.step,
                    l1,
                };
                let _ = write_csv_row(csvpath, &d);
                // qs.push(query);
            }
            println!("------------------------------");
            // println!("final ess: {:?}", ess(&ws));
            println!("------------------------------");
        }
    }
}
fn runner(
    sliding_obs: usize,
    gridsize: usize,
    comptype: CompileType,
    determinism: Det,
    runs: usize,
    _runchecks: usize,
    seed: Option<u64>,
    l1: bool,
    csvpath: (&str, bool),
) {
    let synthesize_seed = 5;
    let prg = generate::program(
        gridsize,
        comptype.use_sampled(),
        synthesize_seed,
        determinism.to_f64(),
    );
    if sliding_obs == 0 {
        let csv = csvpath.0.to_owned() + ".csv";
        if fs::metadata(&csv).is_ok() && !csvpath.1 {
            error!("csv file {} exists! Refusing to run.", csv);
            std::process::exit(0x0001);
        }
        info!("...outputting to csv {}", csv);
        let _ = write_csv_header(&csv);
        runner_h(
            prg,
            gridsize,
            comptype,
            determinism,
            runs,
            _runchecks,
            seed,
            l1,
            &csv,
        )
    } else {
        for (obsid, p) in generate::sliding_observes(&prg, sliding_obs).iter() {
            let csv = csvpath.0.to_owned() + &format!("obs{}.csv", obsid.to_string());
            if fs::metadata(&csv).is_ok() && !csvpath.1 {
                error!("csv file {} exists! Refusing to run.", csv);
                std::process::exit(0x0001);
            }
            info!("...outputting to csv {}", csv);
            let _ = write_csv_header(&csv);
            runner_h(
                p.clone(),
                gridsize,
                comptype,
                determinism,
                runs,
                _runchecks,
                seed,
                l1,
                &csv,
            )
        }
    }
}

#[derive(Parser, Debug, Clone)]
pub struct StatArgs {
    #[arg(long, short)]
    pub gridsize: usize,
    #[arg(long, short)]
    pub comptype: CompileType,
    #[arg(long, short)]
    pub determinism: Det,
    #[arg(long, short, default_value_t = 10000)]
    pub steps: usize,
    #[arg(long, short, default_value_t = 100)]
    pub stepcheck: usize,
    #[arg(long, short, default_value_t = 10)]
    pub runs: u64,
}

pub fn read_csv(path: &str) -> MyResult<Vec<DataPoint>> {
    let file = fs::OpenOptions::new().read(true).open(path).unwrap();
    let mut rdr = ReaderBuilder::new()
        .delimiter(b'\t')
        .has_headers(true)
        .from_reader(file);
    let mut rows = vec![];
    for result in rdr.deserialize() {
        // The iterator yields MyResult<StringRecord, Error>, so we check the
        // error here.
        let record: DataPoint = result?;
        rows.push(record);
    }
    Ok(rows)
}

pub fn stats(path: String, args: StatArgs) {
    // println!("found #{}", paths.len());
    // println!("found {:?}", paths);
    let key = SummaryKey {
        comptype: args.comptype,
        gridsize: args.gridsize,
        determinism: args.determinism,
    };
    let mut data: Vec<Vec<DataPoint>> = vec![];
    for ix in 0..args.runs {
        let mut csv = csvname_noext(&args, ix as u64);
        csv += ".csv";
        info!("...reading csv {:?}", csv);
        let s = path.clone() + &csv;
        let p = std::path::Path::new(&s);
        if p.metadata().unwrap().is_file() {
            let cpth = p.canonicalize().unwrap();
            let pth = cpth.as_path();
            let ostr = pth.to_str();
            let pstr = ostr.unwrap();
            println!("Processing... {}", pstr);
            let rows = read_csv(pstr).unwrap();
            data.push(rows);
        }
    }
    let mut avgscsv = csvname_noext(&args, 0);
    avgscsv += "-avgs.csv";
    for step in 0..data[0].len() {
        let mut sum = 0.0;
        for run in 0..args.runs {
            sum += data[run as usize][step].l1;
        }
        let avgpt = DataPoint {
            key,
            step,
            l1: sum / (args.runs as f64),
        };
        let _ = write_csv_row(&avgscsv, &avgpt);
    }
}

pub trait VArgs {
    fn gridsize(&self) -> usize;
    fn determinism(&self) -> Det;
    fn steps(&self) -> usize;
    fn stepcheck(&self) -> usize;
}
pub fn csvname_noext(args: &impl VArgs, ix: u64) -> String {
    let mut csvname = String::from("");
    csvname += &format!("grid{:?}x{:?}-", args.gridsize(), args.gridsize());
    csvname += "approx-"; // must be approx
    csvname += &format!("d{:?}-", args.determinism());
    csvname += &format!("n{:?}-", args.steps());
    csvname += &format!("c{:?}-", args.stepcheck());
    let startseed: u64 = (args.gridsize() as u64) * 100;
    let seed = startseed + ix;
    csvname += &format!("s{:?}", seed);
    csvname
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
    #[arg(long, short, default_value_t = 1)]
    pub runs: usize,
    #[arg(long, short, default_value_t = 10000)]
    pub steps: usize,
    #[arg(long, short, default_value_t = 100)]
    pub stepcheck: usize,
    #[arg(long, short, default_value_t = true)]
    pub l1: bool,
    #[arg(long, short, default_value_t = false)]
    pub overwrite_csv: bool,
    #[arg(long, short, default_value_t = false)]
    pub debug: bool,
    #[arg(long, short, default_value_t = 0)]
    pub sliding_obs: usize,
}

impl VArgs for StatArgs {
    fn gridsize(&self) -> usize {
        self.gridsize
    }
    fn determinism(&self) -> Det {
        self.determinism
    }
    fn steps(&self) -> usize {
        self.steps
    }
    fn stepcheck(&self) -> usize {
        self.stepcheck
    }
}
impl VArgs for RunArgs {
    fn gridsize(&self) -> usize {
        self.gridsize
    }
    fn determinism(&self) -> Det {
        self.determinism
    }
    fn steps(&self) -> usize {
        self.steps
    }
    fn stepcheck(&self) -> usize {
        self.stepcheck
    }
}
pub fn main(path: String, args: RunArgs) {
    let steps;
    let stepcheck;
    if args.debug {
        info!(",<><><><><><><><><>.");
        info!("| debug    : true  |");
        info!("`<><><><><><><><><>'");
        steps = 10;
        stepcheck = 3;
    } else {
        steps = args.steps;
        stepcheck = args.stepcheck;
        // assert!(stepcheck < steps);
        // let ratio = (steps as f64 / stepcheck as f64) as usize;
        // assert!(ratio < 4);
    }
    info!("checking l1  : {:?}", args.l1);
    info!("overwrite csv: {:?}", args.overwrite_csv);
    info!("gridsize     : {:?}x{:?}", args.gridsize, args.gridsize);
    info!("comptype     : {:?}", args.comptype);
    info!("determinism  : {:?}", args.determinism);
    info!("steps        : {:?}", steps);
    info!("step check   : {:?}", stepcheck);
    info!("# runs       : {:?}", args.runs);
    let startseed: u64 = args.seed.unwrap_or_else(|| (args.gridsize as u64) * 100);
    info!("start seed   : {:?}", startseed);
    let _ = fs::create_dir_all(path.clone());

    for ix in 0..args.runs {
        let seed = startseed + (ix as u64);
        let csv_noext: String = args
            .csv
            .clone()
            .unwrap_or_else(|| csvname_noext(&args, ix as u64));
        let csvpath_noext = &(path.clone() + &csv_noext);
        // let (key, data, expectations, ws, result) = runner(
        runner(
            args.sliding_obs,
            args.gridsize,
            args.comptype,
            args.determinism,
            steps,
            stepcheck,
            Some(seed),
            args.l1,
            (csvpath_noext, args.overwrite_csv),
        );
    }
}
