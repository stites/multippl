#![allow(unused_imports)]
#![allow(dead_code)]

use csv::{ReaderBuilder, WriterBuilder};
use itertools::*;
// use plotters::coord::types::*;
// use plotters::coord::*;
// use plotters::prelude::*;
// use plotters::style::text_anchor::{HPos, Pos, VPos};
use clap::Parser;
use rayon::prelude::*;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::str::FromStr;
use std::time::{Duration, Instant};
use tracing::*;
use tracing_subscriber::FmtSubscriber;
use yodel::compile::grammar::*;
use yodel::grids::*;
use yodel::inference::*;
use yodel::typeinf::grammar::*;

type MyResult<X> = Result<X, Box<dyn Error>>;

macro_rules! zoom_and_enhance {
    (struct $name:ident { $($fname:ident : $ftype:ty,)* }) => {
        #[derive(Debug, Clone, serde::Deserialize)]
        struct $name {
            $($fname : $ftype),*
        }

        impl $name {
            fn header() -> &'static [&'static str] {
                static NAMES: &'static [&'static str] = &[$(stringify!($fname)),*];
                NAMES
            }
        //     fn write_row() -> Vec<String> {
        //         let r = vec![];
        //         $(
        //             if stringify!($fname) == "duration" {
        // r.pushformat!("{:.2}", self.determinism);
        //             } else {
        //               r.push(format!("{}", self.$fname));
        //             }
        // let c2 = format!("{:?}", self.comptype);
        // let c3 = format!("{:.2}", self.determinism);
        // let c4 = format!("{}", self.seed);
        // let c5 = format!("{}", self.ix);
        // let c6 = format!("{}", self.acceptsize);
        // let c7 = format!("{}", self.distsize);
        // let c8 = format!("{}", self.numsize);
        // let c9 = format!("{}", self.calls);
        // let c10 = format!("{}", self.duration.as_millis());
        // [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]
        //             )*
        //         static NAMES: &'static [&'static str] = &[$(stringify!($fname)),*];
        //         NAMES
        //     }
        }
    }
}

#[derive(Debug, Clone, Hash, Copy, serde::Deserialize, Eq, PartialEq)]
enum CompileType {
    Exact,
    Approx,
    OptApx,
}
impl CompileType {
    fn use_sampled(&self) -> bool {
        match self {
            CompileType::Exact => false,
            _ => true,
        }
    }
}
impl FromStr for CompileType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        use CompileType::*;
        let approx = HashSet::from(["approx", "sampling", "sample", "is"]);
        let opt = HashSet::from(["approx-opt", "sampling-opt", "sample-opt", "opt"]);
        if s == "exact" {
            Ok(Exact)
        } else if approx.contains(s) {
            Ok(Approx)
        } else if opt.contains(s) {
            Ok(OptApx)
        } else {
            Err(format!(
                "{} is not a valid string. Choose one of \"approx\" \"exact\" \"opt\"",
                s
            ))
        }
    }
}

zoom_and_enhance! {
    struct Row {
        gridsize: usize,
        comptype: CompileType,
        determinism: f64,
        seed: Option<u64>,
        ix: u64,
        acceptsize: usize,
        distsize: usize,
        numsize: usize,
        calls: usize,
        duration: Duration,
    }
}
#[derive(Debug, Clone, serde::Deserialize)]
struct DumbRow {
    gridsize: usize,
    comptype: CompileType,
    determinism: f64,
    seed: i64,
    ix: u64,
    acceptsize: usize,
    distsize: usize,
    numsize: usize,
    calls: usize,
    duration: usize, // milliseconds
}
impl Row {
    fn csv_array(&self) -> [String; 10] {
        let c1 = format!("{}", self.gridsize);
        let c2 = format!("{:?}", self.comptype);
        let c3 = format!("{:.2}", self.determinism);
        let c4 = format!("{}", self.seed.map(|x| x as i64).unwrap_or_else(|| -1));
        let c5 = format!("{}", self.ix);
        let c6 = format!("{}", self.acceptsize);
        let c7 = format!("{}", self.distsize);
        let c8 = format!("{}", self.numsize);
        let c9 = format!("{}", self.calls);
        let c10 = format!("{}", self.duration.as_micros());
        [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]
    }
}

fn define_program(size: usize, sampled: bool, prg_seed: u64, determinism: f64) -> ProgramInferable {
    let mk_probability = |_ix, _p| Probability::new(0.5);
    let schema = GridSchema::new_from_fn(
        size,
        sampled,
        Some(prg_seed),
        Some(determinism),
        Selection::Random,
        None,
        &mk_probability,
    );
    make::grid(schema)
}

fn write_csv_header(path: &str) -> MyResult<()> {
    let file = fs::OpenOptions::new()
        .write(true)
        .append(true)
        .create_new(!std::path::Path::new(&path).exists())
        .open(path)
        .unwrap();
    let mut wtr = WriterBuilder::new().delimiter(b'\t').from_writer(file);
    wtr.write_record(Row::header())?;
    wtr.flush()?;
    Ok(())
}
fn write_csv_row(path: &str, row: &Row) -> MyResult<()> {
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
fn duration_runner(
    gridsize: usize,
    comptype: CompileType,
    ix: u64,
    determinism: f64,
    seed: Option<u64>,
) -> Row {
    debug!("begin running");
    use CompileType::*;
    let synthesize_seed = 5;
    let prg = define_program(
        gridsize,
        comptype.use_sampled(),
        synthesize_seed,
        determinism,
    );
    let start = Instant::now();
    let opts = yodel::Options {
        seed,
        ..Default::default()
    };
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
// fn variance_runner(
//     gridsize: usize,
//     comptype: CompileType,
//     ix: u64,
//     determinism: f64,
//     seed: Option<u64>,
// ) -> Row {
//     todo!()
//     // debug!("begin running");
//     // use CompileType::*;
//     // if comptype == Exact {
//     //     panic!("exact compile type not supported for 'duration' task")
//     // }
//     // let synthesize_seed = 5;
//     // let prg = define_program(
//     //     gridsize,
//     //     comptype.use_sampled(),
//     //     synthesize_seed,
//     //     determinism,
//     // );
//     // let opts = yodel::Options {
//     //     seed,
//     //     ..Default::default()
//     // };
//     // let stats = match comptype {
//     //     Exact => exact_with(&prg).1,
//     //     Approx => importance_weighting_h(1, &prg, &opts).1,
//     //     OptApx => importance_weighting_h(1, &prg, &yodel::Options { opt: true, ..opts }).1,
//     // };
//     // let stop = Instant::now();
//     // let duration = stop.duration_since(start);
//     // let row = Row {
//     //     gridsize,
//     //     comptype,
//     //     duration,
//     //     determinism,
//     //     ix,
//     //     seed,
//     //     acceptsize: stats.accept,
//     //     distsize: stats.dist,
//     //     numsize: stats.dist_accept,
//     //     calls: stats.mgr_recursive_calls,
//     // };
//     // info!("computed: {:?}", row);
//     // row
// }

fn run_all_grids(path: &str) -> Vec<Row> {
    debug!("begin running grid");
    use CompileType::*;
    let _ = write_csv_header(path);

    let mut all_answers = vec![];
    for determinism in [0.75, 0.25, 0.5, 0.0_f64] {
        for comptype in [Approx, Exact] {
            for gridsize in [3, 6, 9, 12, 15_u64] {
                for ix in 1..=10_u64 {
                    let row = duration_runner(
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

fn read_csv(path: &str) -> MyResult<Vec<DumbRow>> {
    let file = fs::OpenOptions::new().read(true).open(path).unwrap();
    let mut rdr = ReaderBuilder::new()
        .delimiter(b'\t')
        .has_headers(true)
        .from_reader(file);
    let mut rows = vec![];
    for result in rdr.deserialize() {
        // The iterator yields MyResult<StringRecord, Error>, so we check the
        // error here.
        let record: DumbRow = result?;
        rows.push(record);
    }
    Ok(rows)
}
// fn build_chart(rows: Vec<SummaryRow>) -> MyResult<()> {
//     let root = BitMapBackend::new("out/plots/grid.png", (640, 480)).into_drawing_area();
//     root.fill(&WHITE)?;
//     let root = root.margin(10, 10, 10, 10);
//     let mut chart = ChartBuilder::on(&root)
//         .caption("Grid construction", ("sans-serif", 40).into_font())
//         .x_label_area_size(20)
//         .y_label_area_size(40)
//         .build_cartesian_2d(0f32..10f32, 0f32..10f32)?;
//     let anchor_position = (200, 100);
//     let anchor_left_bottom = Pos::new(HPos::Left, VPos::Bottom);
//     let anchor_right_top = Pos::new(HPos::Right, VPos::Top);
//     let text_style_right_top = BLACK.with_anchor::<RGBColor>(anchor_right_top);
//     chart
//         .configure_mesh()
//         .x_labels(5)
//         .x_desc("duration (s)")
//         .y_labels(5)
//         .y_desc("grid size")
//         .axis_desc_style(text_style_right_top)
//         .y_label_formatter(&|x| format!("{:.2}", x))
//         .x_label_formatter(&|x| format!("{:.0}", x))
//         .draw()?;

//     let as_f32 = |dur: Duration| dur.as_secs() as f32;
//     // exact
//     chart.draw_series(LineSeries::new(
//         rows.iter()
//             .map(|r| (r.gridsize as f32, as_f32(r.exact)))
//             .collect_vec(),
//         &BLUE,
//     ))?;

//     chart.draw_series(LineSeries::new(
//         rows.iter()
//             .map(|r| (r.gridsize as f32, as_f32(r.approx)))
//             .collect_vec(),
//         &BLUE,
//     ))?;
//     root.present()?;
//     Ok(())
// }

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct PlotGridsArgs {
    #[arg(short, default_value_t = 0)]
    verbosity: usize,
    #[arg(long, default_value = None)]
    path: Option<String>,
    #[command(subcommand)]
    command: CommandType,
}

#[derive(Parser)]
struct RunArgs {
    #[arg(long, short)]
    gridsize: usize,
    #[arg(long, default_value = None)]
    csv: Option<String>,
    #[arg(long, short)]
    comptype: CompileType,
    #[arg(long, short)]
    determinism: f64,
    #[arg(long, short, default_value = None)]
    seed: Option<u64>,
    #[arg(long, short, default_value_t = 10)]
    runs: u64,
}

#[derive(clap::Subcommand)]
enum CommandType {
    Duration(RunArgs),
    Variance(RunArgs),
    DurationStats,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
enum Det {
    Zero,
    TwentyFivePer,
    FiftyPer,
    SeventyFivePer,
}
impl Det {
    fn from_f64(f: f64) -> Det {
        if f == 0.0 {
            Det::Zero
        } else if f == 0.25 {
            Det::TwentyFivePer
        } else if f == 0.5 {
            Det::FiftyPer
        } else if f == 0.75 {
            Det::SeventyFivePer
        } else {
            panic!("machine error?")
        }
    }
    fn to_f64(&self) -> f64 {
        match self {
            Det::Zero => 0.0,
            Det::TwentyFivePer => 0.25,
            Det::FiftyPer => 0.5,
            Det::SeventyFivePer => 0.75,
        }
    }
}
#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
struct SummaryKey {
    comptype: CompileType,
    gridsize: usize,
    determinism: Det,
}
impl SummaryKey {
    fn new(comptype: CompileType, gridsize: usize, determinism: Det) -> Self {
        SummaryKey {
            gridsize,
            comptype,
            determinism,
        }
    }
    fn to_header() -> String {
        format!("grid\tcomptype\tdet")
    }
    fn to_string(&self) -> String {
        format!(
            "{}x{} \t{:?}   \t{}",
            self.gridsize,
            self.gridsize,
            self.comptype,
            self.determinism.to_f64()
        )
    }
    fn from_data(d: &DumbRow) -> Self {
        SummaryKey {
            gridsize: d.gridsize,
            comptype: d.comptype,
            determinism: Det::from_f64(d.determinism),
        }
    }
}
#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
struct SummaryData {
    duration: usize,
    acceptsize: usize,
    distsize: usize,
    numsize: usize,
    calls: usize,
    nsamples: usize,
}
impl SummaryData {
    fn from_data(d: &DumbRow) -> Self {
        Self {
            acceptsize: d.acceptsize,
            distsize: d.distsize,
            numsize: d.numsize,
            calls: d.calls,
            duration: d.duration,
            nsamples: 1,
        }
    }
    fn plus(&self, o: &Self) -> Self {
        Self {
            acceptsize: self.acceptsize + o.acceptsize,
            distsize: self.distsize + o.distsize,
            numsize: self.numsize + o.numsize,
            calls: self.calls + o.calls,
            duration: self.duration + o.duration,
            nsamples: self.nsamples + o.nsamples,
        }
    }
    fn to_header() -> String {
        format!("acceptsize\tdistsize\tnumsize     \tcalls    \tnsamples\tduration(micro)")
    }
    fn to_string(&self) -> String {
        // calls vary quite a bit, so we want to adjust the size of padding, if possible
        let cstr = format!("{}", self.calls);
        // aiming for ~10 columns
        let cpadding = if cstr.len() > 5 { "" } else { "      " };
        format!(
            "{}         \t{}         \t{}        \t{}{}\t{}       \t{}",
            self.acceptsize,
            self.distsize,
            self.numsize,
            self.calls,
            cpadding,
            self.nsamples,
            self.duration
        )
    }
    fn avg(&self) -> Self {
        let acceptsize = (self.acceptsize as f64 / self.nsamples as f64) as usize;
        let distsize = (self.distsize as f64 / self.nsamples as f64) as usize;
        let numsize = (self.numsize as f64 / self.nsamples as f64) as usize;
        let calls = (self.calls as f64 / self.nsamples as f64) as usize;
        let duration = (self.duration as f64 / self.nsamples as f64) as usize;
        Self {
            acceptsize,
            distsize,
            numsize,
            calls,
            duration,
            nsamples: self.nsamples,
        }
    }
}
fn main() -> MyResult<()> {
    use CommandType::*;
    let args = PlotGridsArgs::parse();
    let verbosity = match args.verbosity {
        0 => None,
        1 => Some(tracing::Level::INFO),
        2 => Some(tracing::Level::DEBUG),
        _ => panic!("stop that!"),
    };
    match verbosity {
        None => {}
        Some(level) => tracing_subscriber::fmt()
            .with_max_level(level)
            .with_target(false)
            .init(),
    };
    let path = args.path.unwrap_or_else(|| String::from("out/plots/"));
    // info section
    match args.command {
        Duration(args) => {
            let csv = args.csv.unwrap_or_else(|| String::from("grids.csv"));
            let seed: u64 = args.seed.unwrap_or_else(|| (args.gridsize as u64) * 100);
            info!("gridsize   : {:?}x{:?}", args.gridsize, args.gridsize);
            info!("comptype   : {:?}", args.comptype);
            info!("determinism: {:?}", args.determinism);
            info!("runs       : {:?}", args.runs);
            info!("start seed : {:?}", seed);
            info!("path       : {:?}", path);
            info!("csv        : {:?}", csv);
            info!("verbosity  : {:?}", verbosity);
            fs::create_dir_all(path.clone())?;
            let csvpath = &(path + &csv);
            let _ = write_csv_header(csvpath);
            for ix in 0..args.runs {
                let row = duration_runner(
                    args.gridsize,
                    args.comptype,
                    ix,
                    args.determinism,
                    Some(seed + ix),
                );
                let _ = write_csv_row(csvpath, &row);
            }
        }
        Variance(args) => {
            let csv = args.csv.unwrap_or_else(|| String::from("grids.csv"));
            let seed: u64 = args.seed.unwrap_or_else(|| (args.gridsize as u64) * 100);
            info!("gridsize   : {:?}x{:?}", args.gridsize, args.gridsize);
            info!("comptype   : {:?}", args.comptype);
            info!("determinism: {:?}", args.determinism);
            info!("runs       : {:?}", args.runs);
            info!("start seed : {:?}", seed);
            info!("path       : {:?}", path);
            info!("csv        : {:?}", csv);
            info!("verbosity  : {:?}", verbosity);
            todo!()
        }
        DurationStats => {
            let paths = fs::read_dir(path).unwrap();
            let mut data = vec![];
            for path in paths {
                let p = path.unwrap();
                if p.metadata().unwrap().is_file() {
                    let cpth = p.path().canonicalize()?;
                    let pth = cpth.as_path();
                    let ostr = pth.to_str();
                    let pstr = ostr.unwrap();
                    println!("Processing... {}", pstr);
                    let rows = read_csv(pstr)?;
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
    }
    // build_chart(rows);
    Ok(())
}
