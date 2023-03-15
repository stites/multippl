#![allow(unused_imports)]

use csv::{Reader, Writer};
use itertools::*;
// use plotters::coord::types::*;
// use plotters::coord::*;
// use plotters::prelude::*;
// use plotters::style::text_anchor::{HPos, Pos, VPos};
use clap::Parser;
use rayon::prelude::*;
use rsdd::sample::probability::Probability;
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
use yodel::typecheck::grammar::*;

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
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize)]
enum CompileType {
    Exact,
    Approx,
    OptApprox,
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
            Ok(OptApprox)
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
        duration: Duration,
        determinism: f64,
        seed: u64,
        ix: u64,
    }
}

impl Row {
    fn csv_array(&self) -> [String; 6] {
        let c1 = format!("{}", self.gridsize);
        let c2 = format!("{:?}", self.comptype);
        let c3 = format!("{}", self.duration.as_millis());
        let c4 = format!("{:.2}", self.determinism);
        let c5 = format!("{}", self.seed);
        let c6 = format!("{}", self.ix);
        [c1, c2, c3, c4, c5, c6]
    }
}

fn define_program(size: usize, sampled: bool, prg_seed: u64, determinism: f64) -> ProgramTyped {
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
    let mut wtr = Writer::from_writer(file);
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
    let mut wtr = Writer::from_writer(file);
    wtr.write_record(&row.csv_array())?;
    wtr.flush()?;
    Ok(())
}
fn runner(gridsize: usize, comptype: CompileType, ix: u64, determinism: f64) -> (Row, WmcStats) {
    debug!("begin running");
    use CompileType::*;
    let seed = 5;
    let prg = define_program(gridsize, comptype.use_sampled(), seed, determinism);
    let start = Instant::now();
    let stats = match comptype {
        Exact => exact_with(&prg).1,
        Approx => importance_weighting_h(1, &prg, &Default::default()).1,
        OptApprox => {
            let opts = yodel::Options {
                opt: true,
                ..Default::default()
            };
            importance_weighting_h(1, &prg, &opts).1
        }
    };
    let stop = Instant::now();
    let duration = stop.duration_since(start);
    let row = Row {
        gridsize: gridsize,
        comptype: comptype,
        duration,
        determinism: determinism,
        ix: ix,
        seed: seed,
    };
    info!("computed: {:?} {:?}", row, stats);
    (row, stats)
}
fn run_all_grids(path: &str) -> Vec<(Row, WmcStats)> {
    debug!("begin running");
    use CompileType::*;
    let _ = write_csv_header(path);

    let specs: Vec<_> = iproduct!(
        // [4_usize],
        // [2, 3, 4, 5, 7, 9, 12, 15, 20, 25_usize],
        [5, 7, 9_usize],
        // [Exact, OptApprox],
        [Exact, Approx, OptApprox],
        (1..=1_u64)
    )
    .collect_vec();

    let mut all_answers = vec![];
    for determinism in [0.5, 0.25, 0.0_f64] {
        // for determinism in [0.0_f64] {
        let some_answers: Vec<_> = specs
            .clone()
            .into_iter()
            .map(|(gridsize, comptype, ix)| {
                let (row, stats) = runner(gridsize, comptype, ix, determinism);
                let _ = write_csv_row(path, &row);
                (row, stats)
            })
            .collect();
        all_answers.extend(some_answers);
    }
    all_answers
}

// fn read_csv(path: &str) -> MyResult<Vec<Row>> {
//     let file = fs::OpenOptions::new().open(path).unwrap();
//     let mut rdr = Reader::from_reader(file);
//     let mut rows = vec![];
//     for result in rdr.deserialize() {
//         // The iterator yields MyResult<StringRecord, Error>, so we check the
//         // error here.
//         let record: Row = result?;
//         println!("{:?}", record);
//         rows.push(record);
//     }
//     Ok(rows)
// }
// struct DataRow {
//     gridsize: usize,
//     exact: Duration,
//     approx: Duration,
//     approx_opt: Duration,
//     determinism: f64,
//     seed: u64,
//     ix: u64,
// }
// struct SummaryRow {
//     gridsize: usize,
//     exact: Duration,
//     approx: Duration,
//     approx_opt: Duration,
//     determinism: f64,
// }
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
    #[arg(long)]
    gridsize: usize,
    #[arg(long)]
    comptype: CompileType,
    #[arg(long, short)]
    determinism: f64,
    #[arg(short, default_value_t = 0)]
    verbosity: usize,
    #[arg(short, default_value = None)]
    csv: Option<String>,
    #[arg(short, default_value = None)]
    path: Option<String>,
}

fn main() -> MyResult<()> {
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
    let csv = args.csv.unwrap_or_else(|| String::from("grids.csv"));
    let path = args.path.unwrap_or_else(|| String::from("out/plots/"));
    info!("gridsize   : {:?}x{:?}", args.gridsize, args.gridsize);
    info!("comptype   : {:?}", args.comptype);
    info!("determinism: {:?}", args.determinism);
    info!("path       : {:?}", path);
    info!("csv        : {:?}", csv);
    info!("verbosity  : {:?}", verbosity);

    fs::create_dir_all(path)?;
    let _rows = run_all_grids(&(path + &csv));
    // build_chart(rows);
    Ok(())
}
