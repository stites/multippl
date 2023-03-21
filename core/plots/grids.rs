#![allow(unused_imports)]
#![allow(dead_code)]

use csv::{Reader, WriterBuilder};
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

#[derive(Debug, Clone, Copy, serde::Deserialize)]
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

fn run_all_grids(path: &str) -> Vec<Row> {
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
    #[arg(long, short)]
    gridsize: usize,
    #[arg(long, short)]
    comptype: CompileType,
    #[arg(long, short)]
    determinism: f64,
    #[arg(long, short, default_value = None)]
    seed: Option<u64>,
    #[arg(long, short, default_value_t = 10)]
    runs: u64,
    #[arg(short, default_value_t = 0)]
    verbosity: usize,
    #[arg(long, default_value = None)]
    csv: Option<String>,
    #[arg(long, default_value = None)]
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
        let row = runner(
            args.gridsize,
            args.comptype,
            ix,
            args.determinism,
            Some(seed + ix),
        );
        let _ = write_csv_row(csvpath, &row);
    }

    // build_chart(rows);
    Ok(())
}
