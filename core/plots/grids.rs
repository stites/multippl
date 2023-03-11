#![allow(unused_imports)]

use csv::{Reader, Writer};
use itertools::*;
// use plotters::coord::types::*;
// use plotters::coord::*;
// use plotters::prelude::*;
// use plotters::style::text_anchor::{HPos, Pos, VPos};
use rayon::prelude::*;
use rsdd::sample::probability::Probability;
use std::error::Error;
use std::fs;
use std::time::{Duration, Instant};
// use tracing::*;
// use yodel::compile::grammar::*;
use yodel::grids::{make, GridSchema, Selection};
use yodel::inference::exact;
use yodel::inference::importance_weighting;
use yodel::inference::importance_weighting_h;
use yodel::typecheck::grammar::*;
// use yodel::*;

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

fn run_all_grids(path: &str) -> Vec<Row> {
    use CompileType::*;

    let _ = write_csv_header(path);
    let specs: Vec<_> = iproduct!(
        [4_usize],
        // [2, 3, 4, 5, 7, 9, 12, 15, 20, 25_usize],
        [Exact, OptApprox],
        // [Exact, Approx, OptApprox],
        [1_u64],
        // (1..=5_u64),
        [0.0_f64] // [0.25, 0.5, 0.75, 1.0_f64]
    )
    .collect_vec();
    let answers: Vec<Row> = specs
        .iter()
        .map(|(gridsize, comptype, ix, determinism)| {
            let seed = 5;
            let prg = define_program(*gridsize, comptype.use_sampled(), seed, *determinism);
            let start = Instant::now();
            match comptype {
                Exact => {
                    exact(&prg);
                }
                Approx => {
                    importance_weighting(1, &prg);
                }
                OptApprox => {
                    let opts = yodel::Options {
                        opt: true,
                        ..Default::default()
                    };
                    importance_weighting_h(1, &prg, &opts);
                }
            }
            let stop = Instant::now();
            let duration = stop.duration_since(start);
            let row = Row {
                gridsize: *gridsize,
                comptype: *comptype,
                duration,
                determinism: *determinism,
                ix: *ix,
                seed: seed,
            };
            let _ = write_csv_row(path, &row);
            row
        })
        .collect();
    answers
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

fn main() -> MyResult<()> {
    fs::create_dir_all("out/plots/")?;
    let _rows = run_all_grids("out/plots/grids.csv");
    // build_chart(rows);
    Ok(())
}
