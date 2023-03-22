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
use yodel::compile::Importance;
use yodel::grids::*;
use yodel::inference::*;
use yodel::typeinf::grammar::*;

pub mod csv;
mod data;
pub mod generate;
pub mod tasks;
use crate::csv::*;
use crate::data::*;
use crate::tasks::*;

type MyResult<X> = Result<X, Box<dyn Error>>;

pub struct QueryRet(Vec<f64>);
fn variance_runner(
    gridsize: usize,
    comptype: CompileType,
    determinism: Det,
    runs: usize,
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

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
struct PlotGridsArgs {
    #[arg(short, default_value_t = 0)]
    verbosity: usize,
    #[arg(long, default_value = None)]
    path: Option<String>,
    #[command(subcommand)]
    command: CommandType,
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

#[derive(clap::Subcommand, Debug, Clone)]
enum CommandType {
    Duration(RunArgs),
    Variance(RunArgs),
    DurationStats,
}

fn main() -> MyResult<()> {
    use CommandType::*;
    let cliargs = PlotGridsArgs::parse();
    let verbosity = match cliargs.verbosity {
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
    let path = cliargs
        .path
        .clone()
        .unwrap_or_else(|| String::from("out/plots/"));
    // info section
    info!("verbosity  : {:?}", verbosity);
    info!("path       : {:?}", path);
    match &cliargs.command {
        Duration(args) => crate::duration::main(path, args.clone()),
        DurationStats => crate::duration::stats(path),
        Variance(args) => crate::variance::main(path, args.clone()),
    }
    // build_chart(rows);
    Ok(())
}
