use csv::Writer;
use itertools::*;
use plotters::coord::types::*;
use plotters::coord::*;
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use rsdd::sample::probability::Probability;
use spinoff::*;
use std::error::Error;
use std::fs;
use std::time::{Duration, Instant};
use yodel::grids::{make, GridSchema, Selection};
use yodel::inference::exact;
use yodel::inference::importance_weighting;

struct Row {
    gridsize: usize,
    exact: Duration,
    approx: Duration,
}
impl Row {
    fn csv_array(&self) -> [String; 3] {
        let a = format!("{}", self.gridsize);
        let b = format!("{}", self.exact.as_micros());
        let c = format!("{}", self.approx.as_micros());
        [a, b, c]
    }
}

fn run_one_grid(sp: &mut Spinner, size: usize, determinism: f64) -> Row {
    let mk_probability = |_ix, _p| Probability::new(0.5);
    let schema = GridSchema::new_from_fn(
        size,
        false,
        None,
        Some(0.5),
        Selection::Random,
        None,
        &mk_probability,
    );
    let grid = make::grid(schema);

    let start = Instant::now();
    sp.update_text(format!(
        "[{size}x{size}][{start:?}] compiling exact circuit..."
    ));
    let _ = exact(&grid);
    let now = Instant::now();
    let edur = now.duration_since(start);
    sp.update_text(format!("[{size}x{size}][{now:?}] compiled exact circuit!"));

    sp.update_after_time(
        format!("[{size}x{size}][{now:?}] compiling approximate circuit..."),
        Duration::from_secs(2),
    );
    let start = Instant::now();
    let _ = importance_weighting(1, &grid);
    let now = Instant::now();
    let adur = now.duration_since(start);
    sp.update_text(format!(
        "[{size}x{size}][{now:?}] compiled approximate circuit!"
    ));
    Row {
        gridsize: size,
        exact: edur,
        approx: adur,
    }
}

fn write_csv_header(path: &str) -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path(path)?;
    wtr.write_record(&["grid size", "exact(µs)", "approx(µs)"])?;
    wtr.flush()?;
    Ok(())
}
fn write_csv_row(path: &str, row: &Row) -> Result<(), Box<dyn Error>> {
    let mut file = fs::OpenOptions::new()
        .write(true)
        .append(true)
        .create_new(true)
        .open(path)
        .unwrap();
    let mut wtr = Writer::from_writer(file);
    wtr.write_record(&row.csv_array())?;
    wtr.flush()?;
    Ok(())
}

fn make_spinner() -> Spinner {
    Spinner::new(
        spinners::Dots,
        "building circuits...",
        spinoff::Color::Green,
    )
}
fn run_all_grids(sp: &mut Spinner, path: &str, determinism: f64) -> Vec<Row> {
    write_csv_header(path);
    let mut rows = vec![];
    for size in [2, 3, 4, 5, 7, 9, 12, 15, 20, 25] {
        let row = run_one_grid(sp, size, determinism);
        write_csv_row(path, &row);
        rows.push(row);
    }
    rows
}

// type ChartScaffold<DB> = Result<(plotters::chart::ChartBuilder<'static, 'static, DB>, DrawingArea<plotters::prelude::BitMapBackend<'static>, Shift>), Box<(dyn std::error::Error + 'static)>>;
// type ChartScaffold = Result<(ChartContext<'static, BitMapBackend<'static>, Cartesian2d<RangedCoordf32, RangedCoordf32>>, DrawingArea<BitMapBackend<'static>, Shift>), Box<(dyn std::error::Error + 'static)>>;
type ChartScaffold = Result<(), Box<(dyn std::error::Error + 'static)>>;

fn build_chart(rows: Vec<Row>) -> ChartScaffold {
    let root = BitMapBackend::new("out/plots/grid.png", (640, 480)).into_drawing_area();
    root.fill(&WHITE)?;
    let root = root.margin(10, 10, 10, 10);
    let mut chart = ChartBuilder::on(&root)
        .caption("Grid construction", ("sans-serif", 40).into_font())
        .x_label_area_size(20)
        .y_label_area_size(40)
        .build_cartesian_2d(0f32..10f32, 0f32..10f32)?;
    let anchor_position = (200, 100);
    let anchor_left_bottom = Pos::new(HPos::Left, VPos::Bottom);
    let anchor_right_top = Pos::new(HPos::Right, VPos::Top);
    let text_style_right_top = BLACK.with_anchor::<RGBColor>(anchor_right_top);
    chart
        .configure_mesh()
        .x_labels(5)
        .x_desc("duration (s)")
        .y_labels(5)
        .y_desc("grid size")
        .axis_desc_style(text_style_right_top)
        .y_label_formatter(&|x| format!("{:.2}", x))
        .x_label_formatter(&|x| format!("{:.0}", x))
        .draw()?;

    let as_f32 = |dur: Duration| dur.as_secs() as f32;
    // exact
    chart.draw_series(LineSeries::new(
        rows.iter()
            .map(|r| (r.gridsize as f32, as_f32(r.exact)))
            .collect_vec(),
        &BLUE,
    ))?;

    chart.draw_series(LineSeries::new(
        rows.iter()
            .map(|r| (r.gridsize as f32, as_f32(r.approx)))
            .collect_vec(),
        &BLUE,
    ))?;
    root.present()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all("out/plots/")?;
    let mut sp = make_spinner();
    let rows = run_all_grids(&mut sp, "out/plots/grids.csv", 0.5);
    build_chart(rows);
    Ok(())
}
