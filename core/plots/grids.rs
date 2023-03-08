use csv::Writer;
use plotters::{
    prelude::*,
    style::text_anchor::{HPos, Pos, VPos},
};
use std::error::Error;
use std::fs;

/// pull in example from csv to persist plot statistics in case plotters goes wrong.
fn example() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("foo.csv")?;
    wtr.write_record(&["a", "b", "c"])?;
    wtr.write_record(&["x", "y", "z"])?;
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all("out/plots/")?;
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
        // We can also change the format of the label text
        .y_label_formatter(&|x| format!("{:.2}", x))
        .x_label_formatter(&|x| format!("{:.0}", x))
        .draw()?;

    // And we can draw something in the drawing area
    chart.draw_series(LineSeries::new(
        vec![(0.0, 0.0), (5.0, 5.0), (8.0, 7.0)],
        &RED,
    ))?;
    root.present()?;
    Ok(())
}
