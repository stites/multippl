use crate::Row;
use csv::WriterBuilder;
use std::error::Error;
use std::fs;
use std::time::Duration;

type MyResult<X> = Result<X, Box<dyn Error>>;

pub fn duration_to_usize(d: &Duration) -> usize {
    d.as_micros().try_into().unwrap()
}

pub fn write_csv_header(path: &str) -> MyResult<()> {
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
pub fn write_csv_row(path: &str, row: &Row) -> MyResult<()> {
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
