#[macro_use]
extern crate yodel;

use clap::Parser;
use std::error::Error;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    file: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    println!("{:?}", args);
    Ok(())
}
