#[macro_use]
extern crate yodel;

use clap::Parser;
use rsgm::bayesian_network::BayesianNetwork;
use std::error::Error;
use yodel::bayesian_network::*;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    file: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let bn = BayesianNetwork::from_string(std::fs::read_to_string(&args.file).unwrap().as_str());
    print_network!(bn);
    println!("----------------------------");
    let program = compile_allmarg(&bn);
    pprint(&program);
    Ok(())
}
