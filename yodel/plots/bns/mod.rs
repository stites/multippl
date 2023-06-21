#[macro_use]
extern crate yodel;

use clap::Parser;
use rsgm::bayesian_network::BayesianNetwork;
use rsgm_networks::{ParseError, Specification};
use std::error::Error;
use std::str::FromStr;
use yodel::bayesian_network::*;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    network: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let spec = Specification::_from_str(&args.network)?;

    println!("{:?}", spec);

    // let bn = BayesianNetwork::from_string(std::fs::read_to_string(&args.file).unwrap().as_str());
    // print_network!(bn);
    println!("----------------------------");
    // let program = compile_allmarg(&bn);
    // pprint(&program);

    for spec in Specification::iterator() {
        let n = spec.network();
        let vars = n.get_variables();
    }

    Ok(())
}
