#[macro_use]
extern crate yodel;

use clap::Parser;
use rsgm::bayesian_network::BayesianNetwork;
use rsgm_networks::{discrete::Specification, ParseError};
use std::error::Error;
use std::str::FromStr;
use yodel::bayesian_network::*;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    network: String,

    #[clap(short, long)]
    list: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let spec = Specification::_from_str(&args.network)?;
    if args.list {}

    println!("{:?}", spec);

    let bn = spec.network();
    print_network!(bn);
    println!("----------------------------");

    for spec in Specification::iterator() {
        let n = spec.network();
        let vars = n.get_variables();
    }
    let opts = yodel::Options::new(Some(7), false, false, 0);
    let program = compile_allmarg(&bn);
    // pprint(&program);
    println!("----------------------------");
    let (rez, stats) = yodel::inference::exact_with(&program);
    println!("answer: {:?}", rez);
    println!("stats : {:?}", stats);

    // insert_sample_statements(p: &ProgramInferable) -> ProgramAnn {

    // Exact => panic!("exact compile type not supported for 'variance' task"),
    // OptApx => panic!("optimized approx on hold"),
    // Approx => {
    //     for res in SamplingIter::new(runs, &prg, &opts) {
    //         let (query, _weight) = (res.expectations.query(), res.weight.clone());
    //         let l1 = l1_distance(&exp, &query);
    //         if (res.step > runs - 10) || (res.step < 10) {
    //             println!("{}: {:.8}", res.step, l1);
    //         }
    //     }
    // }
    Ok(())
}
