use clap::Parser;
use itertools::*;
use rsgm::bayesian_network::BayesianNetwork;
use std::error::Error;
use std::fs;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    file: String,
}

// impl BayesianNetwork {
//     fn parent_assignments(&self, var: String) -> HashMap<String, String> {
//         ""
//     }

// }
fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let bn = BayesianNetwork::from_string(std::fs::read_to_string(&args.file).unwrap().as_str());
    println!("{:?}", bn);
    println!("");
    println!("{:?}", bn.topological_sort());

    for v in bn.topological_sort() {
        println!("{}", v);
        println!("    assignments: {}", bn.get_all_assignments(&v).join(","));
        println!("    parents    : {}", bn.get_parents(&v).join(","));
        println!("    CPT");
        for passigns in bn.parent_assignments(&v) {
            let show_passigns = passigns
                .iter()
                .map(|(p, pa)| format!("{}={}", p, pa))
                .collect_vec()
                .join(", ");
            for vassign in bn.get_all_assignments(&v) {
                println!(
                    "            P({} = {}\t| {})\t: {}",
                    v,
                    vassign,
                    show_passigns,
                    bn.get_conditional_prob(&v, &vassign, &passigns)
                );
            }
        }
    }
    Ok(())
}
