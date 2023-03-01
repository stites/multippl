use clap::Parser;

#[derive(Parser)]
#[command(name = "formulas")]
#[command(about = "turn a quick ascii formula into a BDD", long_about = None)]
#[command(author, version, about, long_about = None)]
struct Cli {
    rest: Vec<String>,
}
fn main() {
    let cli = Cli::parse();
    let src = cli.rest.join(" ");

    match formula::eval(src) {
        Err(s) => {
            println!("Error! {}", s);
        }
        Ok((out, _)) => {
            println!("{}", out.circuit.print_bdd());
        }
    }
}
