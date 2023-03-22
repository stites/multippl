use rsdd::sample::probability::Probability;
use yodel::grids::*;
use yodel::typeinf::grammar::ProgramInferable;

pub fn program(size: usize, sampled: bool, prg_seed: u64, determinism: f64) -> ProgramInferable {
    let mk_probability = |_ix, _p| Probability::new(0.5);
    let schema = GridSchema::new_from_fn(
        size,
        sampled,
        Some(prg_seed),
        Some(determinism),
        Selection::Random,
        None,
        &mk_probability,
    );
    make::grid(schema)
}
