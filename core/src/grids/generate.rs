use crate::grids::*;
use crate::typeinf::grammar::ProgramInferable;
use rsdd::sample::probability::Probability;

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

pub fn sliding_observes(p0: &ProgramInferable) -> Vec<ProgramInferable> {
    use Expr::*;
    let mut ps = vec![];
    match p0.query() {
        EProd(_, qs) => {
            for q in qs {
                let p = p0.clone();
                let pnew = p.insert_observe(EObserve((), Box::new(q)));
                ps.push(pnew);
            }
        }
        _ => panic!("impossible"),
    }
    ps
}

pub fn program_sliding_observes(
    size: usize,
    sampled: bool,
    prg_seed: u64,
    determinism: f64,
) -> Vec<ProgramInferable> {
    sliding_observes(&program(size, sampled, prg_seed, determinism))
}
