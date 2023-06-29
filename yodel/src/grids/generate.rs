use crate::grids::*;
use crate::typeinf::grammar::ProgramInferable;
use itertools::*;
use rsdd::sample::probability::Probability;
use std::fmt::Debug;

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

pub struct ObsId(pub Vec<usize>);
impl ToString for ObsId {
    fn to_string(&self) -> String {
        self.0.iter().map(|x| x.to_string()).join(":")
    }
}
pub fn disjunction<X: Debug + Clone + PartialEq>(vars: Vec<AnfInferable<X>>) -> AnfInferable<X>
where
    AVarExt<X>: ξ<Inferable>,
    AValExt<X>: ξ<Inferable>,
    <AVarExt<X> as ξ<crate::typeinf::grammar::Inferable>>::Ext: Debug + Clone + PartialEq,
    <AValExt<X> as ξ<crate::typeinf::grammar::Inferable>>::Ext: Debug + Clone + PartialEq,
{
    println!("{:?}", vars);
    vars.into_iter()
        .fold(None, |fin, var| match fin {
            None => Some(var),
            Some(prev) => Some(Anf::Or(Box::new(prev), Box::new(var))),
        })
        .unwrap()
}

pub fn sliding_observes(
    p0: &ProgramInferable,
    clause_size: usize,
) -> Vec<(ObsId, ProgramInferable)> {
    use EExpr::*;
    match p0.query() {
        Query::EQuery(EProd(_, qs)) => qs
            .iter()
            .enumerate()
            .combinations(clause_size)
            .map(|ivars| {
                let ixs = ivars.iter().map(|(i, _)| *i).collect_vec();
                let vars = ivars.into_iter().map(|(_, v)| v.clone()).collect_vec();
                let obs = disjunction(vars);
                let p = p0.clone();
                let p = p.insert_observe(EObserve((), Box::new(obs)));
                (ObsId(ixs), p)
            })
            .collect_vec(),
        _ => panic!("impossible"),
    }
}

pub fn program_sliding_observes(
    size: usize,
    sampled: bool,
    prg_seed: u64,
    determinism: f64,
    clause_size: usize,
) -> Vec<(ObsId, ProgramInferable)> {
    sliding_observes(&program(size, sampled, prg_seed, determinism), clause_size)
}
