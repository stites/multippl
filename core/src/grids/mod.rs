use crate::grammar::Expr::*;
use crate::grammar::*;
use crate::typecheck::grammar::*;
use crate::ExprTyped;
use crate::*;
use itertools::*;
use rsdd::sample::probability::*;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
use tracing::*;
use tracing_test::traced_test;

#[cfg(test)]
mod tests;

pub struct GridSpec<'a> {
    size: usize,
    sampled: bool,
    query: ExprTyped,
    probability: &'a dyn Fn(Ix, Parents<bool>) -> Probability,
}

impl<'a> GridSpec<'a> {
    pub fn new(
        size: usize,
        query: &ExprTyped,
        sampled: bool,
        probability: &'a dyn Fn(Ix, Parents<bool>) -> Probability,
    ) -> Self {
        GridSpec {
            size,
            query: query.clone(),
            sampled,
            probability,
        }
    }
}
#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug, PartialOrd, Ord)]
pub struct Ix(usize, usize);

impl Ix {
    pub fn from_tuple(ij: (usize, usize)) -> Ix {
        let (i, j) = ij;
        Ix(i, j)
    }
    pub fn new(i: usize, j: usize) -> Ix {
        Ix(i, j)
    }
    pub fn as_string(&self) -> String {
        format!("{}{}", self.0, self.1)
    }
    pub fn to_string(i: Ix) -> String {
        format!("{}{}", i.0, i.1)
    }
}

#[derive(Eq, Clone, Copy, Debug)]
pub enum Parents<X> {
    Zero,
    One(Ix, X),
    Two((Ix, X), (Ix, X)),
}
impl<X> Parents<X> {
    pub fn to_vec(&self) -> Vec<Ix> {
        use Parents::*;
        match self {
            Zero => vec![],
            One(i, _) => vec![*i],
            Two((i, _), (j, _)) => vec![*i, *j],
        }
    }
}
use std::hash::*;
impl<X: Hash> Hash for Parents<X> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Parents::*;
        match self {
            Zero => 0_usize.hash(state),
            One(il, l) => {
                1_usize.hash(state);
                il.hash(state);
                l.hash(state);
            }
            Two((il, l), (ir, r)) => {
                2_usize.hash(state);
                let (a, b, c, d) = match il.cmp(&ir) {
                    Ordering::Less => (il, l, ir, r),
                    Ordering::Equal => (il, l, ir, r),
                    Ordering::Greater => (ir, r, il, l),
                };
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
            }
        }
    }
}
impl<X: PartialEq> PartialEq for Parents<X> {
    fn eq(&self, o: &Self) -> bool {
        use Parents::*;
        match (self, o) {
            (Zero, Zero) => true,
            (One(il, l), One(ir, r)) => il == ir && l == r,
            (Two((il1, l1), (il2, l2)), Two((ir1, r1), (ir2, r2))) => {
                match (il1 == ir1, il2 == ir2, il1 == ir2, il2 == ir1) {
                    (true, true, _, _) => l1 == r1 && l2 == r2,
                    (_, _, true, true) => l1 == r2 && l2 == r1,
                    _ => false,
                }
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GridSchema {
    tril: HashSet<Ix>,
    triu: HashSet<Ix>,
    diag: HashSet<Ix>,

    flips: HashMap<(Ix, Parents<bool>), Probability>,
    parents: HashMap<Ix, Parents<()>>,
    size: usize,
    sampled: bool,
    query: ExprTyped,
}
impl GridSchema {
    pub fn get_flip(&self, ix: Ix, p: Parents<bool>) -> f64 {
        self.flips
            .get(&(ix, p))
            .unwrap_or_else(|| panic!("get_flip failed to find {ix:?} with parents {p:?}!"))
            .as_f64()
    }
    pub fn get_parents(&self, ix: Ix) -> Parents<()> {
        *self
            .parents
            .get(&ix)
            .expect("get_parents failed to find {ix:?}!")
    }
    pub fn get_parents_vec(&self, ix: Ix) -> Vec<Ix> {
        self.get_parents(ix).to_vec()
    }
    pub fn new_from_fn(
        size: usize,
        sampled: bool,
        query: Option<&ExprTyped>,
        probability: &dyn Fn(Ix, Parents<bool>) -> Probability,
    ) -> GridSchema {
        let query = match query {
            Some(q) => q.clone(),
            None => make::full_query(size),
        };
        make::schema(GridSpec::new(size, &query, sampled, probability))
    }
    pub fn new_from_map(
        size: usize,
        sampled: bool,
        query: &ExprTyped,
        probmap: &HashMap<(Ix, Parents<bool>), Probability>,
    ) -> GridSchema {
        let prob = |ix, p| {
            probmap
                .get(&(ix, p))
                .cloned()
                .unwrap_or_else(|| Probability::new(0.0))
        };
        make::schema(GridSpec::new(size, query, sampled, &prob))
    }
}
fn get_parents(ix: Ix, size: usize) -> Parents<()> {
    use Parents::*;
    let p0 = usize::checked_sub(ix.0, 1).map(|i0| Ix::new(i0, ix.1));
    let p1 = usize::checked_sub(ix.1, 1).map(|i1| Ix::new(ix.0, i1));
    match (p0, p1) {
        (None, None) => Zero,
        (Some(p), None) => One(p, ()),
        (None, Some(p)) => One(p, ()),
        (Some(l), Some(r)) => Two((l, ()), (r, ())),
    }
}

#[rustfmt::skip]
pub mod make {
    use super::*;
    const DIAG : &str = "diag";
    pub fn schema(spec: GridSpec) -> GridSchema {
        use Parents::*;
        if spec.size < 2 {
            panic!("grid size too small and I don't want to make this typesafe yet");
        }
        let mut flips = HashMap::new();
        let mut parents = HashMap::new();
        let mut diag = HashSet::new();
        let mut tril = HashSet::new();
        let mut triu = HashSet::new();
        let sampled = spec.sampled;
        let query = spec.query;

        for tuple in iproduct!((0..spec.size), (0..spec.size)) {
            let i = Ix::new(tuple.0, tuple.1);
            let ps = get_parents(i, spec.size);
            parents.insert(i, ps);
            match ps {
                Zero => {flips.insert((i, Zero), (spec.probability)(i, Zero)); },
                One(p, _) => {
                    flips.insert((i, One(p, true)), (spec.probability)(i, One(p, true)));
                    flips.insert((i, One(p, false)), (spec.probability)(i, One(p, false)));
                },
                Two((p1, _), (p2, _)) => {
                    for (b1, b2) in [(true,  true), (true, false), (false, true), (false, false)] {
                        let par = Two((p1, b1), (p2, b2));
                        let par_inv = Two((p2, b2), (p1, b1));
                        let pr = (spec.probability)(i, par);
                        let pr_inv = (spec.probability)(i, par_inv);
                        assert_eq!(pr, pr_inv, "error! probability function needs to account for symmetry in {:?} with parents: {:?}, {:?}", i, (p1, b1), (p2, b2));
                        flips.insert((i, par), pr);
                        flips.insert((i, par_inv), pr);
                    }
                },
            }
            match (i.0 + i.1).cmp(&(spec.size - 1)) {
                Ordering::Less => triu.insert(i),
                Ordering::Greater => tril.insert(i),
                Ordering::Equal => diag.insert(i),
            };
        }

        GridSchema {
            flips,
            parents,
            tril,
            triu,
            diag,
            sampled,
            query,
            size: spec.size,
        }
    }

    pub fn no_parents(schema: &GridSchema, ix: Ix, rest: ExprTyped) -> ExprTyped {
        let pr = schema.get_flip(ix, Parents::Zero);
        ELetIn(
            LetInTypes {
                bindee: b!(),
                body: b!(),
            },
            ix.as_string(),
            Box::new(EFlip((), pr)),
            Box::new(rest),
        )
    }

    pub fn one_parents(schema: &GridSchema, ix: Ix, parent: Ix, rest: ExprTyped) -> ExprTyped {
        let p_t = schema.get_flip(ix, Parents::One(parent, true));
        let p_f = schema.get_flip(ix, Parents::One(parent, false));
        let p = parent.as_string();

        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            ix.as_string(),
            Box::new(ite![(var!(@ p)) ? (flip!(@ p_t)) : (flip!(@ p_f))]),
            Box::new(rest),
        )
    }

    pub fn two_parents(schema: &GridSchema, ix: Ix, parents: (Ix, Ix), rest: ExprTyped) -> ExprTyped {
        let (pl, pr) = parents;
        let (pl, pr) = (pl.as_string(), pr.as_string());
        let p_t_t = schema.get_flip(ix, Parents::Two((parents.0, true), (parents.1, true)));
        let p_f_t = schema.get_flip(ix, Parents::Two((parents.0, false), (parents.1, true)));
        let p_t_f = schema.get_flip(ix, Parents::Two((parents.0, true), (parents.1, false)));
        let p_f_f = schema.get_flip(ix, Parents::Two((parents.0, false), (parents.1, false)));
        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            ix.as_string(),
            Box::new(
                ite!((b!((var!(@ pl)) && (var!(@ pr)))) ? (flip!(@ p_t_t)) : (
                ite!((b!((var!(@ pl)) && (not!(@ pr)))) ? (flip!(@ p_t_f)) : (
                ite!((b!((not!(@ pl)) && (var!(@ pr)))) ? (flip!(@ p_f_t)) : (
                                                           flip!(@ p_f_f)))))))
            ),
            Box::new(rest),
        )
    }

    pub fn diag_alias(schema: &GridSchema, ix: Ix, prj: usize, rest: ExprTyped) -> ExprTyped {
        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            ix.as_string(),
            Box::new(EPrj(b!(), prj, Box::new(var!(@ DIAG)))),
            Box::new(rest),
        )
    }
    pub fn diag_ixs(schema: &GridSchema) -> Vec<Ix> {
        izip!((0..schema.size), (0..schema.size).rev())
            .map(|(j, i)| Ix::new(j, i))
            .collect()
    }
    pub fn product_of(ixs:Vec<Ix>) -> ExprTyped {
        let mut prod = vec![];
        let mut ty = vec![];
        for i in ixs {
            let s = i.as_string();
            prod.push(var!(@ s));
            ty.push(Ty::Bool);
        }
        Expr::EProd(Ty::Prod(ty), prod)
    }
    pub fn all_diag_aliases(schema: &GridSchema, rest: ExprTyped) -> ExprTyped {
        let ixs = diag_ixs(schema);
        debug!("{ixs:?}");
        diag_ixs(schema)
            .iter()
            .cloned()
            .enumerate()
            .rev() // just to keep everything in order, visually
            .fold(rest, |prg, (i, ix) | {
                diag_alias(schema, ix, i, prg)
            })
    }
    pub fn sample_diag(prg: ExprTyped, tril: ExprTyped) -> ExprTyped {
        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            DIAG.to_string(),
            Box::new(ESample((), Box::new(prg))),
            Box::new(tril),
        )
    }
    pub fn fill_schema(schema: &GridSchema, region: &HashSet<Ix>, seen: HashSet<Ix>, seed: Vec<Ix>, prg: ExprTyped) -> (ExprTyped, HashSet<Ix>, Vec<Ix>) {
        use Parents::*;
        let mut q = VecDeque::from(seed.clone());
        let mut seen = seen;
        let mut next = vec![];
        let mut prg = prg;
        debug!("seen: {seen:?}");
        debug!("seed: {seed:?}");
        debug!("region: {region:?}");

        while let Some(ix) = q.pop_front() {
            if seen.contains(&ix) {
                debug!("{ix:?} seen");
                // continue;
            } else if !region.contains(&ix) {
                debug!("{ix:?} belongs in next queue");
                next.push(ix);
                // continue;
            } else {
                let parents = schema.get_parents(ix);
                prg = match parents {
                    Zero => make::no_parents(schema, ix, prg),
                    One(p, _) => make::one_parents(schema, ix, p, prg),
                    Two((l, _), (r, _)) => make::two_parents(schema, ix, (l, r), prg),
                };
                seen.insert(ix);
                let mut nxt_parents = parents.to_vec().into();
                debug!("{ix:?} added");
                debug!("queuing: {nxt_parents:?}");
                q.append(&mut nxt_parents);
            }
        }

        (prg, seen, next)
    }

    pub fn tril(schema: &GridSchema) -> (ExprTyped, HashSet<Ix>, Vec<Ix>) {
        let span = tracing::span!(tracing::Level::DEBUG, "tril");
        let _enter = span.enter();

        fill_schema(schema, &schema.tril, HashSet::from([]), vec![Ix::new(schema.size-1, schema.size-1)], schema.query.clone())
    }

    pub fn diag(schema: &GridSchema, prg:ExprTyped, seen:HashSet<Ix>, build_diag:Vec<Ix>) -> (ExprTyped, HashSet<Ix>, Vec<Ix>) {
        let span = tracing::span!(tracing::Level::DEBUG, "diag");
        let _enter = span.enter();

        let mut prg = prg;
        let mut tril_prg : Option<ExprTyped> = None;
        if schema.sampled {
            prg = all_diag_aliases(schema, prg);
            tril_prg = Some(prg);
            let ixs = make::diag_ixs(schema);
            prg = make::product_of(ixs);
        }
        let (mut prg, seen, next_triu) = fill_schema(schema, &schema.diag, seen, build_diag, prg);
        if schema.sampled {
            prg = make::sample_diag(prg, tril_prg.unwrap());
        }
        (prg, seen, next_triu)
    }

    pub fn triu(schema: &GridSchema, prg:ExprTyped, seen:HashSet<Ix>, build_triu:Vec<Ix>) -> (ExprTyped, HashSet<Ix>, Vec<Ix>) {
        let span = tracing::span!(tracing::Level::DEBUG, "triu");
        let _enter = span.enter();
        fill_schema(schema, &schema.triu, seen, build_triu, prg)
    }

    pub fn grid(schema: GridSchema) -> ProgramTyped {
        let (prg, seen, next_diag) = tril(&schema);
        let (prg, seen, next_triu) = diag(&schema, prg, seen, next_diag);
        let (prg, seen, remainder) = triu(&schema, prg, seen, next_triu);
        assert_eq!(remainder, vec![], "grid construction incomplete! found remaining nodes on the left");
        program!(prg)
    }
    pub fn full_query(size: usize) -> ExprTyped {
        let ps = iproduct!((0..size), (0..size))
            .map(Ix::from_tuple)
            .map(Ix::to_string)
            .map(crate::typecheck::grammar::AnfTyped::var)
            .collect_vec();
        Expr::EProd(b!(), ps)
    }
}
