use super::Expr::*;
use crate::grammar::*;
use crate::typecheck::grammar::*;
use crate::ExprTyped;
use crate::*;
use itertools::*;
use rsdd::sample::probability::*;
use std::collections::{HashMap, HashSet, VecDeque};
use tracing::*;
use tracing_test::traced_test;

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
#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug)]
pub struct Ix(usize, usize);

impl Ix {
    pub fn new(i: usize, j: usize) -> Ix {
        Ix(i, j)
    }
    pub fn as_string(&self) -> String {
        format!("{}{}", self.0, self.1)
    }
}

#[derive(Eq, Hash, Clone, Copy, Debug)]
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
mod make {
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
                        assert_eq!(pr, pr_inv, "error! probability function needs to account for symmetry in {:?} with parents: {:?}, {:?}", i, p1, p2);
                        flips.insert((i, par), pr);
                        flips.insert((i, par_inv), pr);
                    }
                },
            }
            if (i.0 + i.1) < (spec.size - 1) {
                triu.insert(i);
            } else if (i.0 + i.1) > (spec.size - 1) {
                tril.insert(i);
            } else {
                diag.insert(i);
            }
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
            Box::new(rest.clone()),
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
        let mut seen = seen.clone();
        let mut next = vec![];
        let mut prg = prg.clone();
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
                    Zero => make::no_parents(&schema, ix, prg),
                    One(p, _) => make::one_parents(&schema, ix, p, prg),
                    Two((l, _), (r, _)) => make::two_parents(&schema, ix, (l, r), prg),
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

        fill_schema(&schema, &schema.tril, HashSet::from([]), vec![Ix::new(schema.size-1, schema.size-1)], schema.query.clone())
    }

    pub fn diag(schema: &GridSchema, prg:ExprTyped, seen:HashSet<Ix>, build_diag:Vec<Ix>) -> (ExprTyped, HashSet<Ix>, Vec<Ix>) {
        let span = tracing::span!(tracing::Level::DEBUG, "diag");
        let _enter = span.enter();

        let mut prg = prg;
        let mut tril_prg : Option<ExprTyped> = None;
        if schema.sampled {
            prg = all_diag_aliases(&schema, prg);
            tril_prg = Some(prg);
            let ixs = make::diag_ixs(&schema);
            prg = make::product_of(ixs);
        }
        let (mut prg, seen, next_triu) = fill_schema(&schema, &schema.diag, seen, build_diag, prg);
        if schema.sampled {
            prg = make::sample_diag(prg, tril_prg.unwrap());
        }
        (prg, seen, next_triu)
    }

    pub fn triu(schema: &GridSchema, prg:ExprTyped, seen:HashSet<Ix>, build_triu:Vec<Ix>) -> (ExprTyped, HashSet<Ix>, Vec<Ix>) {
        let span = tracing::span!(tracing::Level::DEBUG, "triu");
        let _enter = span.enter();
        fill_schema(&schema, &schema.triu, seen, build_triu, prg)
    }

    pub fn grid(schema: GridSchema) -> ProgramTyped {
        let (prg, seen, next_diag) = tril(&schema);
        let (prg, seen, next_triu) = diag(&schema, prg, seen, next_diag);
        let (prg, seen, remainder) = triu(&schema, prg, seen, next_triu);
        assert_eq!(remainder, vec![], "grid construction incomplete! found remaining nodes on the left");
        program!(prg)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests::*;
    pub fn ix(i: usize, j: usize) -> Ix {
        Ix::new(i, j)
    }

    macro_rules! pkey {
        (=> $i:literal $j:literal) => {{
            (Ix::new($i, $j), Parents::Zero)
        }};
        ($pi:literal $pj:literal $pbool:literal => $i:literal $j:literal) => {{
            (Ix::new($i, $j), Parents::One(Ix::new($pi, $pj), $pbool))
        }};
        (($p1i:literal $p1j:literal $p1bool:literal, $p2i:literal $p2j:literal $p2bool:literal)  => $i:literal $j:literal) => {{
            (
                Ix::new($i, $j),
                Parents::Two(
                    (Ix::new($p1i, $p1j), $p1bool),
                    (Ix::new($p2i, $p2j), $p2bool),
                ),
            )
        }};
    }

    macro_rules! pr {
        ($p:literal) => {{
            Probability::new($p)
        }};
        ($n:literal / $d:literal) => {{
            Probability::new(($n as f64) / ($d as f64))
        }};
    }

    macro_rules! pmap {
        ( $(($key:expr , $val:expr)),*) => {{
            let mut probmap: HashMap<(Ix, Parents<bool>), Probability> = HashMap::new();
            $(
                probmap.insert($key, $val);
            )*
            probmap
        }};
        ( $($p:literal @ $key:expr),* ) => {{
            let mut probmap: HashMap<(Ix, Parents<bool>), Probability> = HashMap::new();
            $(
                probmap.insert($key, pr!($val));
            )*
            probmap
        }};
        ( $($n:literal / $d:literal @ $key:expr),* ) => {{
            let mut probmap: HashMap<(Ix, Parents<bool>), Probability> = HashMap::new();
            $(
                probmap.insert($key, pr!($n / $d));
                let (i, p) = $key;
                match p {
                    Parents::Two((li, l), (ri, r)) => {
                        probmap.insert((i, Parents::Two((ri, r), (li, l))), pr!($n / $d));
                    }
                    _ => {},
                }
            )*
            probmap
        }};
    }
    fn make_2x2_pmap() -> HashMap<(Ix, Parents<bool>), Probability> {
        pmap![
            1 / 2 @ pkey!(=> 0 0),

            1 / 3 @ pkey!(0 0 true  => 0 1),
            1 / 4 @ pkey!(0 0 false => 0 1),

            1 / 6 @ pkey!(0 0 true  => 1 0),
            1 / 5 @ pkey!(0 0 false => 1 0),

            1 /  7 @ pkey!((1 0 true, 0 1 true)   => 1 1),
            1 /  8 @ pkey!((1 0 true, 0 1 false)  => 1 1),
            1 /  9 @ pkey!((1 0 false, 0 1 true)  => 1 1),
            1 / 11 @ pkey!((1 0 false, 0 1 false) => 1 1)
        ]
    }

    #[test]
    fn test_grid_2x2_schema() {
        use Parents::*;
        let query = b!("11");
        let probmap = make_2x2_pmap();
        let schema = GridSchema::new_from_map(2, false, &query, &probmap);
        println!("{:?}", schema);
        assert_eq!(schema.size, 2, "size");
        assert_eq!(schema.tril, HashSet::from([ix(1, 1)]), "tril");
        assert_eq!(schema.triu, HashSet::from([ix(0, 0)]), "triu");
        assert_eq!(schema.diag, HashSet::from([ix(0, 1), ix(1, 0)]), "diag");

        let parents = HashMap::from([
            (ix(0, 0), Zero),
            (ix(1, 0), One(ix(0, 0), ())),
            (ix(0, 1), One(ix(0, 0), ())),
            (ix(1, 1), Two((ix(1, 0), ()), (ix(0, 1), ()))),
        ]);
        assert_eq!(schema.parents.len(), parents.len());
        for (k, v) in parents.iter() {
            assert!(
                schema.parents.contains_key(k),
                "{k:?} missing from {:?}",
                schema.parents.keys()
            );
            assert_eq!(schema.parents.get(k).unwrap(), v);
        }
        assert_eq!(schema.flips.len(), probmap.len());
        for (k, v) in probmap.iter() {
            assert!(
                schema.flips.contains_key(k),
                "{k:?} missing from {:?}",
                schema.flips.keys()
            );
            assert_eq!(schema.flips.get(k).unwrap(), v);
        }
    }

    #[test]
    fn test_make_2x2_tril() {
        use crate::grammar::Anf::*;
        use crate::grammar::Expr::*;
        use Parents::*;
        let query = b!("11");
        let probmap = make_2x2_pmap();
        let schema = GridSchema::new_from_map(2, false, &query, &probmap);

        let (prg, seen, next_diag) = make::tril(&schema);
        let seen: HashSet<Ix> = seen.into_iter().collect();
        let next_diag: HashSet<Ix> = next_diag.into_iter().collect();
        assert_eq!(seen, HashSet::from([ix(1, 1)]));
        assert_eq!(next_diag, HashSet::from([ix(0, 1), ix(1, 0)]));
        match &prg.query() {
            EAnf(_, a) => assert_eq!(*a, Box::new(b!(@anf "11"))),
            _ => assert!(false, "expected an anf statement!"),
        }
        let flip11 = make::two_parents(
            &schema,
            ix(1, 1),
            (ix(0, 1), ix(1, 0)),
            schema.query.clone(),
        );
        assert_eq!(prg, flip11);
        let expected = lets![
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
                ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                          flip!(1/11) ))))));
           ...? query.clone() ; B!()
        ];
        assert_eq!(prg, expected);
    }

    #[test]
    fn test_make_2x2_diag() {
        use crate::grammar::Anf::*;
        use crate::grammar::Expr::*;
        use Parents::*;
        let query = b!("11");
        let probmap = make_2x2_pmap();
        let schema = GridSchema::new_from_map(2, false, &query, &probmap);

        let (prg, seen, next_diag) = make::tril(&schema);
        let (prg, seen, next_triu) = make::diag(&schema, prg, seen, next_diag);
        let seen: HashSet<Ix> = seen.into_iter().collect();
        let next_triu: HashSet<Ix> = next_triu.into_iter().collect();
        assert_eq!(seen, HashSet::from([ix(0, 1), ix(1, 0), ix(1, 1)]));
        assert_eq!(next_triu, HashSet::from([ix(0, 0)]));
        match &prg.query() {
            EAnf(_, a) => assert_eq!(*a, Box::new(b!(@anf "11"))),
            _ => assert!(false, "expected an anf statement!"),
        }
        let (prg, _, _) = make::diag(
            &schema,
            query.clone(),
            HashSet::from([ix(1, 1)]),
            vec![ix(0, 1), ix(1, 0)],
        );
        let expected = lets![
            "10" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
            "01" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
           ...? query.clone() ; B!()
        ];
        assert_eq!(prg, expected);

        let mut sampled_schema = schema.clone();
        sampled_schema.sampled = true;
        let (prg, seen, next_triu) = make::diag(
            &sampled_schema,
            query.clone(),
            HashSet::from([ix(1, 1)]),
            vec![ix(0, 1), ix(1, 0)],
        );
        let seen: HashSet<Ix> = seen.into_iter().collect();
        let next_triu: HashSet<Ix> = next_triu.into_iter().collect();
        assert_eq!(seen, HashSet::from([ix(0, 1), ix(1, 0), ix(1, 1)]));
        assert_eq!(next_triu, HashSet::from([ix(0, 0)]));
        let expected = lets![
            "diag" ; B!() ;= sample!(lets![
                "10" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
                "01" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                ...? b!("01", "10") ; B!()
                ]);
           "01" ; B!() ;= EPrj(b!(), 0, Box::new(b!(@anf "diag")));
           "10" ; B!() ;= EPrj(b!(), 1, Box::new(b!(@anf "diag")));
           ...? query.clone() ; B!()
        ];
        assert_eq!(prg, expected);
    }
    #[test]
    fn test_grid_2x2_compiles() {
        use crate::grammar::Anf::*;
        use crate::grammar::Expr::*;
        let query = b!("11");
        let probmap = make_2x2_pmap();
        let schema = GridSchema::new_from_map(2, false, &query, &probmap);
        let grid = make::grid(schema);

        let expected = program!(lets![
            "00" ; B!() ;= flip!(1/2);
            "10" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
            "01" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
                ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                                     flip!(1/11) ))))));
            ...? query.clone() ; B!()
        ]);

        println!("{:?}", grid);
        match &grid {
            Program::Body(ELetIn(_, var, _, _)) => assert_eq!(var, &"00".to_string()),
            _ => assert!(false, "expected a let-in binding!"),
        }
        match &grid.query() {
            EAnf(_, a) => assert_eq!(*a, Box::new(b!(@anf "11"))),
            _ => assert!(false, "expected an anf statement!"),
        }
        assert_eq!(grid, expected);
        let schema = GridSchema::new_from_map(2, true, &query, &probmap);
        let grid = make::grid(schema);

        let expected = program!(lets![
            "00" ; B!() ;= flip!(1/2);
            "diag" ; B!() ;= sample!(lets![
                "10" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
                "01" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                ...? b!("01", "10") ; B!()
            ]);
            "01" ; B!() ;= EPrj(b!(), 0, Box::new(b!(@anf "diag")));
            "10" ; B!() ;= EPrj(b!(), 1, Box::new(b!(@anf "diag")));
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
                ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                          flip!(1/11) ))))));
            ...? query.clone() ; B!()
        ]);
        assert_eq!(grid, expected);
    }

    #[test]
    fn test_grid2x2_inference() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "00" ; B!() ;= flip!(1/2);
                "01" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                "10" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
                "11" ; B!() ;=
                ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
                ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                          flip!(1/11) ))))));
                ...? ret ; B!()
            ])
        };
        crate::tests::check_exact1("grid2x2/3/00", 1.0 / 2.0, &mk(b!("00")));
        crate::tests::check_exact1("grid2x2/3/01", 0.291666667, &mk(b!("01")));
        crate::tests::check_exact1("grid2x2/3/10", 0.183333333, &mk(b!("10")));
        crate::tests::check_exact1("grid2x2/3/11", 0.102927589, &mk(b!("11")));

        let query = b!("00", "01", "10", "11");
        let probmap = make_2x2_pmap();
        let schema = GridSchema::new_from_map(2, false, &query, &probmap);
        let grid = make::grid(schema);
        crate::tests::check_exact(
            "grid2x2/all",
            vec![1.0 / 2.0, 0.291666667, 0.183333333, 0.102927589],
            &grid,
        );

        crate::tests::check_approx(
            "grid2x2/all",
            vec![1.0 / 2.0, 0.291666667, 0.183333333, 0.102927589],
            &grid,
            2000,
        );
    }

    #[test]
    fn test_grid2x2_sampled_inference() {
        let query = b!("00", "01", "10", "11");
        let probmap = make_2x2_pmap();
        let schema = GridSchema::new_from_map(2, true, &query, &probmap);
        let grid = make::grid(schema);
        check_approx(
            "grid2x2/all",
            vec![1.0 / 2.0, 0.291666667, 0.183333333, 0.102927589],
            &grid,
            2000,
        );
    }
    fn make_3x3_pmap() -> HashMap<(Ix, Parents<bool>), Probability> {
        pmap![
            1 / 2 @ pkey!(=> 0 0),

            1 / 3 @ pkey!(0 0 true  => 0 1),
            1 / 4 @ pkey!(0 0 false => 0 1),

            1 / 3 @ pkey!(0 1 true  => 0 2),
            1 / 4 @ pkey!(0 1 false => 0 2),


            1 / 6 @ pkey!(0 0 true  => 1 0),
            1 / 5 @ pkey!(0 0 false => 1 0),

            1 / 6 @ pkey!(1 0 true  => 2 0),
            1 / 5 @ pkey!(1 0 false => 2 0),


            1 /  7 @ pkey!((1 0 true,  0 1 true)  => 1 1),
            1 /  8 @ pkey!((1 0 true,  0 1 false) => 1 1),
            1 /  9 @ pkey!((1 0 false, 0 1 true)  => 1 1),
            1 / 11 @ pkey!((1 0 false, 0 1 false) => 1 1),

            2 /  7 @ pkey!((2 0 true,  1 1 true)  => 2 1),
            2 /  8 @ pkey!((2 0 true,  1 1 false) => 2 1),
            2 /  9 @ pkey!((2 0 false, 1 1 true)  => 2 1),
            2 / 11 @ pkey!((2 0 false, 1 1 false) => 2 1),

            6 /  7 @ pkey!((1 1 true,  0 2 true)  => 1 2),
            6 /  8 @ pkey!((1 1 true,  0 2 false) => 1 2),
            6 /  9 @ pkey!((1 1 false, 0 2 true)  => 1 2),
            6 / 11 @ pkey!((1 1 false, 0 2 false) => 1 2),

            3 /  7 @ pkey!((2 1  true, 1 2 true)  => 2 2),
            3 /  8 @ pkey!((2 1  true, 1 2 false) => 2 2),
            8 /  9 @ pkey!((2 1 false, 1 2 true)  => 2 2),
            9 / 11 @ pkey!((2 1 false, 1 2 false) => 2 2)
        ]
    }

    #[test]
    fn test_grid_3x3_schema() {
        use Parents::*;
        let query = b!("11");
        let probmap = make_3x3_pmap();
        let schema = GridSchema::new_from_map(3, false, &query, &probmap);
        println!("{:?}", schema);
        assert_eq!(schema.size, 3, "size");
        assert_eq!(
            schema.triu,
            HashSet::from([ix(0, 0), ix(0, 1), ix(1, 0)]),
            "triu"
        );
        assert_eq!(
            schema.tril,
            HashSet::from([ix(2, 2), ix(2, 1), ix(1, 2)]),
            "tril"
        );
        assert_eq!(
            schema.diag,
            HashSet::from([ix(0, 2), ix(1, 1), ix(2, 0)]),
            "diag"
        );

        let parents = HashMap::from([
            (ix(0, 0), Zero),
            (ix(1, 0), One(ix(0, 0), ())),
            (ix(0, 1), One(ix(0, 0), ())),
            (ix(2, 0), One(ix(1, 0), ())),
            (ix(0, 2), One(ix(0, 1), ())),
            (ix(1, 1), Two((ix(1, 0), ()), (ix(0, 1), ()))),
            (ix(2, 1), Two((ix(2, 0), ()), (ix(1, 1), ()))),
            (ix(1, 2), Two((ix(1, 1), ()), (ix(0, 2), ()))),
            (ix(2, 2), Two((ix(2, 1), ()), (ix(1, 2), ()))),
        ]);
        assert_eq!(schema.parents.len(), parents.len());
        for (k, v) in parents.iter() {
            assert!(
                schema.parents.contains_key(k),
                "expected key {k:?}, but missing from {:?}",
                schema.parents.keys()
            );
            assert_eq!(schema.parents.get(k).unwrap(), v);
        }
        assert_eq!(schema.flips.len(), probmap.len());
        for (k, v) in probmap.iter() {
            assert!(
                schema.flips.contains_key(k),
                "expected key {k:?}, but missing from {:?}",
                schema.flips.keys()
            );
            assert_eq!(schema.flips.get(k).unwrap(), v);
        }
    }

    #[test]
    #[traced_test]
    fn test_make_3x3_tril() {
        let query = b!("11");
        let probmap = make_3x3_pmap();
        let schema = GridSchema::new_from_map(3, false, &query, &probmap);

        let (prg, seen, next_diag) = make::tril(&schema);
        let seen: HashSet<Ix> = seen.into_iter().collect();
        let next_diag: HashSet<Ix> = next_diag.into_iter().collect();
        assert_eq!(seen, HashSet::from([ix(2, 2), ix(2, 1), ix(1, 2)]));
        assert_eq!(next_diag, HashSet::from([ix(0, 2), ix(1, 1), ix(2, 0)]));
        let expected = lets![
                "21" ; B!() ;=
                    ite!(( b!((  b!(@anf "11")) && (  b!(@anf "20"))) ) ? ( flip!(2/7) ) : (
                    ite!(( b!((  b!(@anf "11")) && (not!("20"))) ) ? ( flip!(2/9) ) : (
                    ite!(( b!((  not!("11")) && (  b!(@anf "20"))) ) ? ( flip!(2/8) ) : (
                                                              flip!(2/11) ))))));

                "12" ; B!() ;=
                    ite!(( b!((  b!(@anf "02")) && (  b!(@anf "11"))) ) ? ( flip!(6/7) ) : (
                    ite!(( b!((  b!(@anf "02")) && (not!("11"))) ) ? ( flip!(6/9) ) : (
                    ite!(( b!((  not!("02")) && (  b!(@anf "11"))) ) ? ( flip!(6/8) ) : (
                                                              flip!(6/11) ))))));

                "22" ; B!() ;=
                    ite!(( b!((  b!(@anf "12")) && (  b!(@anf "21"))) ) ? ( flip!(3/7) ) : (
                    ite!(( b!((  b!(@anf "12")) && (not!("21"))) ) ? ( flip!(8/9) ) : (
                    ite!(( b!((  not!("12")) && (  b!(@anf "21"))) ) ? ( flip!(3/8) ) : (
                                                              flip!(9/11) ))))));
                ...? query.clone() ; B!()
        ];
        assert_eq!(prg, expected);
    }

    #[test]
    fn test_make_3x3_diag() {
        use crate::grammar::Anf::*;
        use crate::grammar::Expr::*;
        use Parents::*;
        let query = b!("11");
        let probmap = make_3x3_pmap();
        let schema = GridSchema::new_from_map(3, false, &query, &probmap);

        let (prg, seen, next_triu) = make::diag(
            &schema,
            query.clone(),
            HashSet::from([ix(2, 2), ix(2, 1), ix(1, 2)]),
            vec![ix(0, 2), ix(1, 1), ix(2, 0)],
        );
        let seen: HashSet<Ix> = seen.into_iter().collect();
        let next_triu: HashSet<Ix> = next_triu.into_iter().collect();
        assert_eq!(
            seen,
            HashSet::from([ix(2, 2), ix(2, 1), ix(1, 2), ix(0, 2), ix(1, 1), ix(2, 0)])
        );
        assert_eq!(next_triu, HashSet::from([ix(0, 1), ix(1, 0)]));
        let expected = lets![
            "20" ; B!() ;= ite!( ( b!(@anf "10") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );

            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
                ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                          flip!(1/11) ))))));

            "02" ; B!() ;= ite!( ( b!(@anf "01") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
           ...? query.clone() ; B!()
        ];
        assert_eq!(prg, expected);
    }
    #[test]
    fn test_grid_3x3_compiles() {
        use crate::grammar::Anf::*;
        use crate::grammar::Expr::*;
        let query = b!("11");
        let probmap = make_3x3_pmap();
        let schema = GridSchema::new_from_map(3, false, &query, &probmap);
        let grid = make::grid(schema);

        let expected = program!(lets![
            "00" ; B!() ;= flip!(1/2);
            "10" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
            "01" ; B!() ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "20" ; B!() ;= ite!( ( b!(@anf "10") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );

            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
                ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                          flip!(1/11) ))))));

            "02" ; B!() ;= ite!( ( b!(@anf "01") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );

            "21" ; B!() ;=
                ite!(( b!((  b!(@anf "11")) && (  b!(@anf "20"))) ) ? ( flip!(2/7) ) : (
                ite!(( b!((  b!(@anf "11")) && (not!("20"))) ) ? ( flip!(2/9) ) : (
                ite!(( b!((  not!("11")) && (  b!(@anf "20"))) ) ? ( flip!(2/8) ) : (
                                                          flip!(2/11) ))))));

            "12" ; B!() ;=
                ite!(( b!((  b!(@anf "02")) && (  b!(@anf "11"))) ) ? ( flip!(6/7) ) : (
                ite!(( b!((  b!(@anf "02")) && (not!("11"))) ) ? ( flip!(6/9) ) : (
                ite!(( b!((  not!("02")) && (  b!(@anf "11"))) ) ? ( flip!(6/8) ) : (
                                                          flip!(6/11) ))))));

            "22" ; B!() ;=
                ite!(( b!((  b!(@anf "12")) && (  b!(@anf "21"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "12")) && (not!("21"))) ) ? ( flip!(8/9) ) : (
                ite!(( b!((  not!("12")) && (  b!(@anf "21"))) ) ? ( flip!(3/8) ) : (
                                                          flip!(9/11) ))))));
            ...? query.clone() ; B!()
        ]);

        assert_eq!(grid, expected);
    }

    /// a directed 3x3 grid test where we place samples according to various policies
    ///   (0,0) -> (0,1) -> (0,2)
    ///     v        v        v
    ///   (1,0) -> (1,1) -> (1,2)
    ///     v        v        v
    ///   (2,0) -> (2,1) -> (2,2)
    #[test]
    // #[traced_test]
    fn test_grid3x3_inference() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "00" ; B!() ;= flip!(1/2);
                "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                "02" ; B!() ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
                "20" ; B!() ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

                "11" ; B!() ;=
                    ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                    ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                    ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                              flip!(1/11) ))))));

                "21" ; B!() ;=
                    ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
                    ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
                    ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
                                                              flip!(2/11) ))))));

                "12" ; B!() ;=
                    ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
                    ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
                    ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
                                                              flip!(6/11) ))))));

                "22" ; B!() ;=
                    ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
                    ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
                    ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
                                                              flip!(9/11) ))))));
                ...? ret ; B!()
            ])
        };
        let query = b!("00", "01", "10", "02", "20", "11", "12", "21", "22");
        check_exact(
            "grid3x3/exact/manual",
            vec![
                0.500000000,
                0.291666667,
                0.183333333,
                0.274305556,
                0.193888889,
                0.102927589,
                0.599355085,
                0.199103758,
                0.770263904,
            ],
            &mk(query.clone()),
        );

        let probmap = make_3x3_pmap();
        let schema = GridSchema::new_from_map(3, false, &query, &probmap);
        let grid = make::grid(schema);
        check_exact(
            "grid3x3/all",
            vec![
                0.500000000,
                0.291666667,
                0.183333333,
                0.274305556,
                0.193888889,
                0.102927589,
                0.599355085,
                0.199103758,
                0.770263904,
            ],
            &grid,
        );
    }

    /// a directed 3x3 grid test where we place samples according to various policies
    ///   (0,0) -> (0,1) -> (0,2)
    ///     v        v        v
    ///   (1,0) -> (1,1) -> (1,2)
    ///     v        v        v
    ///   (2,0) -> (2,1) -> (2,2)
    #[test]
    // #[traced_test]
    fn test_current_grid3x3_sampled_inference() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "00" ; B!() ;= flip!(1/2);
                "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

                "20_11_02" ; b!(B, B) ;= sample!(
                    lets![
                      "20" ; B!() ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
                      "11" ; B!() ;=
                          ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                          ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                          ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                                    flip!(1/11) ))))));
                      "02" ; B!() ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                              ...? b!("20", "11", "02") ; b!(B, B, B)
                    ]);
                "20" ; B!() ;= fst!("20_11_02");
                "11" ; B!() ;= snd!("20_11_02");
                "02" ; B!() ;= thd!("20_11_02");

                "21" ; B!() ;=
                    ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
                    ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
                    ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
                                                              flip!(2/11) ))))));

                "12" ; B!() ;=
                    ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
                    ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
                    ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
                                                              flip!(6/11) ))))));

                "22" ; B!() ;=
                    ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
                    ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
                    ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
                                                              flip!(9/11) ))))));
                ...? ret ; B!()
            ])
        };
        let query = b!("00", "01", "10", "02", "20", "11", "12", "21", "22");
        check_approx(
            "grid3x3/approx/manual",
            vec![
                0.500000000,
                0.291666667,
                0.183333333,
                0.274305556,
                0.193888889,
                0.102927589,
                0.599355085,
                0.199103758,
                0.770263904,
            ],
            &mk(query.clone()),
            20000,
        );

        let probmap = make_3x3_pmap();
        let schema = GridSchema::new_from_map(3, true, &query, &probmap);
        let grid = make::grid(schema);
        check_approx(
            "grid3x3/approx/gen",
            vec![
                0.500000000,
                0.291666667,
                0.183333333,
                0.274305556,
                0.193888889,
                0.102927589,
                0.599355085,
                0.199103758,
                0.770263904,
            ],
            &grid,
            20000,
        );
    }
}
