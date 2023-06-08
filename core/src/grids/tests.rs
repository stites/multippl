use crate::grids::*;
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
pub fn full_2x2_query() -> EExprInferable {
    b!("00", "01", "10", "11")
}

pub fn make_2x2_pmap() -> HashMap<(Ix, Parents<bool>), Probability> {
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
    let schema =
        GridSchema::new_from_map(2, false, None, None, Default::default(), &query, &probmap);
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
    use crate::grammar::EExpr::*;
    use Parents::*;
    let query = b!("11");
    let probmap = make_2x2_pmap();
    let schema =
        GridSchema::new_from_map(2, false, None, None, Default::default(), &query, &probmap);

    let (prg, seen, next_diag, mut schema) = make::tril(schema);
    let seen: HashSet<Ix> = seen.into_iter().collect();
    let next_diag: HashSet<Ix> = next_diag.into_iter().collect();
    assert_eq!(seen, HashSet::from([ix(1, 1)]));
    assert_eq!(next_diag, HashSet::from([ix(0, 1), ix(1, 0)]));
    match &prg.query() {
        EAnf(_, a) => assert_eq!(*a, Box::new(b!(@anf "11"))),
        _ => assert!(false, "expected an anf statement!"),
    }
    let query = schema.query.clone();
    let flip11 =
        make::choose_with_two_parents(&mut schema, ix(1, 1), (ix(0, 1), ix(1, 0)), query.clone());
    assert_eq!(prg, flip11);
    let expected = lets![
        "11"  ;=
            ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
            ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
            ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                      flip!(1/11) ))))));
       ...? query.clone()
    ];
    assert_eq!(prg, expected);
}

#[test]
fn test_make_2x2_diag() {
    use crate::grammar::Anf::*;
    use crate::grammar::EExpr::*;
    use Parents::*;
    let query = b!("11");
    let probmap = make_2x2_pmap();
    let schema =
        GridSchema::new_from_map(2, false, None, None, Default::default(), &query, &probmap);

    let (prg, seen, next_diag, schema) = make::tril(schema);
    let (prg, seen, next_triu, schema) = make::diag(schema, prg, seen, next_diag);
    let seen: HashSet<Ix> = seen.into_iter().collect();
    let next_triu: HashSet<Ix> = next_triu.into_iter().collect();
    assert_eq!(seen, HashSet::from([ix(0, 1), ix(1, 0), ix(1, 1)]));
    assert_eq!(next_triu, HashSet::from([ix(0, 0)]));
    match &prg.query() {
        EAnf(_, a) => assert_eq!(*a, Box::new(b!(@anf "11"))),
        _ => assert!(false, "expected an anf statement!"),
    }
    let (prg, _, _, schema) = make::diag(
        schema,
        query.clone(),
        HashSet::from([ix(1, 1)]),
        vec![ix(0, 1), ix(1, 0)],
    );
    let expected = lets![
        "10"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
        "01"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
       ...? query.clone()
    ];
    assert_eq!(prg, expected);

    let mut sampled_schema = schema.clone();
    sampled_schema.sampled = true;
    let (prg, seen, next_triu, sampled_schema) = make::diag(
        sampled_schema,
        query.clone(),
        HashSet::from([ix(1, 1)]),
        vec![ix(0, 1), ix(1, 0)],
    );
    let seen: HashSet<Ix> = seen.into_iter().collect();
    let next_triu: HashSet<Ix> = next_triu.into_iter().collect();
    assert_eq!(seen, HashSet::from([ix(0, 1), ix(1, 0), ix(1, 1)]));
    assert_eq!(next_triu, HashSet::from([ix(0, 0)]));
    let expected = lets![
        "diag"  ;= sample!(lets![
            "10"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
            "01"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            ...? b!("01", "10")
            ]);
       "01"  ;= EPrj(None, 0, Box::new(b!(@anf "diag")));
       "10"  ;= EPrj(None, 1, Box::new(b!(@anf "diag")));
       ...? query.clone()
    ];
    assert_eq!(prg, expected);
}
#[test]
fn test_grid_2x2_compiles() {
    use crate::grammar::Anf::*;
    use crate::grammar::EExpr::*;
    let query = b!("11");
    let probmap = make_2x2_pmap();
    let schema =
        GridSchema::new_from_map(2, false, None, None, Default::default(), &query, &probmap);
    let grid = make::grid(schema);

    let expected = program!(lets![
        "00"  ;= flip!(1/2);
        "10"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
        "01"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
        "11"  ;=
            ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
            ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
            ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                                 flip!(1/11) ))))));
        ...? query.clone()
    ]);

    println!("{:?}", grid);
    match &grid {
        Program::EBody(ELetIn(_, var, _, _)) => assert_eq!(var, &"00".to_string()),
        _ => assert!(false, "expected a let-in binding!"),
    }
    match &grid.query() {
        EAnf(_, a) => assert_eq!(*a, Box::new(b!(@anf "11"))),
        _ => assert!(false, "expected an anf statement!"),
    }
    assert_eq!(grid, expected);
    let schema =
        GridSchema::new_from_map(2, true, None, None, Default::default(), &query, &probmap);
    let grid = make::grid(schema);

    let expected = program!(lets![
        "00"  ;= flip!(1/2);
        "diag"  ;= sample!(lets![
            "10"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
            "01"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            ...? b!("01", "10")
        ]);
        "01"  ;= EPrj(None, 0, Box::new(b!(@anf "diag")));
        "10"  ;= EPrj(None, 1, Box::new(b!(@anf "diag")));
        "11"  ;=
            ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
            ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
            ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                      flip!(1/11) ))))));
        ...? query.clone()
    ]);
    assert_eq!(grid, expected);
}

#[test]
fn test_grid2x2_inference() {
    let mk = |ret: EExprInferable| {
        Program::EBody(lets![
            "00"  ;= flip!(1/2);
            "01"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
            "11"  ;=
            ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
            ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
            ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                      flip!(1/11) ))))));
            ...? ret
        ])
    };
    crate::tests::check_exact1("grid2x2/3/00", 1.0 / 2.0, &mk(b!("00")));
    crate::tests::check_exact1("grid2x2/3/01", 0.291666667, &mk(b!("01")));
    crate::tests::check_exact1("grid2x2/3/10", 0.183333333, &mk(b!("10")));
    crate::tests::check_exact1("grid2x2/3/11", 0.102927589, &mk(b!("11")));

    let query = b!("00", "01", "10", "11");
    let probmap = make_2x2_pmap();
    let schema =
        GridSchema::new_from_map(2, false, None, None, Default::default(), &query, &probmap);
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
    let schema =
        GridSchema::new_from_map(2, true, None, None, Default::default(), &query, &probmap);
    let grid = make::grid(schema);
    check_approx(
        "grid2x2/all",
        vec![1.0 / 2.0, 0.291666667, 0.183333333, 0.102927589],
        &grid,
        10000,
    );
}
pub fn make_3x3_pmap() -> HashMap<(Ix, Parents<bool>), Probability> {
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
pub fn full_3x3_query() -> EExprInferable {
    b!("00", "01", "10", "02", "20", "11", "12", "21", "22")
}

#[test]
fn test_grid_3x3_schema() {
    use Parents::*;
    let query = b!("11");
    let probmap = make_3x3_pmap();
    let schema =
        GridSchema::new_from_map(3, false, None, None, Default::default(), &query, &probmap);
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
    let schema =
        GridSchema::new_from_map(3, false, None, None, Default::default(), &query, &probmap);

    let (prg, seen, next_diag, schema) = make::tril(schema);
    let seen: HashSet<Ix> = seen.into_iter().collect();
    let next_diag: HashSet<Ix> = next_diag.into_iter().collect();
    assert_eq!(seen, HashSet::from([ix(2, 2), ix(2, 1), ix(1, 2)]));
    assert_eq!(next_diag, HashSet::from([ix(0, 2), ix(1, 1), ix(2, 0)]));
    let expected = lets![
            "21"  ;=
                ite!(( b!((  b!(@anf "11")) && (  b!(@anf "20"))) ) ? ( flip!(2/7) ) : (
                ite!(( b!((  b!(@anf "11")) && (not!("20"))) ) ? ( flip!(2/9) ) : (
                ite!(( b!((  not!("11")) && (  b!(@anf "20"))) ) ? ( flip!(2/8) ) : (
                                                          flip!(2/11) ))))));

            "12"  ;=
                ite!(( b!((  b!(@anf "02")) && (  b!(@anf "11"))) ) ? ( flip!(6/7) ) : (
                ite!(( b!((  b!(@anf "02")) && (not!("11"))) ) ? ( flip!(6/9) ) : (
                ite!(( b!((  not!("02")) && (  b!(@anf "11"))) ) ? ( flip!(6/8) ) : (
                                                          flip!(6/11) ))))));

            "22"  ;=
                ite!(( b!((  b!(@anf "12")) && (  b!(@anf "21"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "12")) && (not!("21"))) ) ? ( flip!(8/9) ) : (
                ite!(( b!((  not!("12")) && (  b!(@anf "21"))) ) ? ( flip!(3/8) ) : (
                                                          flip!(9/11) ))))));
            ...? query.clone()
    ];
    assert_eq!(prg, expected);
}

#[test]
fn test_make_3x3_diag() {
    use crate::grammar::Anf::*;
    use crate::grammar::EExpr::*;
    use Parents::*;
    let query = b!("11");
    let probmap = make_3x3_pmap();
    let schema =
        GridSchema::new_from_map(3, false, None, None, Default::default(), &query, &probmap);

    let (prg, seen, next_triu, schema) = make::diag(
        schema,
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
        "20"  ;= ite!( ( b!(@anf "10") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );

        "11"  ;=
            ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
            ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
            ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                      flip!(1/11) ))))));

        "02"  ;= ite!( ( b!(@anf "01") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
       ...? query.clone()
    ];
    assert_eq!(prg, expected);
}
#[test]
fn test_grid_3x3_compiles() {
    use crate::grammar::Anf::*;
    use crate::grammar::EExpr::*;
    let query = b!("11");
    let probmap = make_3x3_pmap();
    let schema =
        GridSchema::new_from_map(3, false, None, None, Default::default(), &query, &probmap);
    let grid = make::grid(schema);

    let expected = program!(lets![
        "00"  ;= flip!(1/2);
        "10"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );
        "01"  ;= ite!( ( b!(@anf "00") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
        "20"  ;= ite!( ( b!(@anf "10") ) ? ( flip!(1/6) ) : ( flip!(1/5) ) );

        "11"  ;=
            ite!(( b!((  b!(@anf "01")) && (  b!(@anf "10"))) ) ? ( flip!(1/7) ) : (
            ite!(( b!((  b!(@anf "01")) && (not!("10"))) ) ? ( flip!(1/9) ) : (
            ite!(( b!((  not!("01")) && (  b!(@anf "10"))) ) ? ( flip!(1/8) ) : (
                                                      flip!(1/11) ))))));

        "02"  ;= ite!( ( b!(@anf "01") ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );

        "21"  ;=
            ite!(( b!((  b!(@anf "11")) && (  b!(@anf "20"))) ) ? ( flip!(2/7) ) : (
            ite!(( b!((  b!(@anf "11")) && (not!("20"))) ) ? ( flip!(2/9) ) : (
            ite!(( b!((  not!("11")) && (  b!(@anf "20"))) ) ? ( flip!(2/8) ) : (
                                                      flip!(2/11) ))))));

        "12"  ;=
            ite!(( b!((  b!(@anf "02")) && (  b!(@anf "11"))) ) ? ( flip!(6/7) ) : (
            ite!(( b!((  b!(@anf "02")) && (not!("11"))) ) ? ( flip!(6/9) ) : (
            ite!(( b!((  not!("02")) && (  b!(@anf "11"))) ) ? ( flip!(6/8) ) : (
                                                      flip!(6/11) ))))));

        "22"  ;=
            ite!(( b!((  b!(@anf "12")) && (  b!(@anf "21"))) ) ? ( flip!(3/7) ) : (
            ite!(( b!((  b!(@anf "12")) && (not!("21"))) ) ? ( flip!(8/9) ) : (
            ite!(( b!((  not!("12")) && (  b!(@anf "21"))) ) ? ( flip!(3/8) ) : (
                                                      flip!(9/11) ))))));
        ...? query.clone()
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
    let mk = |ret: EExprInferable| {
        Program::EBody(lets![
            "00"  ;= flip!(1/2);
            "01"  ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "02"  ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10"  ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            "20"  ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

            "11"  ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));

            "21"  ;=
                ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
                ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
                ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
                                                          flip!(2/11) ))))));

            "12"  ;=
                ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
                ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
                ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
                                                          flip!(6/11) ))))));

            "22"  ;=
                ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
                                                          flip!(9/11) ))))));
            ...? ret
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
    let schema =
        GridSchema::new_from_map(3, false, None, None, Default::default(), &query, &probmap);
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
fn test_grid3x3_sampled_inference() {
    let mk = |ret: EExprInferable| {
        Program::EBody(lets![
            "00"  ;= flip!(1/2);
            "01"  ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10"  ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

            "20_11_02" ;= sample!(
                lets![
                  "20"  ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
                  "11"  ;=
                      ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                      ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                      ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                                flip!(1/11) ))))));
                  "02"  ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                          ...? b!("20", "11", "02")
                ]);
            "20"  ;= fst!("20_11_02");
            "11"  ;= snd!("20_11_02");
            "02"  ;= thd!("20_11_02");

            "21"  ;=
                ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
                ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
                ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
                                                          flip!(2/11) ))))));

            "12"  ;=
                ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
                ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
                ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
                                                          flip!(6/11) ))))));

            "22"  ;=
                ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
                                                          flip!(9/11) ))))));
            ...? ret
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
        15000,
    );

    let probmap = make_3x3_pmap();
    let schema =
        GridSchema::new_from_map(3, true, None, None, Default::default(), &query, &probmap);
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
        15000,
    );
}
