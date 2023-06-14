// macros
#[macro_export]
macro_rules! typ {
    ($ty:ty) => {{
        if TypeId::of::<$ty>() == TypeId::of::<bool>() {
            $crate::grammar::ETy::EBool
        } else {
            todo!()
        }
    }};
    ($ty:expr) => {{
        if $ty == TypeId::of::<bool>() {
            $crate::grammar::ETy::EBool
        } else {
            todo!()
        }
    }};
}
#[macro_export]
macro_rules! B {
    () => {{
        $crate::grammar::ETy::EBool
    }};
}
#[macro_export]
macro_rules! P {
    ( $l:expr , $r:expr ) => {{
        $crate::grammar::ETy::EProd(Box::new($l), Box::new($r))
    }};
}

#[macro_export]
macro_rules! anf {
    ( $x:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EAnf((), Box::new($x))
    }};
}

#[macro_export]
macro_rules! val {
    ( $x:ident ) => {
        anf!($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVal((), $crate::grammar::EVal::EBool($x)))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = Box::new($crate::grammar::EVal::EBool($y));
        $(
            fin = Box::new($crate::grammar::Anf::EProd(fin, Box::new($crate::grammar::EVal::EBool($x))));
        )+
        anf!($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVal((), *fin))
    }};
}

#[macro_export]
macro_rules! var {
    (@ $x:expr ) => {
        $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $x.to_string())
    };
    ( $x:expr ) => {
        anf!(var!(@ $x))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = anf!($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $y.to_string()));
        $(
            fin = Box::new($crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EProd((), fin, Box::new(anf!(Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $x.to_string())))));
        )+
       *fin
    }};
}

#[macro_export]
macro_rules! b {
    (B) => {
        $crate::grammar::ETy::EBool
    };
    ( ) => {
        $crate::grammar::ETy::EBool
    };
    (@anf $x:literal ; $ty:expr) => {
        if $x.to_string() == "true" || $x.to_string() == "false" {
            $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVal((), $crate::grammar::EVal::EBool($x.to_string() == "true"))
        } else {
            $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar($ty, $x.to_string())
        }
    };
    (@anf $x:literal) => {
        b!(@anf $x ; None)
    };
    ( true ) => { anf!(b!(@anf "true")) };
    ( false ) => { anf!(b!(@anf "false")) };
    ( $x:literal ) => {
        anf!(b!(@anf $x))
    };
    ( $x:literal ; $ty:expr ) => {
        anf!(b!(@anf $x ; $ty))
    };
    ( B , B ) => {{
        $crate::grammar::ETy::EProd(vec![Ty::EBool, ETy::EBool])
    }};
    ( B , B, B ) => {{
        $crate::grammar::ETy::EProd(vec![Ty::EBool, ETy::EBool, ETy::EBool])
    }};
    ( $x:literal, $( $xs:literal ),+  ) => {{
        let mut prod = vec![b!(@anf $x)];
        $(
            prod.push(b!(@anf $xs));
        )+
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EProd(None, prod)
    }};
    ( $x:expr, $y:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EProd(None, Box::new(ty))
    }};
    ( $y:literal && $( $x:literal )&&+ ) => {{
        let mut fin = Box::new(Anf::AVar(None, $y.to_string()));
        $(
            fin = Box::new($crate::grammar::Anf::And(fin, Box::new($crate::grammar::Anf::AVar(None, $x.to_string()))));
        )+
        anf!(*fin)
    }};
    ( ($y:expr) && $( ($x:expr) )&&+ ) => {{
        let mut fin = Box::new($y);
        $(
            fin = Box::new($crate::grammar::Anf::And(fin, Box::new($x)));
        )+
        *fin
    }};
    ( $y:literal || $( $x:literal )||+ ) => {{
        let mut fin = Box::new($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $y.to_string()));
        $(
            fin = Box::new($crate::grammar::Anf::Or(fin, Box::new($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $x.to_string()))));
        )+
        anf!(*fin)
    }};
    (@anf $y:literal && $( $x:literal )&&+ ) => {{
        let mut fin = Box::new($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $y.to_string()));
        $(
            fin = Box::new($crate::grammar::Anf::And(fin, Box::new($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $x.to_string()))));
        )+
        *fin
    }};
    (@anf $y:literal || $( $x:literal )||+ ) => {{
        let mut fin = Box::new($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $y.to_string()));
        $(
            fin = Box::new($crate::grammar::Anf::Or(fin, Box::new($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $x.to_string()))));
        )+
        *fin
    }};
}

#[macro_export]
macro_rules! q {
    ( $x:literal x $y:literal ) => {{
        let prod = vec![b!(@anf $x), b!(@anf $y), b!(@anf $x || $y), b!(@anf $x && $y)];
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EProd(None, prod)
    }};
}

#[macro_export]
macro_rules! snd {
    ( $x:literal ) => {{
        snd!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EPrj(None, 1, Box::new($x))
    }};
}

pub mod discrete {
    use crate::grammar::{Anf, EExpr, ETy, EVal};
    use crate::typeinf::grammar::{EExprInferable, Inferable};
    use itertools::*;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    #[macro_export]
    macro_rules! discrete {
        ( $( $param:expr),+ ) => {{
            let mut params : Vec<f64> = vec![];
            $(
                params.push($param);
            )+
            $crate::grammar_macros::discrete::params2bindings(&params)
        }};
    }

    fn params2partitions(params: &Vec<f64>) -> Vec<f64> {
        // assumes normalized probs, just to keep this subroutine simple
        params.iter().fold(vec![1.0], |mut zs, p| {
            let z = zs.last().unwrap() - p;
            if z > 0.0 {
                zs.push(z);
            }
            zs
        })
    }
    fn normalize(params: &Vec<f64>) -> Vec<f64> {
        let z = params.iter().sum::<f64>();
        if z == 1.0 {
            params.clone()
        } else {
            params.iter().map(|p| p / z).collect()
        }
    }
    fn params2probs(params: &Vec<f64>) -> Vec<f64> {
        // put this somewhere else
        let params = normalize(params);
        let partitions = params2partitions(&params);
        params
            .into_iter()
            .zip(partitions.into_iter())
            .map(|(phat, z)| phat / z)
            .collect()
    }
    fn hash_discrete(params: &Vec<f64>) -> String {
        let mut hasher = DefaultHasher::new();
        let s = params.iter().map(|p| format!("{}", p)).join("-");
        s.hash(&mut hasher);
        (&format!("{:x}", hasher.finish())[..6]).to_string()
    }

    fn mkvar(v: String) -> Anf<Inferable, EVal> {
        Anf::AVar(Some(ETy::EBool), v)
    }

    fn mkguard(ids: Vec<String>) -> Anf<Inferable, EVal> {
        let mut var = None;
        for id in ids {
            let nxt = match var {
                None => Anf::Neg(Box::new(mkvar(id))),
                Some(prev) => Anf::And(Box::new(prev), Box::new(Anf::Neg(Box::new(mkvar(id))))),
            };
            var = Some(nxt);
        }
        var.unwrap()
    }

    fn mkdiscrete_var(flip_id: String, seen_ids: Vec<String>) -> Anf<Inferable, EVal> {
        let flip = mkvar(flip_id);
        if seen_ids.len() == 0 {
            flip
        } else {
            let seen = mkguard(seen_ids);
            Anf::And(Box::new(seen), Box::new(flip))
        }
    }

    fn params2statements(params: &Vec<f64>) -> Vec<(String, EExprInferable)> {
        let discrete_id = hash_discrete(params);
        let probs = params2probs(params);

        let mut flips: Vec<(String, usize, EExprInferable)> = probs
            .iter()
            .enumerate()
            .map(|(ix, p)| (format!("f{}_{}", discrete_id, ix), ix, EExpr::EFlip((), *p)))
            .collect_vec();
        flips.pop();

        let mut vars: Vec<(String, EExprInferable)> = vec![];
        let mut seen: Vec<String> = vec![];

        for (flip_lbl, ix, flip) in flips.into_iter() {
            let var_lbl = format!("v{}_{}", discrete_id, ix);
            let var_exp = mkdiscrete_var(flip_lbl.clone(), seen.clone());
            vars.push((flip_lbl.clone(), flip));
            vars.push((var_lbl.clone(), EExpr::EAnf((), Box::new(var_exp))));
            seen.push(var_lbl);
        }
        vars.push((
            format!("v{}_{}", discrete_id, "last"),
            EExpr::EAnf((), Box::new(mkguard(seen))),
        ));
        vars
    }

    pub fn params2bindings(params: &Vec<f64>) -> EExprInferable {
        let mut statements = params2statements(params);
        let mut binding = statements.last().unwrap().1.clone();
        statements.pop();
        let thetype = Some(crate::typecheck::grammar::LetInTypes {
            bindee: ETy::EBool,
            body: ETy::EProd(vec![ETy::EBool; params.len()]),
        });
        for (label, bindee) in statements.into_iter().rev() {
            binding = EExpr::ELetIn(thetype.clone(), label, Box::new(bindee), Box::new(binding));
        }
        binding
    }
    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        pub fn test_params2partitions() {
            let params = vec![0.1, 0.4, 0.5];
            let partitions = params2partitions(&params);
            assert_eq!(partitions, vec![1.0, 0.9, 0.5]);
            let params = vec![0.1, 0.4, 0.2, 0.3];
            let partitions = params2partitions(&params);
            assert_eq!(partitions, vec![1.0, 0.9, 0.5, 0.3]);
        }
        #[test]
        pub fn test_params2probs() {
            let params = vec![0.1, 0.4, 0.5];
            let probs = params2probs(&params);
            assert_eq!(probs, vec![0.1, 0.4 / 0.9, 1.0]);
            let params = vec![0.1, 0.4, 0.2, 0.3];
            let probs = params2probs(&params);
            assert_eq!(probs, vec![0.1, 0.4 / 0.9, 0.2 / 0.5, 1.0]);
            let params = vec![1.0, 4.0, 2.0, 3.0];
            let probs = params2probs(&params);
            assert_eq!(probs, vec![0.1, 0.4 / 0.9, 0.2 / 0.5, 1.0]);
        }
        #[test]
        pub fn test_params2hash() {
            let params = vec![0.1, 0.4, 0.5];
            let probs = hash_discrete(&params);
            assert_eq!(probs, "22f7f9");
            let params = vec![0.1, 0.4, 0.2, 0.3];
            let probs = hash_discrete(&params);
            assert_eq!(probs, "1501f9");
            let params = vec![1.0, 4.0, 2.0, 3.0];
            let probs = hash_discrete(&params);
            assert_eq!(probs, "4b912b");
        }
        #[test]
        pub fn test_mkguard_for_discrete_var() {
            let bindee = mkguard(vec![String::from("seen1"), String::from("seen2")]);
            let seen1 = Anf::Neg(Box::new(Anf::AVar(Some(ETy::EBool), String::from("seen1"))));
            let seen2 = Anf::Neg(Box::new(Anf::AVar(Some(ETy::EBool), String::from("seen2"))));
            assert_eq!(
                bindee,
                Anf::And(Box::new(seen1.clone()), Box::new(seen2.clone()),)
            );
            let bindee = mkguard(vec![
                String::from("seen1"),
                String::from("seen2"),
                String::from("seen3"),
            ]);
            let seen3 = Anf::Neg(Box::new(Anf::AVar(Some(ETy::EBool), String::from("seen3"))));
            assert_eq!(
                bindee,
                Anf::And(
                    Box::new(Anf::And(Box::new(seen1), Box::new(seen2))),
                    Box::new(seen3),
                )
            );
        }
        #[test]
        pub fn test_params2statements() {
            let params = vec![0.1, 0.4, 0.5];
            let mut statements = params2statements(&params);
            for (s, e) in statements.iter() {
                println!("let {} := {:?}", s, e);
            }
            statements.pop();
            let last_flip = statements.last().unwrap();
            // assert!(false);
        }
        #[test]
        pub fn test_params2bindings() {
            let params = vec![0.1, 0.4, 0.5];
            let bindings = params2bindings(&params);
            println!("{:?}", bindings);
            // assert!(false);
        }
    }
}

#[macro_export]
macro_rules! not {
    (@ $x:expr ) => {{
        let x = $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVar(None, $x.to_string());
        $crate::grammar::Anf::Neg(Box::new(x))
    }};
    ( $x:expr ) => {{
        $crate::grammar::Anf::Neg(Box::new(b!(@anf $x)))
    }};
}

#[macro_export]
macro_rules! prj {
    ( $i:literal, $x:literal ) => {{
        prj!($i, b!(@anf $x))
    }};
    ( $i:literal, $x:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EPrj(None, $i, Box::new($x))
    }};
}
#[macro_export]
macro_rules! fst {
    ( $x:literal ) => {{
        fst!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EPrj(None, 0, Box::new($x))
    }};
}
#[macro_export]
macro_rules! thd {
    ( $x:literal ) => {{
        thd!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EPrj(None, 2, Box::new($x))
    }};
}
#[macro_export]
macro_rules! sample {
    ( $x:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::ESample(
            (),
            Box::new(
                $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SExact(
                    (),
                    Box::new($x),
                ),
            ),
        )
    }};
}
#[macro_export]
macro_rules! observe {
    ( $x:expr ) => {{
        if let EExpr::<$crate::typeinf::grammar::Inferable>::EAnf(_, a) = $x {
            $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EObserve((), a)
        } else {
            panic!("passed in a non-anf expression!");
        }
    }};
}
#[macro_export]
macro_rules! flip {
    ( $num:literal / $denom:literal) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EFlip(
            (),
            $num as f64 / $denom as f64,
        )
    }};
    ( $p:literal ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EFlip((), $p)
    }};
    (@ $p:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EFlip((), $p)
    }};
}

#[macro_export]
macro_rules! lets {
    ( $var:literal ;= $bound:expr ; in $body:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::ELetIn(None,
            $var.to_string(),
            Box::new($bound.clone()),
            Box::new($body.clone()),
        )
    }};
    ( $( $var:literal ;= $bound:expr);+ ;...? $body:expr ) => {
            {
                let mut fin = Box::new($body.clone());
                let mut bindees = vec![];
                $(
                    tracing::debug!("(let {} = {:?})", $var.clone(), $bound.clone());
                    bindees.push(($var, $bound));
                )+
                tracing::debug!("...? {:?}", $body);
                for (v, e) in bindees.iter().rev() {
                    fin = Box::new(EExpr::<$crate::typeinf::grammar::Inferable>::ELetIn(None, v.to_string(), Box::new(e.clone()), fin));
                }
                *fin
            }
        };

    ( $( $var:literal := $bound:expr);+ ;...? $body:expr ; $finty:ty ) => {
            {
                let mut fin = Box::new($body.clone());
                let mut bindees = vec![];
                $(
                    tracing::debug!("(let {} = {:?})", $var.clone(), $bound.clone());
                    bindees.push(($var, $bound));
                )+
                tracing::debug!("...? {:?} ; {:?}", $body, TypeId::of::<$ty>());
                for (v, tyid, e) in bindees.iter().rev() {
                    fin = Box::new(EExpr::<$crate::typeinf::grammar::Inferable>::ELetIn(None, v.to_string(), Box::new(e.clone()), fin));
                }
                *fin
            }
        };
    // ( $( $var:literal : $fromty:ty := $bound:expr);+ ;...? $body:expr ; $finty:ty ) => {
    //         {
    //             let mut bindees = vec![];
    //             let mut types = vec![];
    //             let fintype = Box::new(typ!($finty));
    //             $(
    //                 tracing::debug!("(let {} : {:?} = {:?})", $var.clone(), TypeId::of::<$fromty>(), $bound.clone());
    //                 bindees.push(($var, TypeId::of::<$fromty>(), $bound));
    //                 types.push(($var, typ!(TypeId::of::<$fromty>())));
    //             )+
    //             tracing::debug!("...? {:?} ; {:?}", $body, TypeId::of::<$finty>());
    //             let mut fin = Box::new($body.clone());

    //             for (v, cotyid, e) in bindees.iter().rev() {
    //                 fin = Box::new(EExpr::<$crate::typeinf::grammar::Inferable>::ELetIn(v.to_string(), Box::new(typ!(tyid)), Box::new(e.clone()), fin, fintype.clone()));
    //             }
    //             *fin
    //         }
    //     };

    ( $( $var:literal ; $fromty:expr ;= $bound:expr);+ ;...? $body:expr ; $finty:expr ) => {
            {
                let mut bindees = vec![];
                let fintype = Box::new($finty);
                $(
                    tracing::debug!("(let {} : {:?} = {:?})", $var.clone(), $fromty, $bound.clone());
                    bindees.push(($var, $fromty, $bound));
                )+
                tracing::debug!("...? {:?} ; {:?}", $body, $finty);
                let mut fin = Box::new($body.clone());

                for (v, tyid, e) in bindees.iter().rev() {
                    fin = Box::new(EExpr::<$crate::typeinf::grammar::Inferable>::ELetIn(None, v.to_string(), Box::new(e.clone()), fin));
                }
                *fin
            }
        };

}
#[macro_export]
macro_rules! ite {
    ( if ( $pred:expr ) then { $true:expr } else { $false:expr } ) => {
        if let $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EAnf((), a) = $pred {
            $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EIte(
                None,
                a,
                Box::new($true),
                Box::new($false),
            )
        } else {
            panic!("passed in a non-anf expression as predicate!");
        }
    };
    ( ( $pred:expr ) ? ( $true:expr ) : ( $false:expr ) ) => {
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::EIte(
            None,
            Box::new($pred),
            Box::new($true),
            Box::new($false),
        )

        // if let EExpr::<$crate::typeinf::grammar::Inferable>::EAnf(a) = $pred {
        //     EExpr::<$crate::typeinf::grammar::Inferable>::EIte($pred, Box::new($true), Box::new($false), Box::new(None))
        // } else {
        //     panic!("passed in a non-anf expression as predicate!");
        // }
    };
}

#[macro_export]
macro_rules! program {
    ( $x:expr ) => {
        $crate::grammar::Program::EBody($x)
    };
}
// #[macro_export]
// macro_rules! run {
//     ( $( $var:literal := $bound:expr );+ ;...? $body:expr ) => {
//         program!($( $var:literal := $bound:expr );+ ;...? $body:expr)
//     };
// }
