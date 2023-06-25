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
    (~ $x:expr ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SAnf((), Box::new($x))
    }};
}

#[macro_export]
macro_rules! val {
    ( $x:ident ) => {
        anf!($crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, EVal>::AVal((), $crate::grammar::EVal::EBool($x)))
    };
    (~ $x:ident ) => {
        anf!(~ $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, SVal>::AVal((), $crate::grammar::SVal::SBool($x)))
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
    (~ $x:expr ) => {
        anf!(~ var!(~@ $x))
    };
    (~@ $x:expr ) => {
        $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, SVal>::AVar(None, $x.to_string())
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

    (~@anf $x:literal) => {
        if $x.to_string() == "true" || $x.to_string() == "false" {
            $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, SVal>::AVal((), $crate::grammar::SVal::SBool($x.to_string() == "true"))
        } else {
            $crate::grammar::Anf::<$crate::typeinf::grammar::Inferable, SVal>::AVar(None, $x.to_string())
        }
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

#[macro_export]
macro_rules! discrete {
        ( $( $param:expr),+ ) => {{
            let mut params : Vec<f64> = vec![];
            $(
                params.push($param);
            )+
            $crate::grammar::discrete::from_params(&params)
        }};
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
    (~ $x:expr ) => {{
        $crate::grammar::EExpr::<$crate::typeinf::grammar::Inferable>::ESample((), Box::new($x))
    }};
}
#[macro_export]
macro_rules! exact {
    ( $x:expr ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SExact((), Box::new($x))
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
macro_rules! bern {
    ( $num:literal / $denom:literal) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SBern(
            (),
            $num as f64 / $denom as f64,
        )
    }};
    ( $p:expr  ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SBern((), $p)
    }};
}
#[macro_export]
macro_rules! categorical {
    ( $vecs:expr ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SDiscrete((), $vecs)
    }};
}
#[macro_export]
macro_rules! uniform {
    ( $l:expr , $h:expr ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SUniform((), $l, $h)
    }};
}
#[macro_export]
macro_rules! normal {
    ( $l:expr , $h:expr ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SNormal((), $l, $h)
    }};
}
#[macro_export]
macro_rules! beta {
    ( $l:expr , $h:expr ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SBeta((), $l, $h)
    }};
}
#[macro_export]
macro_rules! dirichlet {
    ( $vecs:expr ) => {{
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SDirichlet((), $vecs)
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
    ( $( $var:literal ;= $bound:expr );+ ;...? $body:expr ) => {
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

    ( $(~ $var:literal <- $bound:expr ; )+ ~~ $body:expr   ) => {{
        tracing::debug!("do");
        let mut fin = Box::new($body.clone());
        let mut bindees = vec![];
        $(
            tracing::debug!("{} <- {:?}", $var.clone(), $bound.clone());
            bindees.push(($var, $bound));
        )+
        tracing::debug!("{:?}", $body);
        for (v, e) in bindees.iter().rev() {
            fin = Box::new(SExpr::<$crate::typeinf::grammar::Inferable>::SLetIn(None, v.to_string(), Box::new(e.clone()), fin));
        }
        *fin
    }};


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
    };
    (~ ( $pred:expr ) ? ( $true:expr ) : ( $false:expr ) ) => {
        $crate::grammar::SExpr::<$crate::typeinf::grammar::Inferable>::SIte(
            None,
            Box::new($pred),
            Box::new($true),
            Box::new($false),
        )
    };
}

#[macro_export]
macro_rules! program {
    ( $x:expr ) => {
        $crate::grammar::Program::EBody($x)
    };
    (~ $x:expr ) => {
        $crate::grammar::Program::SBody($x)
    };
}
// #[macro_export]
// macro_rules! run {
//     ( $( $var:literal := $bound:expr );+ ;...? $body:expr ) => {
//         program!($( $var:literal := $bound:expr );+ ;...? $body:expr)
//     };
// }
