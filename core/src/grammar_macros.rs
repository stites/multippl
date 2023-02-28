use crate::grammar::*;

// macros
#[macro_export]
macro_rules! typ {
    ($ty:ty) => {{
        if TypeId::of::<$ty>() == TypeId::of::<bool>() {
            Ty::Bool
        } else {
            todo!()
        }
    }};
    ($ty:expr) => {{
        if $ty == TypeId::of::<bool>() {
            Ty::Bool
        } else {
            todo!()
        }
    }};
}
#[macro_export]
macro_rules! B {
    () => {{
        Ty::Bool
    }};
}
#[macro_export]
macro_rules! P {
    ( $l:expr , $r:expr ) => {{
        Ty::Prod(Box::new($l), Box::new($r))
    }};
}

#[macro_export]
macro_rules! anf {
    ( $x:expr ) => {{
        Expr::<$crate::typecheck::grammar::Typed>::EAnf((), Box::new($x))
    }};
}

#[macro_export]
macro_rules! val {
    ( $x:ident ) => {
        anf!(Anf::<$crate::typecheck::grammar::Typed>::AVal((), Val::Bool($x)))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = Box::new(Val::Bool($y));
        $(
            fin = Box::new(Anf::Prod(fin, Box::new(Val::Bool($x))));
        )+
        anf!(Anf::<$crate::typecheck::grammar::Typed>::AVal((), *fin))
    }};
}

#[macro_export]
macro_rules! var {
    ( $x:literal ) => {
        anf!(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $x.to_string()))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = anf!(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $y.to_string()));
        $(
            fin = Box::new(Expr::<$crate::typecheck::grammar::Typed>::EProd((), fin, Box::new(anf!(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $x.to_string())))));
        )+
       *fin
    }};
}

#[macro_export]
macro_rules! b {
    (B) => {
        Ty::Bool
    };
    ( ) => {
        Ty::Bool
    };
    (@anf $x:literal ; $ty:expr) => {
        if $x.to_string() == "true" || $x.to_string() == "false" {
            Anf::<$crate::typecheck::grammar::Typed>::AVal((), Val::Bool($x.to_string() == "true"))
        } else {
            Anf::<$crate::typecheck::grammar::Typed>::AVar($ty, $x.to_string())
        }
    };
    (@anf $x:literal) => {
        b!(@anf $x ; B!())
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
        Ty::Prod(vec![b!(), b!()])
    }};
    ( B , B, B ) => {{
        Ty::Prod(vec![b!(), b!(), b!()])
    }};
    ( $x:literal, $( $xs:literal ),+  ) => {{
        let mut prod = vec![b!(@anf $x)];
        let mut typ = vec![b!()];
        $(
            prod.push(b!(@anf $xs));
            typ.push(b!());
        )+
        Expr::<$crate::typecheck::grammar::Typed>::EProd(Ty::Prod(typ), prod)
    }};
    ( $x:expr, $y:expr ) => {{
        let ty = Ty::Prod(vec![b!(), b!()]);
        Expr::<$crate::typecheck::grammar::Typed>::EProd(vec![$x, $y], Box::new(ty))
    }};
    ( $y:literal && $( $x:literal )&&+ ) => {{
        let mut fin = Box::new(Anf::AVar(B!(), $y.to_string()));
        $(
            fin = Box::new(Anf::And(fin, Box::new(Anf::AVar(B!(), $x.to_string()))));
        )+
        anf!(*fin)
    }};
    ( ($y:expr) && $( ($x:expr) )&&+ ) => {{
        let mut fin = Box::new($y);
        $(
            fin = Box::new(Anf::And(fin, Box::new($x)));
        )+
        *fin
    }};
    ( $y:literal || $( $x:literal )||+ ) => {{
        let mut fin = Box::new(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $y.to_string()));
        $(
            fin = Box::new(Anf::Or(fin, Box::new(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $x.to_string()))));
        )+
        anf!(*fin)
    }};
    (@anf $y:literal && $( $x:literal )&&+ ) => {{
        let mut fin = Box::new(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $y.to_string()));
        $(
            fin = Box::new(Anf::And(fin, Box::new(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $x.to_string()))));
        )+
        *fin
    }};
    (@anf $y:literal || $( $x:literal )||+ ) => {{
        let mut fin = Box::new(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $y.to_string()));
        $(
            fin = Box::new(Anf::Or(fin, Box::new(Anf::<$crate::typecheck::grammar::Typed>::AVar(B!(), $x.to_string()))));
        )+
        *fin
    }};
}

#[macro_export]
macro_rules! q {
    ( $x:literal x $y:literal ) => {{
        let typ = vec![b!(); 4];
        let prod = vec![b!(@anf $x), b!(@anf $y), b!(@anf $x || $y), b!(@anf $x && $y)];
        Expr::<$crate::typecheck::grammar::Typed>::EProd(Ty::Prod(typ), prod)
    }};
}

#[macro_export]
macro_rules! snd {
    ( $x:literal ) => {{
        snd!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        Expr::<$crate::typecheck::grammar::Typed>::ESnd(b!(), Box::new($x))
    }};
}

#[macro_export]
macro_rules! not {
    ( $x:literal ) => {{
        Anf::Neg(Box::new(b!(@anf $x)))
    }};
}

#[macro_export]
macro_rules! fst {
    ( $x:literal ) => {{
        fst!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        Expr::<$crate::typecheck::grammar::Typed>::EFst(b!(), Box::new($x))
    }};
}
#[macro_export]
macro_rules! thd {
    ( $x:literal ) => {{
        thd!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        Expr::<$crate::typecheck::grammar::Typed>::EPrj(b!(), 2, Box::new($x))
    }};
}
#[macro_export]
macro_rules! sample {
    ( $x:expr ) => {{
        Expr::<$crate::typecheck::grammar::Typed>::ESample((), Box::new($x))
    }};
}
#[macro_export]
macro_rules! observe {
    ( $x:expr ) => {{
        if let Expr::<$crate::typecheck::grammar::Typed>::EAnf(_, a) = $x {
            Expr::<$crate::typecheck::grammar::Typed>::EObserve((), a)
        } else {
            panic!("passed in a non-anf expression!");
        }
    }};
}
#[macro_export]
macro_rules! flip {
    ( $num:literal / $denom:literal) => {{
        Expr::<$crate::typecheck::grammar::Typed>::EFlip((), $num as f64 / $denom as f64)
    }};
    ( $p:literal ) => {{
        Expr::<$crate::typecheck::grammar::Typed>::EFlip($p)
    }};
}

#[macro_export]
macro_rules! lets {
    ( $var:literal : $vty:ty := $bound:expr ; in $body:expr ; $ty:ty ) => {{
        Expr::<$crate::typecheck::grammar::Typed>::ELetIn($crate::typecheck::grammar::LetInTypes {
            bindee: typ!(TypeId::of::<$vty>()),
            body: typ!(TypeId::of::<$vty>()),
        },
            $var.to_string(),
            Box::new($bound.clone()),
            Box::new($body.clone()),
        )
    }};
    ( $( $var:literal : $fromty:ty := $bound:expr => $toty:ty);+ ;...? $body:expr ; $finty:ty ) => {
            {
                let mut fin = Box::new($body.clone());
                let mut bindees = vec![];
                $(
                    tracing::debug!("(let {} : {:?} = {:?})", $var.clone(), TypeId::of::<$vty>(), $bound.clone());
                    bindees.push(($var, TypeId::of::<$vty>(), $bound));
                )+
                tracing::debug!("...? {:?} ; {:?}", $body, TypeId::of::<$ty>());
                for (v, tyid, e) in bindees.iter().rev() {
                     let types = $crate::typecheck::grammar::LetInTypes { bindee: typ!(tyid), body: typ!(tyid)};
                    fin = Box::new(Expr::<$crate::typecheck::grammar::Typed>::ELetIn(types, v.to_string(), Box::new(e.clone()), fin));
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
    //                 fin = Box::new(Expr::<$crate::typecheck::grammar::Typed>::ELetIn(v.to_string(), Box::new(typ!(tyid)), Box::new(e.clone()), fin, fintype.clone()));
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
                     let types = $crate::typecheck::grammar::LetInTypes { bindee: tyid.clone(), body: *fintype.clone()};
                    fin = Box::new(Expr::<$crate::typecheck::grammar::Typed>::ELetIn(types, v.to_string(), Box::new(e.clone()), fin));
                }
                *fin
            }
        };

}
#[macro_export]
macro_rules! ite {
    ( if ( $pred:expr ) then { $true:expr } else { $false:expr } ) => {
        if let Expr::<$crate::typecheck::grammar::Typed>::EAnf((), a) = $pred {
            Expr::<$crate::typecheck::grammar::Typed>::EIte(
                b!(),
                a,
                Box::new($true),
                Box::new($false),
            )
        } else {
            panic!("passed in a non-anf expression as predicate!");
        }
    };
    ( ( $pred:expr ) ? ( $true:expr ) : ( $false:expr ) ) => {
        Expr::<$crate::typecheck::grammar::Typed>::EIte(
            b!(),
            Box::new($pred),
            Box::new($true),
            Box::new($false),
        )

        // if let Expr::<$crate::typecheck::grammar::Typed>::EAnf(a) = $pred {
        //     Expr::<$crate::typecheck::grammar::Typed>::EIte($pred, Box::new($true), Box::new($false), Box::new(b!()))
        // } else {
        //     panic!("passed in a non-anf expression as predicate!");
        // }
    };
}

#[macro_export]
macro_rules! program {
    ( $x:expr ) => {
        Program::Body($x)
    };
}
// #[macro_export]
// macro_rules! run {
//     ( $( $var:literal := $bound:expr );+ ;...? $body:expr ) => {
//         program!($( $var:literal := $bound:expr );+ ;...? $body:expr)
//     };
// }
