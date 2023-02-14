#![allow(dead_code)]
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::string::String;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Bool,
    Prod(Box<Ty>, Box<Ty>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Bool(bool),
    Prod(Box<Val>, Box<Val>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum ANF {
    AVar(String, Box<Ty>), // FIXME
    AVal(Val),

    // TODO: not sure this is where I should add booleans, but it makes
    // the observe statements stay closer to the semantics: ~observe anf~
    And(Box<ANF>, Box<ANF>),
    Or(Box<ANF>, Box<ANF>),
    Neg(Box<ANF>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    EAnf(Box<ANF>),

    EFst(Box<ANF>, Box<Ty>),
    ESnd(Box<ANF>, Box<Ty>),
    EProd(Box<ANF>, Box<ANF>, Box<Ty>),

    // TODO Ignore function calls for now
    // EApp(String, Box<ANF>),
    ELetIn(String, Box<Ty>, Box<Expr>, Box<Expr>, Box<Ty>),
    EIte(Box<ANF>, Box<Expr>, Box<Expr>, Box<Ty>),
    EFlip(f64),
    EObserve(Box<ANF>),
    ESample(Box<Expr>),
}

#[derive(Default)]
pub struct Γ<'a>(pub Vec<(&'a Expr, &'a Ty)>);
impl<'a> Γ<'a> {
    pub fn get(&self, x: &'a Expr) -> Option<&'a Ty> {
        self.0
            .iter()
            .find(|(e, _)| *e == x)
            .map(|(_, t)| t)
            .copied()
    }
    pub fn append(&self, x: &'a Expr, ty: &'a Ty) -> Γ<'a> {
        let mut ctx = self.0.clone();
        ctx.push((x, ty));
        Γ(ctx)
    }
    pub fn typechecks(&self, x: &Expr, ty: &Ty) -> bool {
        // FIXME this is just a linear scan...
        self.get(x).map(|t| t == ty).is_some()
    }
    pub fn typechecks_var(&self, a: &ANF, ty: &Ty) -> bool {
        self.typechecks(&Expr::EAnf(Box::new(a.clone())), ty)
    }
}
// TODO
// structure Func where
//   name : String
//   arg : String × Ty
//   ret : Ty
//   body : Expr
// deriving Repr

impl Expr {
    pub fn is_sample(&self) -> bool {
        use Expr::*;
        match self {
            ESample(_) => true,
            _ => false,
        }
    }

    pub fn strip_samples(&self) -> Expr {
        use Expr::*;
        match self {
            ESample(e) => *e.clone(),
            ELetIn(s, tx, x, y, ty) => ELetIn(
                s.clone(),
                tx.clone(),
                Box::new(x.strip_samples()),
                Box::new(y.strip_samples()),
                ty.clone(),
            ),
            EIte(p, x, y, ty) => EIte(
                p.clone(),
                Box::new(x.strip_samples()),
                Box::new(y.strip_samples()),
                ty.clone(),
            ),
            e => e.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Program {
    Body(Expr),
    // TODO
    // | define (f: Func) (rest: Program) : Program
}
impl Program {
    fn strip_samples(&self) -> Program {
        use Program::*;
        match self {
            Body(e) => Body(e.strip_samples()),
        }
    }
}

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
        Expr::EAnf(Box::new($x))
    }};
}

#[macro_export]
macro_rules! val {
    ( $x:ident ) => {
        anf!(ANF::AVal(Val::Bool($x)))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = Box::new(Val::Bool($y));
        $(
            fin = Box::new(ANF::Prod(fin, Box::new(Val::Bool($x))));
        )+
        anf!(ANF::AVal(*fin))
    }};
}

#[macro_export]
macro_rules! var {
    ( $x:literal ) => {
        anf!(ANF::AVar($x.to_string(), Box::new(B!())))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = anf!(ANF::AVar($y.to_string()));
        $(
            fin = Box::new(Expr::EProd(fin, Box::new(anf!(ANF::AVar($x.to_string(), Box::new(B!()))))));
        )+
       *fin
    }};
}

#[macro_export]
macro_rules! b {
    ( ) => {
        Ty::Bool
    };
    ( $x:literal ) => {
        anf!(ANF::AVar($x.to_string(), Box::new(B!())))
    };
    ( T ) => {
        anf!(ANF::AVal(Val::Bool(true)))
    };
    ( F ) => {
        anf!(ANF::AVal(Val::Bool(false)))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = b!($y);
        let mut ty = B!();
        $(
            let x = match TryInto<bool>::try_into($x) {
                None => Box::new(ANF::AVar($x.to_string(), Box::new(B!()))),
                Some(_) => Box::new(ANF::AVal(Val::Bool($x)))
            };
            ty = Box::new(Ty::Prod(ty, Box::new(B!())));
            fin = Box::new(ANF::Prod(fin, x, ty));
        )+
        anf!(*fin)
    }};
    ( $y:literal && $( $x:literal )&&+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string(), Box::new(B!())));
        $(
            fin = Box::new(ANF::And(fin, Box::new(ANF::AVar($x.to_string(), Box::new(B!())))));
        )+
        anf!(*fin)
    }};
    ( $y:literal || $( $x:literal )||+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string(), Box::new(B!())));
        $(
            fin = Box::new(ANF::Or(fin, Box::new(ANF::AVar($x.to_string(), Box::new(B!())))));
        )+
        anf!(*fin)
    }};
}

#[macro_export]
macro_rules! sample {
    ( $x:expr ) => {{
        Expr::ESample(Box::new($x))
    }};
}
#[macro_export]
macro_rules! observe {
    ( $x:expr ) => {{
        if let Expr::EAnf(a) = $x {
            Expr::EObserve(a)
        } else {
            panic!("passed in a non-anf expression!");
        }
    }};
}
#[macro_export]
macro_rules! flip {
    ( $num:literal / $denom:literal) => {{
        Expr::EFlip($num as f64 / $denom as f64)
    }};
    ( $p:literal ) => {{
        Expr::EFlip($p)
    }};
}

#[macro_export]
macro_rules! lets {
    ( $var:literal : $vty:ty := $bound:expr ; in $body:expr ; $ty:ty ) => {{
        Expr::ELetIn(
            $var.to_string(),
            Box::new(typ!(TypeId::of::<$vty>())),
            Box::new($bound.clone()),
            Box::new($body.clone()),
            Box::new(typ!(TypeId::of::<$vty>())),
        )
    }};
    ( $( $var:literal : $fromty:ty := $bound:expr => $toty:ty);+ ;...? $body:expr ; $finty:ty ) => {
            {
                let mut fin = Box::new($body.clone());
                let mut bindees = vec![];
                $(
                    debug!("(let {} : {:?} = {:?})", $var.clone(), TypeId::of::<$vty>(), $bound.clone());
                    bindees.push(($var, TypeId::of::<$vty>(), $bound));
                )+
                debug!("...? {:?} ; {:?}", $body, TypeId::of::<$ty>());
                for (v, tyid, e) in bindees.iter().rev() {
                    fin = Box::new(Expr::ELetIn(v.to_string(), Box::new(typ!(tyid)), Box::new(e.clone()), fin, Box::new(typ!(tyid))));
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
    //                 debug!("(let {} : {:?} = {:?})", $var.clone(), TypeId::of::<$fromty>(), $bound.clone());
    //                 bindees.push(($var, TypeId::of::<$fromty>(), $bound));
    //                 types.push(($var, typ!(TypeId::of::<$fromty>())));
    //             )+
    //             debug!("...? {:?} ; {:?}", $body, TypeId::of::<$finty>());
    //             let mut fin = Box::new($body.clone());

    //             for (v, cotyid, e) in bindees.iter().rev() {
    //                 fin = Box::new(Expr::ELetIn(v.to_string(), Box::new(typ!(tyid)), Box::new(e.clone()), fin, fintype.clone()));
    //             }
    //             *fin
    //         }
    //     };

    ( $( $var:literal ; $fromty:expr ;= $bound:expr);+ ;...? $body:expr ; $finty:expr ) => {
            {
                let mut bindees = vec![];
                let fintype = Box::new($finty);
                $(
                    debug!("(let {} : {:?} = {:?})", $var.clone(), $fromty, $bound.clone());
                    bindees.push(($var, $fromty, $bound));
                )+
                debug!("...? {:?} ; {:?}", $body, $finty);
                let mut fin = Box::new($body.clone());

                for (v, tyid, e) in bindees.iter().rev() {
                    fin = Box::new(Expr::ELetIn(v.to_string(), Box::new(tyid.clone()), Box::new(e.clone()), fin, fintype.clone()));
                }
                *fin
            }
        };


}
// #[macro_export]
// macro_rules! ite {
//     ( if ( $pred:expr ) then { $true:expr } else { $false:expr } ) => {
//         if let Expr::EAnf(a) = $pred {
//             Expr::EIte(a, Box::new($true), Box::new($false))
//         } else {
//             panic!("passed in a non-anf expression as predicate!");
//         }
//     };
//     ( ( $pred:expr ) ? ( $true:expr ) : ( $false:expr ) ) => {
//         if let Expr::EAnf(a) = $pred {
//             Expr::EIte(a, Box::new($true), Box::new($false))
//         } else {
//             panic!("passed in a non-anf expression as predicate!");
//         }
//     };
// }

// #[macro_export]
// macro_rules! program {
//     ( $x:expr ) => {
//         Program::Body($x)
//     };
// }
// #[macro_export]
// macro_rules! run {
//     ( $( $var:literal := $bound:expr );+ ;...? $body:expr ) => {
//         program!($( $var:literal := $bound:expr );+ ;...? $body:expr)
//     };
// }
