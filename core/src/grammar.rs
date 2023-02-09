#![allow(dead_code)]
use std::string::String;

#[derive(Debug, Copy, Clone)]
pub enum Ty {
    Bool,
    // Prod(Box<Ty>, Box<Ty>), // TODO for now, no tuples
}
#[derive(Debug, Copy, Clone)]
pub enum Val {
    Bool(bool),
    // Prod(Box<Val>, Box<Val>), // TODO punt
}
#[derive(Debug, Clone)]
pub enum ANF {
    AVar(String),
    AVal(Val),
    // TODO: not sure this is where I should add booleans, but it makes
    // the observe statements stay closer to the semantics: ~observe anf~
    And(Box<ANF>, Box<ANF>),
    Or(Box<ANF>, Box<ANF>),
    Neg(Box<ANF>),
}
#[derive(Debug, Clone)]
pub enum Expr {
    EAnf(Box<ANF>),
    // TODO Ignore product types for now:
    // EFst (Box<ANF>),
    // ESnd (Box<ANF>),
    // EProd (Box<ANF>, Box<ANF>),

    // TODO Ignore function calls for now
    // EApp(String, Box<ANF>),
    ELetIn(String, Box<Expr>, Box<Expr>),
    EIte(Box<ANF>, Box<Expr>, Box<Expr>),
    EFlip(f64),
    EObserve(Box<ANF>),
    ESample(Box<Expr>),
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
            ELetIn(s, x, y) => ELetIn(
                s.clone(),
                Box::new(x.strip_samples()),
                Box::new(y.strip_samples()),
            ),
            EIte(p, x, y) => EIte(
                p.clone(),
                Box::new(x.strip_samples()),
                Box::new(y.strip_samples()),
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
}
#[macro_export]
macro_rules! var {
    ( $x:literal ) => {
        anf!(ANF::AVar($x.to_string()))
    };
}
#[macro_export]
macro_rules! or {
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string()));
        $(
            fin = Box::new(ANF::Or(fin, Box::new(ANF::AVar($x.to_string()))));
        )+
        anf!(*fin)
    }};
}
#[macro_export]
macro_rules! and {
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string()));
        $(
            fin = Box::new(ANF::And(fin, Box::new(ANF::AVar($x.to_string()))));
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
}
#[macro_export]
macro_rules! lets {
    ( $( $var:literal := $bound:expr );+ ;...? $body:expr ) => {
        {
            let mut fin = Box::new($body.clone());
            let mut bindees = vec![];
            $(
                debug!("let {} = {:?};", $var.clone(), $bound.clone());
                bindees.push(($var, $bound));
            )+
            debug!("...? {:?}", $body);
            for (v, e) in bindees.iter().rev() {
                fin = Box::new(Expr::ELetIn(v.to_string(), Box::new(e.clone()), fin));

            }
            *fin
        }
    };
}
#[macro_export]
macro_rules! ite {
    ( if ( $pred:expr ) then { $true:expr } else { $false:expr } ) => {
        if let Expr::EAnf(a) = $pred {
            Expr::EIte(a, Box::new($true), Box::new($false))
        } else {
            panic!("passed in a non-anf expression as predicate!");
        }
    };
    ( ( $pred:expr ) ? ( $true:expr ) : ( $false:expr ) ) => {
        if let Expr::EAnf(a) = $pred {
            Expr::EIte(a, Box::new($true), Box::new($false))
        } else {
            panic!("passed in a non-anf expression as predicate!");
        }
    };
}

#[macro_export]
macro_rules! program {
    ( $x:expr ) => {
        Program::Body($x)
    };
}
#[macro_export]
macro_rules! run {
    ( $( $var:literal := $bound:expr );+ ;...? $body:expr ) => {
        program!($( $var:literal := $bound:expr );+ ;...? $body:expr)
    };
}
