use crate::grammar::*;
use itertools::*;
use quickcheck::*;
use std::fmt::Debug;

impl Arbitrary for EVal {
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[None, Some(true), Some(false), Some(true), Some(false)]);
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(Some(b)) => EVal::EBool(*b),
            Some(None) => {
                // let arb = u8::arbitrary(g);
                // let len = (arb % 3) + 2; // only generate between 2-4 tuples
                // EVal::EProd((0..len).map(|_i| Self::arbitrary(g)).collect_vec())
                // ...actually, just work with 2-tuples for now.
                EVal::EProd((0..2).map(|_i| Self::arbitrary(g)).collect_vec())
            }
        }
    }
}

impl<X: 'static> Arbitrary for Anf<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Arbitrary,
    <AValExt as ξ<X>>::Ext: Arbitrary,
{
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[0, 1, 2_u8]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => Anf::<X>::AVar(Arbitrary::arbitrary(g), String::arbitrary(g)),
            Some(1) => Anf::AVal(Arbitrary::arbitrary(g), EVal::arbitrary(g)),
            Some(2) => {
                let x = g.choose(&[0, 1, 2_u8]);
                match x {
                    None => panic!("impossible: choose vec has len > 0"),
                    Some(0) => Anf::And(Box::<Anf<X>>::arbitrary(g), Box::<Anf<X>>::arbitrary(g)),
                    Some(1) => Anf::Or(Box::<Anf<X>>::arbitrary(g), Box::<Anf<X>>::arbitrary(g)),
                    Some(2) => Anf::Neg(Box::<Anf<X>>::arbitrary(g)),
                    _ => panic!("impossible"),
                }
            }
            _ => panic!("impossible"),
        }
    }
}

impl<X: 'static> Arbitrary for EExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Arbitrary,
    <EPrjExt as ξ<X>>::Ext: Arbitrary,
    <EProdExt as ξ<X>>::Ext: Arbitrary,
    <ELetInExt as ξ<X>>::Ext: Arbitrary,
    <EIteExt as ξ<X>>::Ext: Arbitrary,
    <EFlipExt as ξ<X>>::Ext: Arbitrary,
    <EObserveExt as ξ<X>>::Ext: Arbitrary,
    <ESampleExt as ξ<X>>::Ext: Arbitrary,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Arbitrary,
    <AValExt as ξ<X>>::Ext: Arbitrary,
{
    fn arbitrary(g: &mut Gen) -> EExpr<X> {
        let x = g.choose(&[0, 1, 2, 3, 4, 5, 6, 7]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => EExpr::EAnf(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            Some(1) => EExpr::EProd(
                Arbitrary::arbitrary(g),
                (0..2).map(|_i| Anf::arbitrary(g)).collect_vec(),
            ),
            Some(2) => {
                let var = String::arbitrary(g);
                let bind = Arbitrary::arbitrary(g);
                let body = Arbitrary::arbitrary(g);
                EExpr::ELetIn(Arbitrary::arbitrary(g), var, bind, body)
            }
            Some(3) => {
                let p = Arbitrary::arbitrary(g);
                let t = Arbitrary::arbitrary(g);
                let f = Arbitrary::arbitrary(g);
                EExpr::EIte(Arbitrary::arbitrary(g), p, t, f)
            }
            Some(4) => {
                let r = u8::arbitrary(g); // 256
                EExpr::EFlip(
                    Arbitrary::arbitrary(g),
                    <u8 as Into<f64>>::into(r) / <u8 as Into<f64>>::into(u8::MAX),
                )
            }
            Some(5) => EExpr::EObserve(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            Some(6) => EExpr::ESample(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            _ => panic!("impossible"),
        }
    }
}
impl<X> Arbitrary for Program<X>
where
    X: Clone + 'static,
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <EPrjExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <EProdExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <ELetInExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <EIteExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <EFlipExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <EObserveExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <ESampleExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
    <AValExt as ξ<X>>::Ext: Arbitrary + Debug + Clone + PartialEq,
{
    fn arbitrary(g: &mut Gen) -> Program<X> {
        Program::Body(EExpr::arbitrary(g))
    }
}
