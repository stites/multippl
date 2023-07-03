#![allow(non_camel_case_types)]
use core::marker::PhantomData;

pub trait ξ<X> {
    type Ext;
}
pub struct EAnfExt;
pub struct EPrjExt;
pub struct EProdExt;
pub struct ELetInExt;
pub struct EIteExt;
pub struct EAppExt;
pub struct EFlipExt;
pub struct EObserveExt;
pub struct ESampleExt;
pub struct AVarExt<Val> {
    vartype: PhantomData<Val>,
}
pub struct AValExt<Val> {
    valtype: PhantomData<Val>,
}

pub struct SAnfExt;
pub struct SLetInExt;
pub struct SSeqExt;
pub struct SIteExt;
pub struct SReturnExt;
pub struct SMapExt;
pub struct SFoldExt;

pub struct SAppExt;
pub struct SLambdaExt;

pub struct SObserveExt;
pub struct SExactExt;
pub struct SSampleExt;

// sugar
pub struct SLetSampleExt;
pub struct EDiscreteExt;
pub struct EIterateExt;

#[macro_export]
macro_rules! TTG {
    (#[derive(Debug, Clone, PartialEq)] pub enum $name:ident<X> { $($body:tt)* }) => {
        #[allow(clippy::enum_variant_names)]
        #[derive(Debug, Clone, PartialEq)]
        pub enum $name<X> where
            EAnfExt: ξ<X>,
            EAppExt: ξ<X>,
            EPrjExt: ξ<X>,
            EProdExt: ξ<X>,
            ELetInExt: ξ<X>,
            EIteExt: ξ<X>,
            EFlipExt: ξ<X>,
            EObserveExt: ξ<X>,
            ESampleExt: ξ<X>,
            AVarExt<EVal>: ξ<X>,
            AVarExt<SVal>: ξ<X>,
            <EAnfExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EAppExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EPrjExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EProdExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <ELetInExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EIteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EFlipExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EObserveExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <ESampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            AValExt<EVal>: ξ<X>,
            AValExt<SVal>: ξ<X>,
            <AVarExt<EVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AValExt<EVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AVarExt<SVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AValExt<SVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,

            SAnfExt: ξ<X>,
            SLetInExt: ξ<X>,
            SSeqExt: ξ<X>,
            SIteExt: ξ<X>,

            SAppExt: ξ<X>,
            SLambdaExt: ξ<X>,
            SMapExt: ξ<X>,
            SFoldExt: ξ<X>,
            SSampleExt: ξ<X>,
            <SAppExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SLambdaExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SMapExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SFoldExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SSampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,

            SObserveExt: ξ<X>,
            SExactExt: ξ<X>,
            <SObserveExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SAnfExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SLetInExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SSeqExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SIteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SExactExt as ξ<X>>::Ext: Debug + PartialEq + Clone,

            // sugar
            SLetSampleExt: ξ<X>,
            EDiscreteExt: ξ<X>,
            EIterateExt: ξ<X>,
            <SLetSampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EDiscreteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EIterateExt as ξ<X>>::Ext: Debug + PartialEq + Clone,

            X: Debug + PartialEq + Clone,
        {
            $($body)*
        }
    };
(impl<X> $name:ident<X> { $($body:tt)* }) => {
        impl<X> $name<X> where
            EAnfExt: ξ<X>,
            EAppExt: ξ<X>,
            EPrjExt: ξ<X>,
            EProdExt: ξ<X>,
            ELetInExt: ξ<X>,
            EIteExt: ξ<X>,
            EFlipExt: ξ<X>,
            EObserveExt: ξ<X>,
            ESampleExt: ξ<X>,
            AVarExt<EVal>: ξ<X>,
            AVarExt<SVal>: ξ<X>,
            <EAnfExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EAppExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EPrjExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EProdExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <ELetInExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EIteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EFlipExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EObserveExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <ESampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            AValExt<EVal>: ξ<X>,
            AValExt<SVal>: ξ<X>,
            <AVarExt<EVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AValExt<EVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AVarExt<SVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AValExt<SVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,

            SAnfExt: ξ<X>,
            SLetInExt: ξ<X>,
            SSeqExt: ξ<X>,
            SIteExt: ξ<X>,

            SAppExt: ξ<X>,
            SLambdaExt: ξ<X>,
            SMapExt: ξ<X>,
            SFoldExt: ξ<X>,
            SSampleExt: ξ<X>,

            <SAppExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SLambdaExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SMapExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SFoldExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SSampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,

            SObserveExt: ξ<X>,
            SExactExt: ξ<X>,
            <SObserveExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SAnfExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SLetInExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SSeqExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SIteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SExactExt as ξ<X>>::Ext: Debug + PartialEq + Clone,


            // sugar
            SLetSampleExt: ξ<X>,
            EDiscreteExt: ξ<X>,
            EIterateExt: ξ<X>,
            <SLetSampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EDiscreteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EIterateExt as ξ<X>>::Ext: Debug + PartialEq + Clone,


            X: Debug + PartialEq + Clone,

        {
            $($body)*
        }
    };
    (impl<X> $trait:ident for $name:ident<X> { $($body:tt)* }) => {
        impl<X> $trait for $name<X> where
            EAnfExt: ξ<X>,
            EAppExt: ξ<X>,
            EPrjExt: ξ<X>,
            EProdExt: ξ<X>,
            ELetInExt: ξ<X>,
            EIteExt: ξ<X>,
            EFlipExt: ξ<X>,
            EObserveExt: ξ<X>,
            ESampleExt: ξ<X>,
            AVarExt<EVal>: ξ<X>,
            AVarExt<SVal>: ξ<X>,
            <EAnfExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EAppExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EPrjExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EProdExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <ELetInExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EIteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EFlipExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EObserveExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <ESampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            AValExt<EVal>: ξ<X>,
            AValExt<SVal>: ξ<X>,
            <AVarExt<EVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AValExt<EVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AVarExt<SVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <AValExt<SVal> as ξ<X>>::Ext: Debug + PartialEq + Clone,

            SAnfExt: ξ<X>,
            SLetInExt: ξ<X>,
            SSeqExt: ξ<X>,
            SIteExt: ξ<X>,

            SAppExt: ξ<X>,
            SLambdaExt: ξ<X>,
            SMapExt: ξ<X>,
            SFoldExt: ξ<X>,
            SSampleExt: ξ<X>,

            <SAppExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SLambdaExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SMapExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SFoldExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SSampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,

            SObserveExt: ξ<X>,
            SExactExt: ξ<X>,
            <SObserveExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SAnfExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SLetInExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SSeqExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SIteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <SExactExt as ξ<X>>::Ext: Debug + PartialEq + Clone,

            // sugar
            SLetSampleExt: ξ<X>,
            EDiscreteExt: ξ<X>,
            EIterateExt: ξ<X>,
            <SLetSampleExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EDiscreteExt as ξ<X>>::Ext: Debug + PartialEq + Clone,
            <EIterateExt as ξ<X>>::Ext: Debug + PartialEq + Clone,

            X: Debug + PartialEq + Clone,
        {
            $($body)*
        }
    };
}
