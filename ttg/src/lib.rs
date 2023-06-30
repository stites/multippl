#![allow(dead_code)]
#![allow(unused_imports)]
extern crate proc_macro;
extern crate proc_macro2;

mod alias;

use proc_macro::TokenStream;
use quote::quote;
use std::collections::{HashMap, HashSet};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Colon, Plus};
use syn::{braced, token, Ident, Path, Token, Type, TypePath, Visibility};
use syn::{parse_macro_input, Result};
use syn::{AngleBracketedGenericArguments, GenericArgument, PathArguments, PathSegment};

use crate::alias::*;
const EXPRS: &str = r"{
    AVarExt<SVal>: (),
    AVarExt<EVal>: (),
    AValExt<EVal>: (),
    AValExt<SVal>: (),
    EAnfExt : (),
    EPrjExt : (),
    EProdExt : (),
    ELetInExt : (),
    EIteExt : (),
    EFlipExt : (),
    EObserveExt : (),
    SObserveExt : (),
    ESampleExt : (),
    SAnfExt : (),
    SLetInExt : (),
    SSeqExt : (),
    SIteExt : (),
    SBernExt : (),
    SDiscreteExt : (),
    SUniformExt : (),
    SNormalExt : (),
    SBetaExt : (),
    SDirichletExt : (),
    SExactExt : (),
}";

type TokenStream2 = proc_macro2::TokenStream;

#[derive(Clone)]
struct AllAssociations {
    brace_token: token::Brace,
    associations: Punctuated<Association, Token![,]>,
}

impl Parse for AllAssociations {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(AllAssociations {
            brace_token: braced!(content in input),
            associations: content.parse_terminated(Association::parse, Token![,])?,
        })
    }
}
fn get_ident(t: &Type) -> Ident {
    alias::split_type(t.clone()).ident
}
fn switch_default_ext(a: Association, ext: Option<Type>) -> Association {
    match ext {
        None => a,
        Some(ext) => {
            let mut a = a.clone();
            a.extension = ext;
            a
        }
    }
}
fn mk_basic_extensions(ext: Option<Type>) -> Result<HashMap<Type, Association>> {
    let assocs: AllAssociations = syn::parse_str(EXPRS)?;
    Ok(assocs
        .associations
        .into_iter()
        .map(|assoc| (assoc.expr.clone(), switch_default_ext(assoc, ext.clone())))
        .collect())
}

#[derive(Clone)]
struct Association {
    expr: Type,
    colon_token: Colon,
    extension: Type,
}
impl Parse for Association {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Association {
            expr: input.parse()?,
            colon_token: input.parse()?,
            extension: input.parse()?,
        })
    }
}

#[derive(Clone)]
struct TTGPhase {
    visibility: Visibility,
    struct_token: Token![struct],
    ident: Ident,
    colon_token: Option<Token![:]>,
    default_ext: Option<Type>,
    assocs: Option<AllAssociations>,
}
impl Parse for TTGPhase {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility = input.parse()?;
        let struct_token = input.parse()?;
        let ident = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        if colon_token.is_none() {
            Ok(TTGPhase {
                visibility,
                struct_token,
                ident,
                colon_token,
                default_ext: None,
                assocs: None,
            })
        } else {
            let default_ext = match input.parse() {
                Err(_) => None,
                Ok(e) => Some(e),
            };
            let assocs = match input.parse() {
                Err(_) => None,
                Ok(e) => Some(e),
            };
            Ok(TTGPhase {
                visibility,
                struct_token,
                ident,
                colon_token,
                default_ext,
                assocs,
            })
        }
    }
}
fn expand_impl(phase: &Ident, assoc: &Association) -> TokenStream2 {
    let expr = assoc.expr.clone();
    let ext = assoc.extension.clone();
    quote! {
        #[automatically_derived]
        impl ξ< #phase > for #expr {
            type Ext = #ext;
        }
    }
}
fn expand_phase(input: TTGPhase) -> TokenStream2 {
    let phase_name = input.ident.clone();
    let vis = input.visibility.clone();

    let assocs = input.assocs.iter().flat_map(|ascs| {
        ascs.associations
            .clone()
            .into_iter()
            .map(|assoc| expand_impl(&input.ident, &assoc))
    });
    let included: HashSet<Type> = input
        .assocs
        .iter()
        .flat_map(|ascs| {
            ascs.associations
                .clone()
                .into_iter()
                .map(|assoc| assoc.expr)
        })
        .collect();

    let remainder = mk_basic_extensions(input.default_ext.clone())
        .expect("this is always correct")
        .into_iter()
        .filter(|(i, _)| !included.contains(&i))
        .map(|(_, assoc)| expand_impl(&input.ident, &assoc));

    quote! {
        #[automatically_derived]
        #[derive(Clone, Debug, PartialEq)]
        #vis struct #phase_name;

        #(#assocs)*
        // pub struct FOOF;
        #(#remainder)*
    }
}

#[proc_macro]
pub fn phase(input: TokenStream) -> TokenStream {
    let phase = syn::parse_macro_input!(input as TTGPhase);
    expand_phase(phase).into()
}

#[proc_macro]
pub fn alias(input: TokenStream) -> TokenStream {
    let phase = syn::parse_macro_input!(input as TTGPhaseAliases);
    alias::expand_aliases(phase).into()
}
