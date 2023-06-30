#![allow(dead_code)]
#![allow(unused_imports)]
extern crate proc_macro;
extern crate proc_macro2;

mod alias;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Colon, Plus};
use syn::{braced, token, Ident, Path, Token, Type, TypePath, Visibility};
use syn::{parse_macro_input, Result};
use syn::{AngleBracketedGenericArguments, GenericArgument, PathArguments, PathSegment};

use crate::alias::*;

type TokenStream2 = proc_macro2::TokenStream;
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
    brace_token: token::Brace,
    associations: Punctuated<Association, Token![,]>,
}
impl Parse for TTGPhase {
    fn parse(input: ParseStream) -> Result<Self> {
        println!("parsing phase");
        let content;
        Ok(TTGPhase {
            visibility: input.parse()?,
            struct_token: input.parse()?,
            ident: input.parse()?,
            brace_token: braced!(content in input),
            associations: content.parse_terminated(Association::parse, Token![,])?,
        })
    }
}

fn expand_phase(input: TTGPhase) -> TokenStream2 {
    let phase_name = input.ident.clone();
    let vis = input.visibility.clone();
    let assocs = input.associations.clone().into_iter().map(|assoc| {
        let expr = assoc.expr;
        let ext = assoc.extension;
        let phase_name = input.ident.clone();
        quote! {
            #[automatically_derived]
            impl ξ< #phase_name > for #expr {
                type Ext = #ext;
            }
        }
    });
    quote! {
        #[automatically_derived]
        #[derive(Clone, Debug, PartialEq)]
        #vis struct #phase_name;

        #(#assocs)*
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
