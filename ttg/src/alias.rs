use crate::*;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Colon, Plus};
use syn::{braced, token, Ident, Path, Token, Type, TypePath, Visibility};
use syn::{parse_macro_input, Result};
use syn::{AngleBracketedGenericArguments, GenericArgument, PathArguments, PathSegment};

pub(crate) struct TTGPhaseAliases {
    phase: Ident,
    plus_token: Plus,
    paren_token: token::Paren,
    aliases: Punctuated<Type, Token![,]>,
}

impl Parse for TTGPhaseAliases {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(TTGPhaseAliases {
            phase: input.parse()?,
            plus_token: input.parse()?,
            paren_token: syn::parenthesized!(content in input),
            aliases: content.parse_terminated(Type::parse, Token![,])?,
        })
    }
}
pub(crate) fn split_type(partial_type: Type) -> PathSegment {
    match partial_type {
        Type::Path(tp) => tp.path.segments.last().unwrap().clone(),
        _ => panic!(),
    }
}
fn seg_as_type(seg: PathSegment) -> Type {
    let mut segments = Punctuated::new();
    segments.push(seg);
    let path = Path {
        leading_colon: None,
        segments,
    };
    let typepath = TypePath { qself: None, path };
    Type::Path(typepath)
}

struct NewPathSegment {
    old_args: PathArguments,
    new_seg: PathSegment,
}

fn cons_phase(phase: Ident, seg: PathSegment) -> NewPathSegment {
    use PathArguments::*;
    let phase_seg = PathSegment {
        ident: phase,
        arguments: PathArguments::None,
    };
    let phase_arg = GenericArgument::Type(seg_as_type(phase_seg));

    match seg.arguments.clone() {
        PathArguments::None => {
            let mut args: Punctuated<GenericArgument, token::Comma> = Punctuated::new();
            args.push(phase_arg);
            let new_seg = PathSegment {
                ident: seg.ident,
                arguments: AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: Option::None,
                    lt_token: Default::default(),
                    args: args,
                    gt_token: Default::default(),
                }),
            };
            NewPathSegment {
                old_args: PathArguments::None,
                new_seg,
            }
        }
        AngleBracketed(prev) => {
            let mut args: Punctuated<GenericArgument, token::Comma> = Punctuated::new();
            args.push(phase_arg);
            for a in prev.args.clone() {
                args.push(a);
            }
            let new_seg = PathSegment {
                ident: seg.ident,
                arguments: AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: Option::None,
                    lt_token: Default::default(),
                    args: args,
                    gt_token: Default::default(),
                }),
            };
            NewPathSegment {
                old_args: seg.arguments,
                new_seg,
            }
        }
        _ => panic!("API misuse"),
    }
}
fn mk_alias_pair(phase_name: Ident, partial_type: Type) -> (Type, Type) {
    let pseg = split_type(partial_type);
    let alias_ident = Ident::new(
        &format!("{}{}", pseg.ident, phase_name),
        proc_macro2::Span::call_site(),
    );
    let newseg = cons_phase(phase_name, pseg);
    let alias = PathSegment {
        ident: alias_ident,
        arguments: newseg.old_args.clone(),
    };
    (seg_as_type(alias), seg_as_type(newseg.new_seg))
}

pub(crate) fn expand_aliases(input: TTGPhaseAliases) -> TokenStream2 {
    let aliases = input.aliases.into_iter().map(|partial_type| {
        let (alias, ty) = mk_alias_pair(input.phase.clone(), partial_type);

        quote! {
            #[automatically_derived]
            pub type #alias = #ty;
        }
    });
    quote! {
        #(#aliases)*
    }
}
