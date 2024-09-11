use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token, Expr, Ident, LitInt, Token,
};

struct QueryInput {
    components: Punctuated<QueryComponent, Token![.]>,
}

enum BranchKind {
    Variants,
    Members,
}

enum QueryComponent {
    Variant(Ident, Ident),
    Member(Ident),
    Index(LitInt),
    Branch(BranchKind, Punctuated<QueryInput, Token![,]>),
    Iter,
    Expr(Option<Ident>, Expr),
    Void,
}

impl Parse for QueryComponent {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if input.peek(Ident) && input.peek2(Token![::]) {
            let enum_name: Ident = input.parse()?;
            input.parse::<Token![::]>()?;
            let variant: Ident = input.parse()?;
            Ok(QueryComponent::Variant(enum_name, variant))
        } else if input.peek(Ident) {
            let member = input.parse()?;
            Ok(QueryComponent::Member(member))
        // } else if input.peek(Token![self]) {
        //     input.parse::<Token![self]>()?;
        //     Ok(QueryComponent::Member(format_ident!("self")))
        } else if input.peek(LitInt) {
            let index = input.parse()?;
            Ok(QueryComponent::Index(index))
        } else if input.peek(token::Brace) {
            let content;
            braced!(content in input);
            let branch_kind = if content.peek(Ident) && content.peek2(Token![::]) {
                BranchKind::Variants
            } else {
                BranchKind::Members
            };
            let components =
                Punctuated::<QueryInput, Token![,]>::parse_separated_nonempty(&content)?;
            content.parse::<Token![,]>().ok();
            Ok(QueryComponent::Branch(branch_kind, components))
        } else if input.peek(token::Bracket) {
            input.parse::<proc_macro2::Group>()?;
            Ok(QueryComponent::Iter)
        } else if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let ident = if content.peek2(Token![=>]) {
                let ident = content.parse::<Ident>()?;
                content.parse::<Token![=>]>()?;
                Some(ident)
            } else {
                None
            };
            let expr = content.parse::<Expr>()?;
            Ok(QueryComponent::Expr(ident, expr))
        } else if input.is_empty() {
            Ok(QueryComponent::Void)
        } else {
            Err(input.error("expected a struct member, enum variant, `{`, `[` or `(`"))
        }
    }
}

impl Parse for QueryInput {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let components = Punctuated::<QueryComponent, Token![.]>::parse_separated_nonempty(input)?;
        Ok(QueryInput { components })
    }
}

fn query_impl(input: QueryInput, mutable: bool) -> TokenStream {
    fn quote_components(
        iter: impl Iterator<Item = QueryComponent>,
        mutable: bool,
    ) -> impl Iterator<Item = proc_macro2::TokenStream> {
        let ref_ = if mutable {
            quote! { &mut }
        } else {
            quote! { & }
        };
        iter.map(move |component| match component {
            QueryComponent::Variant(enum_name, variant) => quote! {
                filter_map(|x| match x {
                    #enum_name :: #variant(x) => Some(x),
                    _ => None,
                })
            },
            QueryComponent::Member(member) => {
                quote! { map(|x| #ref_ x.#member) }
            }
            QueryComponent::Index(index) => {
                quote! { map(|x| #ref_ x.#index) }
            }
            QueryComponent::Branch(kind, branch) => match kind {
                BranchKind::Variants => {
                    let cases = branch.into_iter().filter_map(|case| {
                        let mut iter = case.components.into_iter();
                        let first = match iter.next() {
                            Some(QueryComponent::Variant(enum_name, variant)) => {
                                quote! { #enum_name :: #variant (x) }
                            }
                            _ => return None,
                        };
                        let rest = quote_components(iter, mutable);
                        Some(quote! { #first => Box::new(std::iter::once(x) #(.#rest)*) })
                    });
                    quote! {
                        flat_map(|x| -> Box<dyn Iterator<Item = _>> {
                            match x {
                                #(#cases,)*
                                _ => Box::new(std::iter::empty()),
                            }
                        })
                    }
                }
                BranchKind::Members => {
                    let cases = branch.into_iter().filter_map(|case| {
                        let mut iter = case.components.into_iter();
                        let first = match iter.next() {
                            Some(QueryComponent::Member(member)) => {
                                quote! { std::iter::once(#ref_ x.#member) }
                            }
                            _ => return None,
                        };
                        let rest = quote_components(iter, mutable);
                        Some(quote! { #first #(.#rest)* })
                    });
                    quote! {
                        flat_map(|x| {
                            itertools::chain!( #(#cases),* )
                        })
                    }
                }
            },
            QueryComponent::Iter => {
                quote! { flat_map(|x| { x.into_iter() }) }
            }
            QueryComponent::Expr(ident, expr) => {
                if let Some(ident) = ident {
                    quote! { flat_map(|#ident| { #expr }) }
                } else {
                    quote! { flat_map(#expr) }
                }
            }
            QueryComponent::Void => quote! {},
        })
    }

    let mut iter = input.components.into_iter();

    let first = match iter.next() {
        Some(QueryComponent::Member(member)) => quote! { std::iter::once(#member) },
        _ => return quote! {}.into(),
    };

    let rest = quote_components(iter, mutable);

    let expanded = quote! {
        #first #(.#rest)*
    };

    expanded.into()
}

#[proc_macro]
pub fn query(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as QueryInput);
    query_impl(input, false)
}

#[proc_macro]
pub fn query_mut(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as QueryInput);
    query_impl(input, true)
}
