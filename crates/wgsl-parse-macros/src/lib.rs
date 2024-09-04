use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token, Data, DeriveInput, Expr, Fields, Ident, LitInt, Token,
};

#[proc_macro_derive(Visit)]
pub fn derive_visit_fn(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let visit_name = format_ident!("Visit{name}");
    let visit_mut_name = format_ident!("VisitMut{name}");

    let expanded = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                let fields = fields.named.iter().map(|field| (&field.ident, &field.ty));
                let trait_fns = fields.clone().map(|(name, ty)| {
                    quote! {
                        fn #name(self) -> impl Iterator<Item = &'a #ty>;
                    }
                });
                let trait_mut_fns = fields.clone().map(|(name, ty)| {
                    quote! {
                        fn #name(self) -> impl Iterator<Item = &'a mut #ty>;
                    }
                });
                let trait_fn_impls = fields.clone().map(|(name, ty)| {
                    quote! {
                        fn #name(self) -> impl Iterator<Item = &'a #ty> {
                            self.map(|x| &x.#name)
                        }
                    }
                });
                let trait_mut_fn_impls = fields.map(|(name, ty)| {
                    quote! {
                        fn #name(self) -> impl Iterator<Item = &'a mut #ty> {
                            self.map(|x| &mut x.#name)
                        }
                    }
                });
                quote! {
                    pub trait #visit_name<'a> {
                        #(#trait_fns)*
                    }
                    pub trait #visit_mut_name<'a> {
                        #(#trait_mut_fns)*
                    }

                    impl<'a, I> #visit_name<'a> for I
                    where
                        I: Iterator<Item = &'a #name>,
                    {
                        #(#trait_fn_impls)*
                    }
                    impl<'a, I> #visit_mut_name<'a> for I
                    where
                        I: Iterator<Item = &'a mut #name>,
                    {
                        #(#trait_mut_fn_impls)*
                    }

                    impl #name {
                        pub fn visit(&self) -> impl Iterator<Item = &#name> {
                            std::iter::once(self)
                        }
                    }
                    impl #name {
                        pub fn visit_mut(&mut self) -> impl Iterator<Item = &mut #name> {
                            std::iter::once(self)
                        }
                    }
                }
            }
            Fields::Unnamed(_) => todo!(),
            Fields::Unit => todo!(),
        },
        Data::Enum(data) => {
            let unnamed_variants =
                data.variants
                    .iter()
                    .filter_map(|variant| match &variant.fields {
                        Fields::Unnamed(fields) => {
                            let var_snake =
                                format_ident!("match_{}", variant.ident.to_string().to_lowercase());
                            Some((&variant.ident, var_snake, fields))
                        }
                        _ => None,
                    });
            let trait_fns = unnamed_variants.clone().map(|(_, var_snake, fields)| {
                let ty = &fields.unnamed.first().unwrap().ty;
                quote! {
                    fn #var_snake(self) -> impl Iterator<Item = &'a #ty>;
                }
            });
            let trait_mut_fns = unnamed_variants.clone().map(|(_, var_snake, fields)| {
                let ty = &fields.unnamed.first().unwrap().ty;
                quote! {
                    fn #var_snake(self) -> impl Iterator<Item = &'a mut #ty>;
                }
            });
            let trait_fn_impls = unnamed_variants.clone().map(|(var, var_snake, fields)| {
                let ty = &fields.unnamed.first().unwrap().ty;
                quote! {
                    fn #var_snake(self) -> impl Iterator<Item = &'a #ty> {
                        self.filter_map(|x| match x {
                            #name::#var(x) => Some(x),
                            _ => None,
                        })
                    }
                }
            });
            let trait_mut_fn_impls = unnamed_variants.map(|(var, var_snake, fields)| {
                let ty = &fields.unnamed.first().unwrap().ty;
                quote! {
                    fn #var_snake(self) -> impl Iterator<Item = &'a mut #ty> {
                        self.filter_map(|x| match x {
                            #name::#var(x) => Some(x),
                            _ => None,
                        })
                    }
                }
            });
            quote! {
                pub trait #visit_name<'a> {
                    #(#trait_fns)*
                }
                pub trait #visit_mut_name<'a> {
                    #(#trait_mut_fns)*
                }

                impl<'a, I> #visit_name<'a> for I
                where
                    I: Iterator<Item = &'a #name>,
                {
                    #(#trait_fn_impls)*
                }
                impl<'a, I> #visit_mut_name<'a> for I
                where
                    I: Iterator<Item = &'a mut #name>,
                {
                    #(#trait_mut_fn_impls)*
                }

                impl #name {
                    pub fn visit(&self) -> impl Iterator<Item = &#name> {
                        std::iter::once(self)
                    }
                }
                impl #name {
                    pub fn visit_mut(&mut self) -> impl Iterator<Item = &mut #name> {
                        std::iter::once(self)
                    }
                }
            }
        }
        Data::Union(_) => todo!(),
    };

    TokenStream::from(expanded)
}

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

#[proc_macro]
pub fn query(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as QueryInput);

    fn quote_components(
        iter: impl Iterator<Item = QueryComponent>,
    ) -> impl Iterator<Item = proc_macro2::TokenStream> {
        iter.map(|component| match component {
            QueryComponent::Variant(enum_name, variant) => quote! {
                filter_map(|x| match x {
                    #enum_name :: #variant(x) => Some(x),
                    _ => None,
                })
            },
            QueryComponent::Member(member) => {
                quote! { map(|x| &x.#member) }
            }
            QueryComponent::Index(index) => {
                quote! { map(|x| &x.#index) }
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
                        let rest = quote_components(iter);
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
                                quote! { std::iter::once(&x.#member) }
                            }
                            _ => return None,
                        };
                        let rest = quote_components(iter);
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
                quote! { flat_map(|x| { x.iter() }) }
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

    let rest = quote_components(iter);

    let expanded = quote! {
        #first #(.#rest)*
    };

    expanded.into()
}
