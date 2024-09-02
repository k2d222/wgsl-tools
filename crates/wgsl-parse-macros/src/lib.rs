use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput};

#[proc_macro_derive(Visit)]
pub fn derive_visit_fn(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let visit_name = format_ident!("Visit{name}");
    let visit_mut_name = format_ident!("VisitMut{name}");

    let expanded = match input.data {
        Data::Struct(data) => match data.fields {
            syn::Fields::Named(fields) => {
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
            syn::Fields::Unnamed(_) => todo!(),
            syn::Fields::Unit => todo!(),
        },
        Data::Enum(data) => {
            let unnamed_variants =
                data.variants
                    .iter()
                    .filter_map(|variant| match &variant.fields {
                        syn::Fields::Unnamed(fields) => {
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
