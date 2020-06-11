extern crate proc_macro;
extern crate quote;
extern crate syn;

use case::CaseExt;
use quote::*;
use std::collections::HashMap;
use syn::export::Span;
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::*;
use syn::*;

struct Machine {
    attributes: Vec<Attribute>,
    data: ItemEnum,
    methods: Option<Methods>,
}

impl Parse for Machine {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes: Vec<Attribute> = input.call(Attribute::parse_outer)?;
        let data: syn::ItemEnum = input.parse()?;

        let lookahead = input.lookahead1();
        let methods = if lookahead.peek(Token![impl]) {
            let methods: Methods = input.parse()?;

            Some(methods)
        } else {
            None
        };

        Ok(Machine { attributes, data, methods })
    }
}

#[proc_macro]
pub fn machine(input: proc_macro::TokenStream) -> syn::export::TokenStream {
    let ast = parse_macro_input!(input as Machine);

    let mut stream = proc_macro::TokenStream::new();

    let gen_machine = impl_machine(&ast);
    stream.extend(gen_machine);

    let gen_methods = impl_methods(&ast);
    stream.extend(gen_methods);

    stream
}

fn impl_machine(m: &Machine) -> syn::export::TokenStream {
    let Machine { attributes, data, .. } = m;
    let ast = data;

    let machine_name = &ast.ident;
    let variants_names = ast.variants.iter().map(|v| v.ident.clone()).collect::<Vec<_>>();
    let structs_names = variants_names.clone();

    let toks = quote! {
        #(#attributes)*
        pub enum #machine_name {
            #(#variants_names(#structs_names)),*
        }
    };

    let mut stream = proc_macro::TokenStream::from(toks);

    for ref variant in ast.variants.iter() {
        let name = &variant.ident;

        let fields = &variant
            .fields
            .iter()
            .map(|f| {
                let vis = &f.vis;
                let ident = &f.ident;
                let ty = &f.ty;

                quote! {
                    #vis #ident: #ty
                }
            })
            .collect::<Vec<_>>();

        let toks = quote! {
            #(#attributes)*
            pub struct #name {
                #(#fields),*
            }
        };

        stream.extend(proc_macro::TokenStream::from(toks));
    }

    let methods = &ast
        .variants
        .iter()
        .map(|variant| {
            let fn_name = Ident::new(&variant.ident.to_string().to_snake(), Span::call_site());
            let struct_name = &variant.ident;

            let args = &variant
                .fields
                .iter()
                .map(|f| {
                    let ident = &f.ident;
                    let ty = &f.ty;

                    quote! {
                        #ident: #ty
                    }
                })
                .collect::<Vec<_>>();

            let arg_names = &variant.fields.iter().map(|f| &f.ident).collect::<Vec<_>>();

            quote! {
                pub fn #fn_name(#(#args),*) -> #machine_name {
                    #machine_name::#struct_name(#struct_name {
                        #(#arg_names),*
                    })
                }
            }
        })
        .collect::<Vec<_>>();

    let toks = quote! {
        impl #machine_name {
            #(#methods)*
        }
    };

    stream.extend(proc_macro::TokenStream::from(toks));

    stream
}

fn impl_methods(machine: &Machine) -> syn::export::TokenStream {
    let mut stream = proc_macro::TokenStream::new();

    let variants_names = machine.data.variants.iter().map(|v| v.ident.clone()).collect::<Vec<_>>();

    if let Some(methods) = &machine.methods {
				let mut h = HashMap::new();
				
        for method in methods.methods.iter() {
            match &method.states {
                StateMatchType::None => {}
                StateMatchType::Wildcard => {
                    for state in variants_names.iter() {
                        h.entry(state).or_insert_with(Vec::new).push(&method.method);
                    }
                }
                StateMatchType::Include(idents) => {
                    for state in idents.iter() {
                        h.entry(state).or_insert_with(Vec::new).push(&method.method);
                    }
                }
            }
        }

        for (state, methods) in h.iter() {
            let method_toks = methods.iter().filter(|method| method.default.is_some()).collect::<Vec<_>>();

            let toks = quote! {
                impl #state {
                    #(#method_toks)*
                }
            };

            stream.extend(proc_macro::TokenStream::from(toks));
        }

        let machine_name = &machine.data.ident;

        let wrapper_methods = methods
            .methods
            .iter()
            .map(|method_data| {
                let method = &method_data.method;
                let ident = &method.sig.ident;
                let args = method
                    .sig
                    .inputs
                    .iter()
                    .filter_map(|arg| if let FnArg::Typed(a) = arg { Some(&a.pat) } else { None })
                    .collect::<Vec<_>>();

                let is_mut = method.sig.inputs.iter().any(|arg| match arg {
                    FnArg::Receiver(receiver) => receiver.mutability.is_some(),
                    FnArg::Typed(_) => false,
                });

                let ref_mut = if is_mut {
                    quote! { ref mut }
                } else {
                    quote! { ref }
                };

                let variants = variants_names
                    .iter()
                    .map(|state| {
                        let a = args.clone();
                        quote! {
                            #machine_name::#state(#ref_mut v) => v.#ident( #(#a),* ),
                        }
                    })
                    .collect::<Vec<_>>();

                let sig = &method.sig;

                quote! {
                    #sig {
                        match *self {
                            #(#variants)*
                        }
                    }
                }
            })
            .collect::<Vec<_>>();

        let toks = quote! {
            impl #machine_name {
                #(#wrapper_methods)*
            }
        };

        stream.extend(proc_macro::TokenStream::from(toks));
    }

    stream
}

#[derive(Debug)]
struct Methods {
    pub methods: Vec<Method>,
}

#[derive(Debug)]
struct Method {
    pub states: StateMatchType,
    pub method: TraitItemMethod,
}

#[derive(Debug)]
enum StateMatchType {
    None,
    Wildcard,
    Include(Vec<Ident>),
}

impl Parse for Methods {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![impl] = input.parse()?;

        let content;
        braced!(content in input);

        let mut methods = Vec::new();

        while !content.is_empty() {
            let method: Method = content.parse()?;

            methods.push(method);
        }

        Ok(Methods { methods })
    }
}

impl Parse for Method {
    fn parse(input: ParseStream) -> Result<Self> {
        let states: StateMatchType = input.parse()?;
        let _: Token![=>] = input.parse()?;
        let method: TraitItemMethod = input.parse()?;

        match states {
            StateMatchType::None => {
                if method.default.is_some() {
                    return Err(Error::new(
                        method.span(),
                        "Expected no default implementation as no states specified",
                    ));
                }
            }
            StateMatchType::Wildcard => {
                if method.default.is_none() {
                    return Err(Error::new(method.span(), "Expected default implementation for wildcard states"));
                }
            }
            StateMatchType::Include(_) => {
                if method.default.is_none() {
                    return Err(Error::new(method.span(), "Expected default implementation for states specified"));
                }
            }
        }

        Ok(Method { states, method })
    }
}

impl Parse for StateMatchType {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        let state_match = if lookahead.peek(Token![*]) {
            let _: Token![*] = input.parse()?;

            StateMatchType::Wildcard
        } else if lookahead.peek(Token![_]) {
            let _: Token![_] = input.parse()?;

            StateMatchType::None
        } else {
            let mut states = Vec::new();

            let state: Ident = input.parse()?;
            states.push(state);

            loop {
                let lookahead = input.lookahead1();
                if lookahead.peek(Token![,]) {
                    let _: Token![,] = input.parse()?;
                    let state: Ident = input.parse()?;
                    states.push(state);
                } else {
                    break;
                }
            }

            StateMatchType::Include(states)
        };

        Ok(state_match)
    }
}
