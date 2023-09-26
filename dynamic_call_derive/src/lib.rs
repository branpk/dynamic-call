use std::fmt;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, FnArg, Ident, ItemTrait, Pat, TraitItem,
    TraitItemFn, Type,
};

/// Information about a method signature.
#[derive(Debug, Clone)]
struct Method {
    ident: Ident,
    self_ref_type: Option<RefType>,
    params: Vec<Param>,
}

/// A parameter name and type.
#[derive(Clone)]
struct Param {
    ident: Ident,
    ref_type: RefType,
    inner_ty: Type,
}

impl fmt::Debug for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Param")
            .field("ident", &self.ident)
            .field("ref_type", &self.ref_type)
            .finish_non_exhaustive()
    }
}

/// The reference type of a parameter (doesn't consider nested references).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RefType {
    Ref,
    MutRef,
    Value,
}

/// Skips a particular method from being included in the dynamic call implementation.
#[proc_macro_attribute]
pub fn skip(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    input
}

/// Returns true if the attribute is equal to `dynamic_call::skip`.
fn is_skip_attr(attr: &Attribute) -> bool {
    let path = attr.path();
    path.segments.len() == 2
        && path.segments[0].ident == "dynamic_call"
        && path.segments[1].ident == "skip"
}

/// Splits a type into its reference type and the inner type.
fn split_type(ty: &Type) -> (RefType, Type) {
    match ty {
        Type::Paren(paren) => split_type(&paren.elem),
        Type::Reference(reference) => {
            if reference.mutability.is_some() {
                (RefType::MutRef, *reference.elem.clone())
            } else {
                (RefType::Ref, *reference.elem.clone())
            }
        }
        t => (RefType::Value, t.clone()),
    }
}

/// Reads a trait method item and returns a [Method] based on its signature.
fn read_method(func: &TraitItemFn) -> Method {
    let ident = func.sig.ident.clone();

    let self_ref_type = func
        .sig
        .receiver()
        .map(|receiver| split_type(&receiver.ty).0);

    let params: Vec<Param> = func
        .sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(i, arg)| match arg {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pat_type) => Some({
                let (ref_type, inner_ty) = split_type(&pat_type.ty);
                Param {
                    ident: match &*pat_type.pat {
                        Pat::Ident(pat_ident) => pat_ident.ident.clone(),
                        _ => Ident::new(&format!("__arg{i}"), pat_type.pat.span()),
                    },
                    ref_type,
                    inner_ty,
                }
            }),
        })
        .collect();

    Method {
        ident,
        self_ref_type,
        params,
    }
}

/// Reads the methods from the trait which should be included in the dynamic call
/// implementation.
fn read_methods(input: &ItemTrait) -> Vec<Method> {
    let mut methods = Vec::new();
    for item in &input.items {
        if let TraitItem::Fn(func) = item {
            let skip = func.attrs.iter().any(is_skip_attr);
            if !skip {
                methods.push(read_method(func));
            }
        }
    }
    methods
}

/// Generates the `method` and `methods` functions, which return information about
/// the signature of methods in the trait.
fn generate_method_info(methods: &[Method]) -> TokenStream {
    let method_match_arms = methods
        .iter()
        .map(|method| {
            let method_ident = &method.ident;
            let method_name_lit = method_ident.to_string();
            let self_type = match method.self_ref_type {
                Some(RefType::Ref) => quote!(SelfType::Ref),
                Some(RefType::MutRef) => quote!(SelfType::MutRef),
                Some(RefType::Value) => quote!(SelfType::Value),
                None => quote!(SelfType::Static),
            };
            let param_names_array = method
                .params
                .iter()
                .map(|param| {
                    let name_lit = param.ident.to_string();
                    quote!(#name_lit)
                })
                .collect::<Vec<_>>();

            quote! {
                #method_name_lit => Some(MethodInfo {
                    name: #method_name_lit,
                    self_type: #self_type,
                    param_names: [#(#param_names_array),*].into(),
                }),
            }
        })
        .collect::<Vec<_>>();

    let method_info_calls = methods
        .iter()
        .map(|method| {
            let name_lit = method.ident.to_string();
            quote! {
                method(#name_lit).unwrap(),
            }
        })
        .collect::<Vec<_>>();

    quote! {
        /// Returns information about the signature of the given method.
        ///
        /// Returns `None` if the method is not a member of the trait or has been skipped
        /// using the `#[dynamic_call::skip]` attribute.
        pub fn method(name: &str) -> Option<MethodInfo> {
            match name {
                #(#method_match_arms)*
                _ => None,
            }
        }

        /// Returns a list of methods in the trait, in declaration order.
        ///
        /// Excludes methods which have been skipped using the `#[dynamic_call::skip]`
        /// attribute.
        pub fn methods() -> Vec<MethodInfo> {
            [#(#method_info_calls)*].into()
        }
    }
}

/// Generates the `call_method_X` method for a given method, which allows calling the
/// method with json params.
fn generate_call_specific_method(trait_ident: &Ident, method: &Method) -> TokenStream {
    let method_ident = &method.ident;
    let method_name_lit = method_ident.to_string();
    let call_method_name = format_ident!("call_method_{method_ident}");

    let this_param = match method.self_ref_type {
        Some(RefType::Ref) => quote!(this: &T,),
        Some(RefType::MutRef) => quote!(this: &mut T,),
        Some(RefType::Value) => quote!(this: T,),
        None => quote!(),
    };

    let this_arg = match method.self_ref_type {
        Some(_) => quote!(this,),
        None => quote!(),
    };

    let param_name_lits = method
        .params
        .iter()
        .map(|param| {
            let name_lit = param.ident.to_string();
            quote!(#name_lit)
        })
        .collect::<Vec<_>>();

    let param_json_idents = method
        .params
        .iter()
        .enumerate()
        .map(|(i, _)| {
            let json_ident = format_ident!("json_{i}");
            quote!(#json_ident)
        })
        .collect::<Vec<_>>();

    let param_assigns = method
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let value_ident = format_ident!("arg_{i}");
            let json_ident = format_ident!("json_{i}");
            let inner_ty = &param.inner_ty;
            let name_lit = param.ident.to_string();
            quote! {
                let mut #value_ident: <#inner_ty as ToOwned>::Owned =
                    deserialize_arg(#json_ident, #name_lit)?;
            }
        })
        .collect::<Vec<_>>();

    let call_params = method
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let value_ident = format_ident!("arg_{i}");
            match param.ref_type {
                RefType::Ref => quote!(&#value_ident),
                RefType::MutRef => quote!(&mut #value_ident),
                RefType::Value => quote!(#value_ident),
            }
        })
        .collect::<Vec<_>>();

    let doc_comment = format!(
        "Deserializes the given arguments using `serde_json` and calls [{trait_ident}::{method_ident}].\n\n\
        The arguments must be an array or an object with keys equal to the parameter names.",
    );

    quote! {
        #[doc=#doc_comment]
        pub fn #call_method_name<T: #trait_ident>(
            #this_param
            args: serde_json::Value,
        ) -> Result<serde_json::Value, Error> {
            let inner = || -> Result<serde_json::Value, ErrorKind> {
                let [#(#param_json_idents),*] = get_serialized_args(args, &[#(#param_name_lits),*])?;
                #(#param_assigns)*

                let result = <T as #trait_ident>::#method_ident(#this_arg #(#call_params),*);

                let result_value = serialize_result(result)?;
                Ok(result_value)
            };
            inner().map_err(|kind| Error {
                method_name: #method_name_lit.to_string(),
                kind,
            })
        }
    }
}

/// Generates a `call_dynamic_X` which looks up and calls a method from its name.
fn generate_call_dynamic(
    trait_ident: &Ident,
    self_ref_type: Option<RefType>,
    methods: &[Method],
) -> TokenStream {
    let call_dynamic_ident = match self_ref_type {
        Some(RefType::Ref) => format_ident!("call_dynamic"),
        Some(RefType::MutRef) => format_ident!("call_dynamic_mut"),
        Some(RefType::Value) => format_ident!("call_dynamic_value"),
        None => format_ident!("call_dynamic_static"),
    };

    let this_param = match self_ref_type {
        Some(RefType::Ref) => quote!(this: &T,),
        Some(RefType::MutRef) => quote!(this: &mut T,),
        Some(RefType::Value) => quote!(mut this: T,),
        None => quote!(),
    };

    let self_type = match self_ref_type {
        Some(RefType::Ref) => quote!(SelfType::Ref),
        Some(RefType::MutRef) => quote!(SelfType::MutRef),
        Some(RefType::Value) => quote!(SelfType::Value),
        None => quote!(SelfType::Static),
    };

    let method_match_arms = methods
        .iter()
        .filter_map(|method| {
            let method_name_lit = method.ident.to_string();
            let call_method_ident = format_ident!("call_method_{}", method.ident);
            let this_arg = match (self_ref_type, method.self_ref_type) {
                (_, None) => quote!(),
                (None, _) => return None,
                (Some(RefType::Ref), Some(RefType::Ref)) => quote!(this,),
                (Some(RefType::Ref), Some(RefType::MutRef)) => return None,
                (Some(RefType::Ref), Some(RefType::Value)) => return None,
                (Some(RefType::MutRef), Some(RefType::Ref)) => quote!(this,),
                (Some(RefType::MutRef), Some(RefType::MutRef)) => quote!(this,),
                (Some(RefType::MutRef), Some(RefType::Value)) => return None,
                (Some(RefType::Value), Some(RefType::Ref)) => quote!(&this,),
                (Some(RefType::Value), Some(RefType::MutRef)) => quote!(&mut this,),
                (Some(RefType::Value), Some(RefType::Value)) => quote!(this,),
            };
            Some(quote! {
                #method_name_lit => #call_method_ident::<T>(#this_arg args),
            })
        })
        .collect::<Vec<_>>();

    let allowed_method_clause = match self_ref_type {
        Some(RefType::Ref) => "a static or `&self`",
        Some(RefType::MutRef) => "a static, `&self`, or `&mut self`",
        Some(RefType::Value) => "any",
        None => "a static",
    };

    let doc_comment = format!(
        "Calls {allowed_method_clause} method of [{trait_ident}] dynamically.\n\n\
        The method name must be one of the methods in the trait. \
        The arguments must be an array or an object with keys equal to the parameter names.",
    );

    quote! {
        #[doc=#doc_comment]
        pub fn #call_dynamic_ident<T: #trait_ident>(
            #this_param
            method_name: &str,
            args: serde_json::Value,
        ) -> Result<serde_json::Value, Error> {
            match method_name {
                #(#method_match_arms)*
                _ => match method(method_name) {
                    Some(info) => Err(Error {
                        method_name: method_name.to_string(),
                        kind: ErrorKind::WrongSelfType {
                            expected: info.self_type,
                            actual: #self_type,
                        },
                    }),
                    None => Err(Error {
                        method_name: method_name.to_string(),
                        kind: ErrorKind::NoSuchMethod,
                    }),
                },
            }
        }
    }
}

/// Generates a module with the given name, which implements the ability to call
/// trait methods dynamically.
///
/// See the crate-level documentation of `dynamic_call` for more info.
#[proc_macro_attribute]
pub fn dynamic_call(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as Ident);
    let input = parse_macro_input!(input as ItemTrait);

    let trait_ident = &input.ident;
    let methods = read_methods(&input);

    let method_info = generate_method_info(&methods);

    let call_methods = methods
        .iter()
        .map(|method| generate_call_specific_method(trait_ident, method))
        .collect::<Vec<_>>();

    let call_dynamics = [
        Some(RefType::Ref),
        Some(RefType::MutRef),
        Some(RefType::Value),
        None,
    ]
    .into_iter()
    .map(|self_ref_type| generate_call_dynamic(trait_ident, self_ref_type, &methods))
    .collect::<Vec<_>>();

    let module_doc = format!(
        "This module is generated by the `dynamic_call` macro and provides the \
        ability to call methods of [{trait_ident}] dynamically."
    );

    let expanded = quote! {
        #input

        #[allow(unused)]
        #[doc=#module_doc]
        pub mod #args {
            use ::dynamic_call::*;
            use ::dynamic_call::derive_helpers::*;

            use super::#trait_ident;

            #method_info
            #(#call_methods)*
            #(#call_dynamics)*
        }
    };

    proc_macro::TokenStream::from(expanded)
}
