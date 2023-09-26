use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, FnArg, Ident, ItemTrait, Pat, TraitItem,
    TraitItemFn, Type,
};

#[derive(Debug, Clone)]
struct Method {
    ident: Ident,
    self_ref_type: Option<RefType>,
    params: Vec<Param>,
}

#[derive(Debug, Clone)]
struct Param {
    ident: Ident,
    ref_type: RefType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RefType {
    Ref,
    MutRef,
    Value,
}

#[proc_macro_attribute]
pub fn skip(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    input
}

fn is_skip_attr(attr: &Attribute) -> bool {
    let path = attr.path();
    path.segments.len() == 2
        && path.segments[0].ident == "dynamic_call"
        && path.segments[1].ident == "skip"
}

fn type_to_ref_type(ty: &Type) -> RefType {
    match ty {
        Type::Paren(paren) => type_to_ref_type(&paren.elem),
        Type::Reference(reference) => {
            if reference.mutability.is_some() {
                RefType::MutRef
            } else {
                RefType::Ref
            }
        }
        _ => RefType::Value,
    }
}

fn read_method(func: &TraitItemFn) -> Method {
    let ident = func.sig.ident.clone();

    let self_ref_type = func
        .sig
        .receiver()
        .map(|receiver| type_to_ref_type(&receiver.ty));

    let params: Vec<Param> = func
        .sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(i, arg)| match arg {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pat_type) => Some(Param {
                ident: match &*pat_type.pat {
                    Pat::Ident(pat_ident) => pat_ident.ident.clone(),
                    _ => Ident::new(&format!("_arg{i}"), pat_type.pat.span()),
                },
                ref_type: type_to_ref_type(&pat_type.ty),
            }),
        })
        .collect();

    Method {
        ident,
        self_ref_type,
        params,
    }
}

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

fn generate_call_specific_method(trait_ident: &Ident, method: &Method) -> TokenStream {
    let method_ident = &method.ident;
    let method_name_lit = method_ident.to_string();
    let call_method_name = format_ident!("call_method_{method_ident}");

    let this_param = match method.self_ref_type {
        Some(RefType::Ref) => quote!(this: &impl #trait_ident,),
        Some(RefType::MutRef) => quote!(this: &mut impl #trait_ident,),
        Some(RefType::Value) => quote!(this: impl #trait_ident,),
        None => quote!(),
    };

    let this_arg = match method.self_ref_type {
        Some(_) => quote!(this,),
        None => quote!(),
    };

    let mut param_names_array = TokenStream::new();
    method
        .params
        .iter()
        .map(|param| {
            let name_lit = param.ident.to_string();
            quote!(#name_lit,)
        })
        .for_each(|stream| param_names_array.append_all([stream]));

    let mut param_assigns = TokenStream::new();
    method
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let value_ident = format_ident!("arg_{i}");
            let name_lit = param.ident.to_string();
            quote! {
                let #value_ident = deserialize_arg(arg_array[#i], #name_lit)?;
            }
        })
        .for_each(|stream| param_assigns.append_all([stream]));

    let mut call_params = TokenStream::new();
    method
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let value_ident = format_ident!("arg_{i}");
            match param.ref_type {
                RefType::Ref => quote!(&#value_ident,),
                RefType::MutRef => quote!(&mut #value_ident,),
                RefType::Value => quote!(#value_ident,),
            }
        })
        .for_each(|stream| call_params.append_all([stream]));

    quote! {
        pub fn #call_method_name(
            #this_param
            args: serde_json::Value,
        ) -> Result<serde_json::Value, Error> {
            let inner = || -> Result<serde_json::Value, ErrorKind> {
                let arg_array = get_serialized_args(args, &[#param_names_array])?;
                #param_assigns

                let result = #trait_ident::add(#this_arg #call_params);

                let result_value = serialize_result(result)?;
                Ok(result_value)
            };
            inner().map_err(|kind| Error {
                method_name: Some(#method_name_lit.to_string()),
                kind,
            })
        }
    }
}

#[proc_macro_attribute]
pub fn dynamic_call(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as Ident);
    let input = parse_macro_input!(input as ItemTrait);

    let trait_ident = &input.ident;
    let methods = read_methods(&input);

    let mut call_methods = TokenStream::new();
    methods
        .iter()
        .map(|method| generate_call_specific_method(trait_ident, method))
        .for_each(|stream| call_methods.append_all([stream]));

    let expanded = quote! {
        #input

        mod #args {
            use ::dynamic_call::{deserialize_arg, get_serialized_args, serialize_result, Error, ErrorKind};
            use ::dynamic_call::serde_json as serde_json;

            use super::#trait_ident;

            #call_methods

            pub fn call_method(
                this: &impl #trait_ident,
                method: &str,
                args: serde_json::Value,
            ) -> Result<serde_json::Value, Error> {
                match method {
                    "add" => Err(Error {
                        method_name: Some(method.to_string()),
                        kind: ErrorKind::MethodRequiresMut,
                    }),
                    _ => Err(Error {
                        method_name: Some(method.to_string()),
                        kind: ErrorKind::NoSuchMethod,
                    }),
                }
            }

            pub fn call_method_mut(
                this: &mut impl #trait_ident,
                method: &str,
                args: serde_json::Value,
            ) -> Result<serde_json::Value, Error> {
                match method {
                    "add" => call_method_add(this, args),
                    _ => Err(Error {
                        method_name: Some(method.to_string()),
                        kind: ErrorKind::NoSuchMethod,
                    }),
                }
            }

            pub fn call_dynamic(
                this: &impl #trait_ident,
                json: serde_json::Value,
            ) -> Result<serde_json::Value, Error> {
                if let serde_json::Value::Object(mut object) = json {
                    let method = object
                        .remove("method")
                        .ok_or(ErrorKind::MissingMethodField)
                        .and_then(|value| match value {
                            serde_json::Value::String(s) => Ok(s),
                            _ => Err(ErrorKind::MethodFieldNotString),
                        })
                        .map_err(|kind| Error {
                            method_name: None,
                            kind,
                        })?;

                    let args = object
                        .remove("params")
                        .ok_or(ErrorKind::MissingParamsField)
                        .map_err(|kind| Error {
                            method_name: Some(method.to_string()),
                            kind,
                        })?;

                    call_method(this, &method, args)
                } else {
                    Err(Error {
                        method_name: None,
                        kind: ErrorKind::MethodCallNotAnObject,
                    })
                }
            }

            pub fn call_dynamic_mut(
                this: &mut impl #trait_ident,
                json: serde_json::Value,
            ) -> Result<serde_json::Value, Error> {
                if let serde_json::Value::Object(mut object) = json {
                    let method = object
                        .remove("method")
                        .ok_or(ErrorKind::MissingMethodField)
                        .and_then(|value| match value {
                            serde_json::Value::String(s) => Ok(s),
                            _ => Err(ErrorKind::MethodFieldNotString),
                        })
                        .map_err(|kind| Error {
                            method_name: None,
                            kind,
                        })?;

                    let args = object
                        .remove("params")
                        .ok_or(ErrorKind::MissingParamsField)
                        .map_err(|kind| Error {
                            method_name: Some(method.to_string()),
                            kind,
                        })?;

                    call_method_mut(this, &method, args)
                } else {
                    Err(Error {
                        method_name: None,
                        kind: ErrorKind::MethodCallNotAnObject,
                    })
                }
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}
