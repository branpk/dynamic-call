use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse::Error, parse_macro_input, Attribute, FnArg, Ident, ItemTrait, Pat, Signature, TraitItem,
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
pub fn skip(_args: TokenStream, input: TokenStream) -> TokenStream {
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
                    _ => Ident::new(&format!("_arg{i}"), Span::call_site()),
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

#[proc_macro_attribute]
pub fn dynamic_call(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as Ident);
    let input = parse_macro_input!(input as ItemTrait);

    let trait_name = &input.ident;
    let methods = read_methods(&input);
    panic!("{:#?}", methods);

    let expanded = quote! {
        #input

        mod foo_dynamic_call {
            use ::dynamic_call::{deserialize_arg, get_serialized_args, serialize_result, Error, ErrorKind};
            use ::dynamic_call::serde_json as serde_json;

            use super::#trait_name;

            pub fn call_method_add(
                this: &mut impl #trait_name,
                args: serde_json::Value,
            ) -> Result<serde_json::Value, Error> {
                let inner = || -> Result<serde_json::Value, ErrorKind> {
                    let [x_json, y_json] = get_serialized_args(args, &["x", "y"])?;
                    let x = deserialize_arg(x_json, "x")?;
                    let y = deserialize_arg(y_json, "y")?;

                    let result = #trait_name::add(this, &x, y);

                    let result_value = serialize_result(result)?;
                    Ok(result_value)
                };
                inner().map_err(|kind| Error {
                    method_name: Some("add".to_string()),
                    kind,
                })
            }

            pub fn call_method(
                this: &impl #trait_name,
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
                this: &mut impl #trait_name,
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
                this: &impl #trait_name,
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
                this: &mut impl #trait_name,
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

    TokenStream::from(expanded)
}
