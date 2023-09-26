use std::{error, fmt};

pub use dynamic_call_derive::{dynamic_call, skip};

pub mod derive_helpers;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelfType {
    Ref,
    MutRef,
    Value,
    Static,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodInfo {
    pub name: &'static str,
    pub self_type: SelfType,
    pub param_names: Vec<Option<&'static str>>,
}

#[derive(Debug)]
pub struct Error {
    pub method_name: String,
    pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    NoSuchMethod,
    WrongSelfType {
        expected: SelfType,
        actual: SelfType,
    },
    DeserializeArgError {
        param: &'static str,
        error: serde_json::Error,
    },
    SerializeResultError {
        error: serde_json::Error,
    },
    ArgsNotArrayOrObject,
    TooFewArgs {
        expected: usize,
        actual: usize,
    },
    MissingArg {
        param: &'static str,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "while calling {}: ", self.method_name)?;
        match &self.kind {
            ErrorKind::NoSuchMethod => write!(f, "no such method"),
            ErrorKind::WrongSelfType { expected, actual } => {
                match expected {
                    SelfType::Ref => write!(f, "method requires `&self`, ")?,
                    SelfType::MutRef => write!(f, "method requires `&mut self`, ")?,
                    SelfType::Value => write!(f, "method requires `self`, ")?,
                    SelfType::Static => write!(f, "method is static, ")?,
                };
                match actual {
                    SelfType::Ref => write!(f, "called with `&self`")?,
                    SelfType::MutRef => write!(f, "called with `&mut self`")?,
                    SelfType::Value => write!(f, "called with `self`")?,
                    SelfType::Static => write!(f, "called without `self` param")?,
                }
                Ok(())
            }
            ErrorKind::DeserializeArgError { param, error } => {
                write!(f, "failed to deserialize argument {}: {}", param, error)
            }
            ErrorKind::SerializeResultError { error } => {
                write!(f, "failed to serialize result: {}", error)
            }
            ErrorKind::ArgsNotArrayOrObject => {
                write!(f, "arguments must be an array or object")
            }
            ErrorKind::TooFewArgs { expected, actual } => {
                write!(f, "expected {} arguments, got {}", expected, actual)
            }
            ErrorKind::MissingArg { param } => {
                write!(f, "missing argument {}", param)
            }
        }
    }
}

impl error::Error for Error {}

pub trait Foo {
    fn add(&mut self, x: &i32, y: i32) -> i32;
    fn foo(&self) -> i32;
    fn show<'a>(self, s: &'a str, v: &[i32], w: i32, x: &i32) -> &'a str;
}

#[allow(unused)]
mod foo_dynamic_call {
    use crate::derive_helpers::{deserialize_arg, get_serialized_args, serialize_result};
    use crate::{Error, ErrorKind, MethodInfo, SelfType};

    use super::Foo;

    pub fn method(name: &str) -> Option<MethodInfo> {
        match name {
            "add" => Some(MethodInfo {
                name: "add",
                self_type: SelfType::MutRef,
                param_names: vec![Some("x"), Some("y")],
            }),
            "foo" => Some(MethodInfo {
                name: "foo",
                self_type: SelfType::Ref,
                param_names: vec![],
            }),
            "show" => Some(MethodInfo {
                name: "show",
                self_type: SelfType::Value,
                param_names: vec![Some("s"), Some("v"), Some("w"), Some("x")],
            }),
            _ => None,
        }
    }

    pub fn methods() -> Vec<MethodInfo> {
        vec![
            method("add").unwrap(),
            method("foo").unwrap(),
            method("show").unwrap(),
        ]
    }

    pub fn call_method_add(
        this: &mut impl Foo,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        let inner = || -> Result<serde_json::Value, ErrorKind> {
            let [x_json, y_json] = get_serialized_args(args, &["x", "y"])?;
            let mut x = deserialize_arg(x_json, "x")?;
            let mut y = deserialize_arg(y_json, "y")?;

            let result = Foo::add(this, &x, y);

            let result_value = serialize_result(result)?;
            Ok(result_value)
        };
        inner().map_err(|kind| Error {
            method_name: "add".to_string(),
            kind,
        })
    }

    pub fn call_method_foo(
        this: &impl Foo,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        let inner = || -> Result<serde_json::Value, ErrorKind> {
            let [] = get_serialized_args(args, &[])?;

            let result = Foo::foo(this);

            let result_value = serialize_result(result)?;
            Ok(result_value)
        };
        inner().map_err(|kind| Error {
            method_name: "add".to_string(),
            kind,
        })
    }

    pub fn call_method_show(
        this: impl Foo,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        let inner = || -> Result<serde_json::Value, ErrorKind> {
            let [s_json, v_json, w_json, x_json] =
                get_serialized_args(args, &["s", "v", "w", "x"])?;
            let s: <str as ToOwned>::Owned = deserialize_arg(s_json, "s")?;
            let v: <[i32] as ToOwned>::Owned = deserialize_arg(v_json, "v")?;
            let w: <i32 as ToOwned>::Owned = deserialize_arg(w_json, "w")?;
            let x: <i32 as ToOwned>::Owned = deserialize_arg(x_json, "x")?;

            let result = Foo::show(this, &s, &v, w, &x);

            let result_value = serialize_result(result)?;
            Ok(result_value)
        };
        inner().map_err(|kind| Error {
            method_name: "show".to_string(),
            kind,
        })
    }

    pub fn call_dynamic(
        this: &impl Foo,
        method_name: &str,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        match method_name {
            "foo" => call_method_foo(this, args),
            "add" | "show" => Err(Error {
                method_name: method_name.to_string(),
                kind: ErrorKind::WrongSelfType {
                    expected: SelfType::Ref,
                    actual: SelfType::Ref,
                },
            }),
            _ => match method(method_name) {
                Some(info) => Err(Error {
                    method_name: method_name.to_string(),
                    kind: ErrorKind::WrongSelfType {
                        expected: info.self_type,
                        actual: SelfType::Ref,
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
