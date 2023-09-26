use std::{array, error, fmt};

pub use dynamic_call_derive::{dynamic_call, skip};
use serde::{de::DeserializeOwned, ser::Serialize};
pub use serde_json;

#[derive(Debug)]
pub struct Error {
    pub method_name: Option<String>,
    pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    MethodCallNotAnObject,
    MissingMethodField,
    MethodFieldNotString,
    MissingParamsField,
    NoSuchMethod,
    MethodRequiresMut,
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
        if let Some(method_name) = &self.method_name {
            write!(f, "while calling {}: ", method_name)?;
        }
        match &self.kind {
            ErrorKind::MethodCallNotAnObject => write!(f, "method call json is not an object"),
            ErrorKind::MethodFieldNotString => write!(f, "field 'method' is not a string"),
            ErrorKind::MissingMethodField => write!(f, "missing 'method' field"),
            ErrorKind::MissingParamsField => write!(f, "missing 'params' field"),
            ErrorKind::NoSuchMethod => write!(f, "no such method"),
            ErrorKind::MethodRequiresMut => write!(f, "method requires mutable self reference"),
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
}

pub fn get_serialized_args<const N: usize>(
    args: serde_json::Value,
    param_names: &[&'static str; N],
) -> Result<[serde_json::Value; N], ErrorKind> {
    match args {
        serde_json::Value::Array(array) => {
            if array.len() < 2 {
                return Err(ErrorKind::TooFewArgs {
                    expected: 2,
                    actual: array.len(),
                });
            }
            Ok(array.try_into().unwrap())
        }
        serde_json::Value::Object(mut fields) => {
            let mut arg_values: [serde_json::Value; N] =
                array::from_fn(|_| serde_json::Value::Null);
            for (i, &name) in param_names.iter().enumerate() {
                let value = fields
                    .remove(name)
                    .ok_or(ErrorKind::MissingArg { param: name })?;
                arg_values[i] = value;
            }
            Ok(arg_values)
        }
        _ => Err(ErrorKind::ArgsNotArrayOrObject),
    }
}

pub fn deserialize_arg<T: DeserializeOwned>(
    arg: serde_json::Value,
    param: &'static str,
) -> Result<T, ErrorKind> {
    serde_json::from_value::<T>(arg)
        .map_err(|error| ErrorKind::DeserializeArgError { param, error })
}

pub fn serialize_result<T: Serialize>(result: T) -> Result<serde_json::Value, ErrorKind> {
    serde_json::to_value(result).map_err(|error| ErrorKind::SerializeResultError { error })
}

mod foo_dynamic_call {
    use crate::{deserialize_arg, get_serialized_args, serialize_result, Error, ErrorKind};

    use super::Foo;

    pub fn call_method_add(
        this: &mut impl Foo,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        let inner = || -> Result<serde_json::Value, ErrorKind> {
            let [x_json, y_json] = get_serialized_args(args, &["x", "y"])?;
            let x = deserialize_arg(x_json, "x")?;
            let y = deserialize_arg(y_json, "y")?;

            let result = Foo::add(this, &x, y);

            let result_value = serialize_result(result)?;
            Ok(result_value)
        };
        inner().map_err(|kind| Error {
            method_name: Some("add".to_string()),
            kind,
        })
    }

    pub fn call_method(
        this: &impl Foo,
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
        this: &mut impl Foo,
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
        this: &impl Foo,
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
        this: &mut impl Foo,
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
