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
