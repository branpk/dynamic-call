//! This crate allows looking up a trait method by name at runtime,
//! and calling the method with json arguments deserialized using `serde_json`.
//!
//! # Example
//!
//! ```ignore
//! use dynamic_call::dynamic_call;
//!
//! #[dynamic_call(foo_dynamic_call)]
//! trait Foo {
//!     fn add(&mut self, x: i32) -> i32;
//!     fn show(&self, message: &str);
//! }
//! ```
//!
//! The above code generates a module that looks like:
//!
//! ```ignore
//! pub mod foo_dynamic_call {
//!     /// Returns information about the signature of the given method.
//!     ///
//!     /// Returns `None` if the method is not a member of the trait or has been skipped
//!     /// using the `#[dynamic_call::skip]` attribute.
//!     pub fn method(name: &str) -> Option<MethodInfo> {
//!         // ...
//!     }
//!
//!     /// Returns a list of methods in the trait, in declaration order.
//!     ///
//!     /// Excludes methods which have been skipped using the `#[dynamic_call::skip]`
//!     /// attribute.
//!     pub fn methods() -> Vec<MethodInfo> {
//!         // ...
//!     }
//!
//!     /// Deserializes the given arguments using `serde_json` and calls [Foo::add].
//!     ///
//!     /// The arguments must be an array or an object with keys equal to the parameter names.
//!     pub fn call_method_add<T: Foo>(
//!         this: &mut T,
//!         args: serde_json::Value,
//!     ) -> Result<serde_json::Value, Error> {
//!         // ...
//!     }
//!
//!     /// Deserializes the given arguments using `serde_json` and calls [Foo::show].
//!     ///
//!     /// The arguments must be an array or an object with keys equal to the parameter names.
//!     pub fn call_method_show<T: Foo>(
//!         this: &T,
//!         args: serde_json::Value,
//!     ) -> Result<serde_json::Value, Error> {
//!         // ...
//!     }
//!
//!     /// Calls a static or `&self` method of [Foo] dynamically.
//!     ///
//!     /// The method name must be one of the methods in the trait. The arguments must be an
//!     /// array or an object with keys equal to the parameter names.
//!     pub fn call_dynamic<T: Foo>(
//!         this: &T,
//!         method_name: &str,
//!         args: serde_json::Value,
//!     ) -> Result<serde_json::Value, Error> {
//!         // ...
//!     }
//!
//!     /// Calls a static, `&self`, or `&mut self` method of [Foo] dynamically.
//!     ///
//!     /// The method name must be one of the methods in the trait. The arguments must be an
//!     /// array or an object with keys equal to the parameter names.
//!     pub fn call_dynamic_mut<T: Foo>(
//!         this: &mut T,
//!         method_name: &str,
//!         args: serde_json::Value,
//!     ) -> Result<serde_json::Value, Error> {
//!         // ...
//!     }
//!
//!     /// Calls any method of [Foo] dynamically.
//!     ///
//!     /// The method name must be one of the methods in the trait. The arguments must be an
//!     /// array or an object with keys equal to the parameter names.
//!     pub fn call_dynamic_value<T: Foo>(
//!         mut this: T,
//!         method_name: &str,
//!         args: serde_json::Value,
//!     ) -> Result<serde_json::Value, Error> {
//!         // ...
//!     }
//!
//!     /// Calls a static method of [Foo] dynamically.
//!     ///
//!     /// The method name must be one of the methods in the trait. The arguments must be an
//!     /// array or an object with keys equal to the parameter names.
//!     pub fn call_dynamic_static<T: Foo>(
//!         method_name: &str,
//!         args: serde_json::Value,
//!     ) -> Result<serde_json::Value, Error> {
//!         // ...
//!     }
//! }
//! ```
//!
//! # Supported methods
//!
//! Methods with type parameters are not supported.
//!
//! Static methods as well as methods with `self`, `&self`, or `&mut self` are all supported.
//! A compatible version of `call_dynamic_X` must be called depending on the method's `self` type.
//!
//! Arguments are deserialized using `serde_json`. The arguments value must be an array or
//! an object where the keys match the method's parameter names. Extra arguments in the
//! array or object are ignored.
//!
//! Parameter types are supported if they implement `Clone` and `DeserializeOwned`.
//! Additionally, string, array, and path slices are supported.
//! More specifically, an argument may be of the form `T`, `&T`, or `&mut T`, where
//! `T: ToOwned` and `T::Owned: DeserializeOwned`.
//!
//! To skip a method, use the `#[dynamic_call::skip]` attribute.

#![warn(missing_docs, missing_debug_implementations)]

use std::{error, fmt};

pub use dynamic_call_proc_macro::{dynamic_call, skip};

pub mod proc_macro_helpers;

/// The type of `self` in a method signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelfType {
    /// The method uses `&self`.
    Ref,
    /// The method uses `&mut self`.
    MutRef,
    /// The method uses `self`.
    Value,
    /// The method has no `self` parameter.
    Static,
}

/// Information about a method signature.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodInfo {
    /// The name of the method.
    pub name: &'static str,
    /// The type of the method's `self` parameter.
    pub self_type: SelfType,
    /// The names of the method's parameters.
    ///
    /// If the method uses a pattern (such as `(x, y): (i32, i32)`), then the
    /// parameter name is changed to a placeholder name.
    pub param_names: Vec<&'static str>,
}

/// An error during dynamic call execution.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Error {
    pub method_name: String,
    pub kind: ErrorKind,
}

#[allow(missing_docs)]
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
