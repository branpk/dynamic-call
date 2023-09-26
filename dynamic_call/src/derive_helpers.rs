//! Helper methods used by the derive macro.

#![allow(missing_docs)]

use std::array;

use serde::{de::DeserializeOwned, Serialize};
pub use serde_json;

use crate::ErrorKind;

pub fn get_serialized_args<const N: usize>(
    args: serde_json::Value,
    param_names: &[&'static str; N],
) -> Result<[serde_json::Value; N], ErrorKind> {
    match args {
        serde_json::Value::Array(array) => {
            if array.len() < param_names.len() {
                return Err(ErrorKind::TooFewArgs {
                    expected: param_names.len(),
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
