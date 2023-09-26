# dynamic_call

[![Crates.io](https://img.shields.io/crates/v/dynamic_call.svg)](https://crates.io/crates/dynamic_call)
[![Documentation](https://docs.rs/dynamic_call/badge.svg)](https://docs.rs/dynamic_call)

This crate allows looking up a trait method by name at runtime,
and calling the method with json arguments deserialized using `serde_json`.

## Example

```rust
use dynamic_call::dynamic_call;

#[dynamic_call(foo_dynamic_call)]
trait Foo {
    fn add(&mut self, x: i32) -> i32;
    fn show(&self, message: &str);
}
```

The above code generates a module that looks like:

```rust
pub mod foo_dynamic_call {
    /// Returns information about the signature of the given method.
    ///
    /// Returns `None` if the method is not a member of the trait or has been skipped
    /// using the `#[dynamic_call::skip]` attribute.
    pub fn method(name: &str) -> Option<MethodInfo> {
        // ...
    }

    /// Returns a list of methods in the trait, in declaration order.
    ///
    /// Excludes methods which have been skipped using the `#[dynamic_call::skip]`
    /// attribute.
    pub fn methods() -> Vec<MethodInfo> {
        // ...
    }

    /// Deserializes the given arguments using `serde_json` and calls [Foo::add].
    ///
    /// The arguments must be an array or an object with keys equal to the parameter names.
    pub fn call_method_add<T: Foo>(
        this: &mut T,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        // ...
    }

    /// Deserializes the given arguments using `serde_json` and calls [Foo::show].
    ///
    /// The arguments must be an array or an object with keys equal to the parameter names.
    pub fn call_method_show<T: Foo>(
        this: &T,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        // ...
    }

    /// Calls a static or `&self` method of [Foo] dynamically.
    ///
    /// The method name must be one of the methods in the trait. The arguments must be an
    /// array or an object with keys equal to the parameter names.
    pub fn call_dynamic<T: Foo>(
        this: &T,
        method_name: &str,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        // ...
    }

    /// Calls a static, `&self`, or `&mut self` method of [Foo] dynamically.
    ///
    /// The method name must be one of the methods in the trait. The arguments must be an
    /// array or an object with keys equal to the parameter names.
    pub fn call_dynamic_mut<T: Foo>(
        this: &mut T,
        method_name: &str,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        // ...
    }

    /// Calls any method of [Foo] dynamically.
    ///
    /// The method name must be one of the methods in the trait. The arguments must be an
    /// array or an object with keys equal to the parameter names.
    pub fn call_dynamic_value<T: Foo>(
        mut this: T,
        method_name: &str,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        // ...
    }

    /// Calls a static method of [Foo] dynamically.
    ///
    /// The method name must be one of the methods in the trait. The arguments must be an
    /// array or an object with keys equal to the parameter names.
    pub fn call_dynamic_static<T: Foo>(
        method_name: &str,
        args: serde_json::Value,
    ) -> Result<serde_json::Value, Error> {
        // ...
    }
}
```

## Supported methods

Methods with type parameters are not supported.

Static methods as well as methods with `self`, `&self`, or `&mut self` are all supported.
A compatible version of `call_dynamic_X` must be called depending on the method's `self` type.

Arguments are deserialized using `serde_json`. The arguments value must be an array or
an object where the keys match the method's parameter names. Extra arguments in the
array or object are ignored.

Parameter types are supported if they implement `Clone` and `DeserializeOwned`.
Additionally, string, array, and path slices are supported.
More specifically, an argument may be of the form `T`, `&T`, or `&mut T`, where
`T: ToOwned` and `T::Owned: DeserializeOwned`.

To skip a method, use the `#[dynamic_call::skip]` attribute.

License: MIT OR Apache-2.0
