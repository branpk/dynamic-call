use dynamic_call::dynamic_call;
use serde_json::json;

#[dynamic_call(bar_dynamic_call)]
trait Bar {
    #[dynamic_call::skip]
    fn foo(&self, x: i32) -> i32;

    fn add(&mut self, x: &i32, y: i32) -> i32;

    fn sub(x: &mut i32, y: i32, (t, r): (i32, i32)) {
        println!("called sub({:?}, {:?}, {:?})", x, y, (t, r));
        *x -= y + t + r;
    }

    fn print(&self, s: String);

    fn show<'a>(self, s: &'a str, v: &[i32]) -> &'a str;

    fn empty_static();

    fn empty_ref(&self);
}

#[dynamic_call::dynamic_call(foo_dynamic_call)]
trait Foo {
    fn add(&mut self, x: i32) -> i32;
    fn show(&self, message: &str);
}

#[derive(Debug, Clone)]
struct BarImpl {
    inner: i32,
}

impl Bar for BarImpl {
    fn foo(&self, x: i32) -> i32 {
        println!("called foo({x})");
        self.inner + x
    }

    fn add(&mut self, x: &i32, y: i32) -> i32 {
        println!("called add({x}, {y})");
        self.inner += *x + y;
        self.inner
    }

    fn print(&self, s: String) {
        println!("called print({:?})", s);
    }

    fn show<'a>(self, s: &'a str, v: &[i32]) -> &'a str {
        println!("called show({:?}, {:?})", s, v);
        s
    }

    fn empty_static() {
        println!("called empty_static()");
    }

    fn empty_ref(&self) {
        println!("called empty_ref()");
    }
}

#[dynamic_call(baz_dynamic_call)]
pub trait Baz {}

fn main() {
    let calls: Vec<(&str, serde_json::Value)> = vec![
        ("foo", json!([3])),
        ("add", json!({ "x": 2, "y": 3 })),
        ("sub", json!([5, 7, [1, 2]])),
        ("print", json!(["foo"])),
        ("show", json!({ "s": "hello", "v": [1, 2, 3] })),
        ("empty_static", json!([])),
        ("empty_ref", json!({})),
    ];

    println!("\n--- BY REFERENCE ---");
    let bar = BarImpl { inner: 0 };
    for call in &calls {
        match bar_dynamic_call::call_dynamic(&bar, call.0, call.1.clone()) {
            Ok(result) => println!(" -> {}", result),
            Err(err) => println!("error: {}", err),
        }
    }

    println!("\n--- BY MUT REFERENCE ---");
    let mut bar = BarImpl { inner: 0 };
    for call in &calls {
        match bar_dynamic_call::call_dynamic_mut(&mut bar, call.0, call.1.clone()) {
            Ok(result) => println!(" -> {}", result),
            Err(err) => println!("error: {}", err),
        }
    }

    println!("\n--- BY VALUE ---");
    let bar = BarImpl { inner: 0 };
    for call in &calls {
        match bar_dynamic_call::call_dynamic_value(bar.clone(), call.0, call.1.clone()) {
            Ok(result) => println!(" -> {}", result),
            Err(err) => println!("error: {}", err),
        }
    }

    println!("\n--- STATIC ---");
    for call in &calls {
        match bar_dynamic_call::call_dynamic_static::<BarImpl>(call.0, call.1.clone()) {
            Ok(result) => println!(" -> {}", result),
            Err(err) => println!("error: {}", err),
        }
    }
}
