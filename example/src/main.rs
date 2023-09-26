use dynamic_call::dynamic_call;

#[dynamic_call(bar_dynamic_call)]
pub trait Bar {
    #[dynamic_call::skip]
    fn cool(&self, x: i32) -> i32;

    fn add(&mut self, x: &i32, y: i32) -> i32;

    #[allow(unused)]
    fn sub(x: &mut i32, y: i32, (t, r): (u32, u32)) {}

    fn print(&self, foo: String);

    fn show<'a>(self, s: &'a str, v: &[i32]) -> &'a str;

    fn empty_static();

    fn empty_ref(&self);
}

#[dynamic_call(baz_dynamic_call)]
pub trait Baz {}

fn main() {
    println!("Hello, world!");
}
