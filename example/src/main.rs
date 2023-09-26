use dynamic_call::dynamic_call;

#[dynamic_call(bar_dynamic_call)]
pub trait Bar {
    #[dynamic_call::skip]
    fn cool(&self, x: i32) -> i32;

    fn add(&mut self, x: &i32, y: i32) -> i32;

    fn sub(x: &mut i32, y: i32) {}
}

fn main() {
    println!("Hello, world!");
}
