// IMPORTANT:
// https://doc.rust-lang.org/book/first-edition/lifetimes.html


// 'a reads "the lifetime a"
// &mut i32 .. ‘a mutable reference to an i32’
// &'a mut i32 .. ‘a mutable reference to an i32 with the lifetime 'a’
#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_lifetimes<'a, 'b>(x: &'a u8, y: &'b mut u8, z: &'a bool) -> &'a u8 {
  let p: &'a u8;
  x
}



static FOO: u64 = 100;



#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_static_lifetimes() {
  let x: &'static str = "static string";
  let y: &'static u64 = &FOO;
}



#[allow(dead_code)]
// need explicit lifetimes when working with structs that contain references:
struct Foo<'a> {
  x: &'a i32,
}

#[allow(dead_code)]
impl<'a> Foo<'a> {
  fn x(&self) -> &'a i32 { self.x }
}
