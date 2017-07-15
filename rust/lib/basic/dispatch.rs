// https://doc.rust-lang.org/book/first-edition/trait-objects.html



trait Foo {
  fn method(&self) -> String;
}

impl Foo for u8 {
  fn method(&self) -> String { format!("u8: {}", *self) }
}

impl Foo for String {
  fn method(&self) -> String { format!("string: {}", *self) }
}



/*
 * static dispatch
 */
fn static_dispatch<T: Foo>(foo: T) -> () {
  foo.method();
}

fn do_static_dispatch() {
  let x = 0x41u8;
  let y = "hello".to_string();
  static_dispatch(x);
  static_dispatch(y);
}



/*
 * dynamic dispatch
 *
 * precise type can only be known at run time.
 * allows us to pass any argument that satisifies the trait.
 */
fn dynamic_dispatch(foo: &Foo) -> () {
  foo.method();
}

fn do_dynamic_dispatch() {
  let y = "hello".to_string();
  dynamic_dispatch(&y as &Foo);
  // or
  dynamic_dispatch(&y);
}
