struct Point(f64, f64);



impl Point {
  fn new() -> Self {
    Point::origin()
  }
  fn origin() -> Self {
    Point(0.0, 0.0)
  }
}



#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_methods() {
  let point = Point::new();
}
