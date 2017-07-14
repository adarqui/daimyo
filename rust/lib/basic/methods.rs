// https://doc.rust-lang.org/book/first-edition/method-syntax.html



use std;



struct Point(f64, f64);

impl Point {
  fn new() -> Self {
    Point::origin()
  }
  fn origin() -> Self {
    Point(0.0, 0.0)
  }
}

fn basic_methods() {
  let point = Point::new();
}




struct Circle {
  x: f64,
  y: f64,
  radius: f64,
}

impl Circle {
  fn new(x: f64, y: f64, radius: f64) -> Self {
    Circle{x, y, radius}
  }
  fn area(&self) -> f64 {
    std::f64::consts::PI * (self.radius * self.radius)
  }
  fn reference(&self) -> &Circle {
    self
  }
  fn reference2(&self) -> &Circle {
    self
  }
  fn reference3(&self) -> &Self {
    self
  }
  fn mutable_reference(&mut self) -> &mut Circle {
    self.x *= self.x;
    self
  }
  fn takes_ownership(self) -> Circle {
    self
  }
}

fn basic_methods_circle() {
  let c = Circle{x: 1.0, y: 2.0, radius: 10.0};
  let _ = c.area();
  let _ = c.reference().reference2().reference3();
}
