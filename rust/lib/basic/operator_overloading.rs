// https://doc.rust-lang.org/book/first-edition/operators-and-overloading.html



use std::ops::Add;

struct Point(i32, i32);

impl Add for Point {
  type Output = Point;

  fn add(self, rhs: Point) -> Point {
    Point(self.0 + rhs.0, self.1 + rhs.1)
  }
}
