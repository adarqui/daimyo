use std::fmt;



#[allow(dead_code)]
struct Point {
  x: i64,
  y: i64
}



// https://doc.rust-lang.org/std/fmt/trait.Display.html
impl fmt::Display for Point {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "({}, {})", self.x, self.y)
  }
}



#[test]
fn test_point_display() {
  let p = Point{ x: 0, y: 1 };
  assert_eq!("(0, 1)".to_owned(), format!("{}", p));
}
