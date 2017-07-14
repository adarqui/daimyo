// https://doc.rust-lang.org/book/first-edition/generics.html



use std;



fn basic_generics() {
  let v = Vec::<bool>::new();
}



enum Maybe<T> {
  Just(T),
  Nothing
}



fn basic_generic_function<T>(a: T) -> Maybe<T> {
  Maybe::Just(a)
}



struct Point<T> {
  x: T,
  y: T
}

impl<T> Point<T> {
  fn swap(&mut self) {
    std::mem::swap(&mut self.x, &mut self.y);
  }
}

fn basic_generic_points() {
  let p = Point{x: 0, y: 1};
  // error: p.swap();

  let mut p = Point{x: 0, y: 1};
  p.swap();
}
