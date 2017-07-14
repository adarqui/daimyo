// IMPORTANT:
// https://doc.rust-lang.org/book/first-edition/mutability.html



fn basic_mutability() {
  let x = 5;
  // x = 6 .. can't rebind x

  let mut x = 5;
  x = 6; // can rebind x

  let mut x = 5;
  let mut z = 7;
  let y = &mut x;
  *y = 6;
  // fails: re-assignment of immutable variable y
  // y = &mut z;

  let mut x = 5;
  let mut z = 7;
  let mut y = &mut x;
  *y = 6;
  y = &mut z;
}



/*
 * You may have one or the other of these two kinds of borrows, but not both at the same time:
 *
 * - one or more references (&T) to a resource,
 * - exactly one mutable reference (&mut T).
 */



struct Point {
  x: i32,
  y: i32,
  // can't have mixed mut/immut fields in a struct, mutability is in its binding: let mut p = Point {..}
  // mut y: i32
  // y: Cell<i32> - can emmulate field level mutability
}



// structs can contain mutable references
struct PointRef<'a> {
  x: &'a mut i32,
  y: &'a mut i32,
}



fn basic_mutability_structs() {
  let mut point = Point { x: 0, y: 0 };
  {
    let r = PointRef { x: &mut point.x, y: &mut point.y };

    *r.x = 5;
    *r.y = 6;
  }

  let _ = point.x == 5 && point.y == 6;
}
