// https://doc.rust-lang.org/book/first-edition/structs.html



struct Nil;



struct Tuple(u8, bool);



struct Record {
  member: u8
}



impl Record {
  pub fn new(value: u8) -> Self {
    Record {
      member: value
    }
  }
}



fn basic_record() {
  let rec = Record::new(0);
}




struct Inches(i32);



fn basic_tuple_struct() {
  let length = Inches(10);
  let Inches(integer_length) = length;
  let _ = integer_length * 100;
}



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
