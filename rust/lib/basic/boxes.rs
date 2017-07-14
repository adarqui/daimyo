use std::mem;



// RAII - Resources Acquisition is Initialization



#[allow(dead_code)]
#[allow(unused_variables)]
fn create_box() {
  let box1 = Box::new(1024i32);
  let _ = mem::size_of_val(&box1);
  // box1 destroyed
}



#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_boxes() {
  let box2 = Box::new(5i32);

  // new scope
  { let box3 = Box::new(4i32); /* box3 destroyed */ }

  for _ in 0u32..1_000 {
    create_box();
  }

  // box2 destroyed
}



#[allow(dead_code)]
fn basic_boxes_ownership() {
  let immutable_box = Box::new(10u8);
  let mut mutable_box = immutable_box;
  *mutable_box = 0;
}



#[allow(dead_code)]
#[allow(unused_variables)]
fn eat_box_i32(v: Box<i32>) {
  // v is destroyed
}



#[allow(dead_code)]
#[allow(unused_variables)]
// borrows v via an immutable references
fn borrow_i32(v: &i32) {
  // v is not destroyed, since it is borrowed
}



#[allow(dead_code)]
#[allow(unused_variables)]
// borrows v via a mutable reference
fn mutable_borrow_i32(v: &mut i32) {
  // v is not destroyed, since it is borrowed
}
