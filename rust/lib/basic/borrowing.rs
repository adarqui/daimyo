// &T: a binding that borrows something does not deallocate the resource when it goes out of scope
fn basic_borrow_immutable() {
  let x = 5;
  {
    let y = &x;
  }
  let _ = x == 5;
}



// &mut T: a ‘mutable reference’ allows you to mutate the resource you’re borrowing.
fn basic_borrow_mutable() {
  let mut x = 5;
  { // any borrow must last for a scope no greater than that of the owner.
    let y = &mut x;
    *y += 1;
  }
  let _ = x == 6;
}



fn borrow_tedious(v1: Vec<i32>, v2: Vec<i32>) -> (Vec<i32>, Vec<i32>, i32) {
  // hand back ownership, and the result of our function.
  (v1, v2, 42)
}



fn use_borrow_tedious() {
  let v1 = vec![1, 2, 3];
  let v2 = vec![1, 2, 3];

  let (v1, v2, answer) = borrow_tedious(v1, v2);
}



fn borrow_good(v1: &Vec<i32>, v2: &Vec<i32>) -> i32 {
  42
}



fn use_borrow_good() {
  let v1 = vec![1, 2, 3];
  let v2 = vec![1, 2, 3];

  let answer = borrow_good(&v1, &v2);
}



fn basic_borrow_iterator_invalidation() {
  let mut v = vec![1, 2, 3];

  for i in &v {
    println!("{}", i);
  }

  for i in &v {
    // violates rules, can't mutate while borrowed immutable:
    // v.push(34);
  }
}



fn basic_borrow_use_after_free() {
  let y: &i32;
  {
    let x = 5;
    // x does not live long enough
    // y = &x;
  }

  let y: &i32;
  let x = 5;
  // x does not live long enough
  // y = &x;

}
