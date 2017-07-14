#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_slices() {
  let array = [0,1,2,3,4,5];

  // slices have type &T
  let complete_slice = &array[..];
  let middle_slies = &array[1..5];
}
