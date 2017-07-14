use std;



// https://doc.rust-lang.org/std/mem/fn.size_of_val.html
fn basic_size_of_val() {
  let n64: u64 = 0;
  let x: usize = std::mem::size_of_val(&n64);
}
