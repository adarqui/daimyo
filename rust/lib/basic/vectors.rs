#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_vectors() {
  // vectors always allocate their data on the heap
  // behave like arrays but size may change by push()'ing more elements to them
  let vec_u8: Vec<u8> = vec![0,1,2];

  let _ = vec_u8.iter().any(|&x| x == 2);
  let _ = vec_u8.into_iter().any(|x| x == 2);

  let vec_41s = vec![0x41; 10];

  // you can only index a vec with a usize index
  let idx: usize = 5;
  let _ = vec_41s[idx];
}




#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_vector_iteration() {
  // vectors always allocate their data on the heap
  let v: Vec<u8> = vec![0,1,2,3,4,5];

  /*
   * can iterate multiple times because we haven't taken ownership of it,
   * we're borrowing a reference to v
   */
  for referenced_value in &v {
  }

  for referenced_value in &v {
  }

  for referenced_value in &v {
  }

  let mut v_m: Vec<u8> = v;
  for referenced_value in &mut v_m {
    *referenced_value = 0;
  }

  // take ownership of v_m
  for i in v_m {
  }
}
