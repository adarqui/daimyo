#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_arrays() {
  // arrays have type [T; N] where N is the compile time length
  let arr_u8: [u8; 2] = [0, 1];
  let arr_bool = [false, true];

  let arr_41s: [u8; 1024] = [0x41; 1024];
  let arr_41s_0 = arr_41s[0];
}



#[allow(dead_code)]
fn basic_arrays_iteration() {
  let arr_41s: [u8; 1024] = [0x41; 1024];
  for c in arr_41s.iter() {
    println!("{}", c);
  }

  let arr1 = [1,2,3];
  let _ = arr1.iter().any(|&x| x == 2);
  let _ = arr1.into_iter().any(|&x| x == 2);
}
