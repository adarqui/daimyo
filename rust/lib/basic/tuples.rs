#[allow(dead_code)]
#[allow(unused_variables)]



fn basic_tuples() {
  let tup: (u8, bool) = (0, false);
  let tup_0 = tup.0;
  let tup_1 = tup.1;

  let (destruct_x, destruct_y) = tup;

  let mut mut_tup: (u8, bool) = (1, true);
  mut_tup.0 = tup.0;
  mut_tup.1 = tup.1;

  let tup3 = (false, true, false, true);
  let tup3_3 = tup3.3;

  let single_element_tuple = (true,);
}
