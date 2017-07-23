#[cfg(test)]

extern crate util;
use util::vec::*;



#[test]
fn test_string_to_vec_of_i64() {
  assert_eq!(string_to_vec_of_i64("hello"), vec![104, 101, 108, 108, 111]);
}

#[test]
fn test_string_to_vec_of_i64_m26() {
  assert_eq!(string_to_vec_of_i64_m26("hello"), vec![07, 04, 11, 11, 14]);
}

#[test]
fn test_vec_of_i64_to_string() {
  assert_eq!(vec_of_i64_to_string(vec![104, 101, 108, 108, 111]), "hello");
}

#[test]
fn test_vec_of_i64_m64_to_string() {
  assert_eq!(vec_of_i64_m26_to_string(vec![07, 04, 11, 11, 14]), "hello");
}
