#[cfg(test)]

extern crate util;
use std::char;
use util::char as util_char;
use util::vec::*;



#[test]
fn test_string_to_vec_of_i64() {
  assert_eq!(string_to_vec_of_i64("hello"), vec![104, 101, 108, 108, 111]);
}



/// convert a string to a Vec<i64>
///
pub fn string_to_vec_of_i64_m26(s: &str) -> Vec<i64> {
  s
  .as_bytes()
  .into_iter()
  .map(|x| (util_char::char_to_base(char::from_u32(*x as u32).unwrap())).clone() as i64)
  .collect()
}

#[test]
fn test_string_to_vec_of_i64_m26() {
  assert_eq!(string_to_vec_of_i64_m26("hello"), vec![07, 04, 11, 11, 14]);
}



/// convert a Vec<i64> to a String
///
pub fn vec_of_i64_to_string(v: Vec<i64>) -> String {
  let mut s = String::new();

  for i in v {
    s.push(char::from_u32(i as u32).unwrap());
  }

  s
}

#[test]
fn test_vec_of_i64_to_string() {
  assert_eq!(vec_of_i64_to_string(vec![104, 101, 108, 108, 111]), "hello");
}



/// convert a Vec<i64> mod 26 to a String
///
pub fn vec_of_i64_m26_to_string(v: Vec<i64>) -> String {
  let v_: Vec<i64> = v.into_iter().map(|c| c + 97).collect();
  vec_of_i64_to_string(v_)
}

#[test]
fn test_vec_of_i64_m64_to_string() {
  assert_eq!(vec_of_i64_m26_to_string(vec![07, 04, 11, 11, 14]), "hello");
}
