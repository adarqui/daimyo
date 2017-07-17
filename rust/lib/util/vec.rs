use std::char;
use crypto::char as crypto_char;



/// convert a string to a Vec<char>
///
pub fn string_to_vec_of_char(s: &str) -> Vec<char> {
  s
  .as_bytes()
  .into_iter()
  .map(|x| char::from_u32(*x as u32)
  .unwrap())
  .collect()
}

#[test]
fn test_string_to_vec_of_char() {
  assert_eq!(string_to_vec_of_char("hello"), vec!['h','e','l','l','o']);
}



/// convert a string to a Vec<i64>
///
pub fn string_to_vec_of_i64(s: &str) -> Vec<i64> {
  s
  .as_bytes()
  .into_iter()
  .map(|x| x.clone() as i64)
  .collect()
}

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
  .map(|x| (crypto_char::char_to_base(char::from_u32(*x as u32).unwrap())).clone() as i64)
  .collect()
}

#[test]
fn test_string_to_vec_of_i64_m26() {
  assert_eq!(string_to_vec_of_i64_m26("hello"), vec![07, 04, 11, 11, 14]);
}
