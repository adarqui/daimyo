use std::char;
use char as util_char;



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



/// convert a string to a Vec<i64>
///
pub fn string_to_vec_of_i64(s: &str) -> Vec<i64> {
  s
  .as_bytes()
  .into_iter()
  .map(|x| x.clone() as i64)
  .collect()
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



/// convert a Vec<i64> to a String
///
pub fn vec_of_i64_to_string(v: Vec<i64>) -> String {
  let mut s = String::new();

  for i in v {
    s.push(char::from_u32(i as u32).unwrap());
  }

  s
}



/// convert a Vec<i64> mod 26 to a String
///
pub fn vec_of_i64_m26_to_string(v: Vec<i64>) -> String {
  let v_: Vec<i64> = v.into_iter().map(|c| c + 97).collect();
  vec_of_i64_to_string(v_)
}
