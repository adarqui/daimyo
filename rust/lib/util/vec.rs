use std::char;



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
fn test_string_to_char_of_vec() {
  assert_eq!(string_to_vec_of_char("hello"), vec!['h','e','l','l','o']);
}
