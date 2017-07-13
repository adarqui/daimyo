pub fn is_path_separator(c: char) -> bool {
  match c {
    '/' => true,
    _   => false,
  }
}



#[cfg(test)]
mod tests {
  use path::misc::*;
  #[test]
  fn test_is_path_separator() {
    assert_eq!(is_path_separator('/'), true);
    assert_eq!(is_path_separator('z'), false);
  }
}
