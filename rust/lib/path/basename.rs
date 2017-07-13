use path::misc;



pub fn basename(path: &str) -> &str {
  let mut p = &path[0..];
  let mut i: usize = 0;
  for c in path.chars() {
    if misc::is_path_separator(c) {
      p = &path[i+1..];
    }
    i = i + 1;
  }
  p
}



#[cfg(test)]
mod tests {
  use path::basename::*;
  #[test]
  fn test_basename() {
    assert_eq!(basename("/tmp"), "tmp");
    assert_eq!(basename("/tmp/1"), "1");
  }
}
