#[cfg(test)]

extern crate math;
use math::divisibility::*;

#[test]
fn test_euclidean_algorithm() {
  assert_eq!(euclidean_algorithm(13, 5), 2);
}

#[test]
fn test_mod_inv() {
  assert_eq!(mod_inv(1, 26), 1);
  assert_eq!(mod_inv(3, 26), 9);
  assert_eq!(mod_inv(5, 26), 21);
  assert_eq!(mod_inv(7, 26), 15);
  assert_eq!(mod_inv(11, 26), 19);
  assert_eq!(mod_inv(25, 26), 25);
}
