use std::ops::Rem;



pub fn modulo(a: i64, m: i64) -> i64 {
  // a % m
  a.rem(m)
}



pub fn a_is_congruent_to_b_modulo_m(a: i64, b: i64, m: i64) -> bool {
  let r1 = modulo(a, m);
  let r2 = modulo(b, m);
  r1 == r2
}



#[test]
fn test_modulo() {
  assert_eq!(modulo(101, 7), 3);
  assert_eq!(modulo(-101, 7), 4);
  // assert_eq!(-101 % 7, -3);
}

#[test]
fn test_a_is_congruent_to_b_modulo_m() {
  assert_eq!(a_is_congruent_to_b_modulo_m(101, -101, 7), false);
  assert_eq!(a_is_congruent_to_b_modulo_m(101, 101, 7), true)
}
