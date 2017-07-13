use std::ops::Rem;



pub trait ModuloSignedExt {
    fn modulox(&self, n: Self) -> Self;
}

macro_rules! modulo_signed_ext_impl {
    ($($t:ty)*) => ($(
        impl ModuloSignedExt for $t {
            #[inline]
            fn modulox(&self, n: Self) -> Self {
                (self % n + n) % n
            }
        }
    )*)
}
modulo_signed_ext_impl! { i8 i16 i32 i64 }



/*
pub fn modulo(a: i64, m: i64) -> i64 {
  // a % m
  a.rem(m)
}
*/



pub fn a_is_congruent_to_b_modulo_m(a: i64, b: i64, m: i64) -> bool {
  let r1 = a.modulox(m);
  let r2 = b.modulox(m);
  r1 == r2
}



#[test]
fn test_modulo() {
  assert_eq!(101.modulox(7), 3);
  assert_eq!((-101).modulox(7), 4);
  // assert_eq!(-101 % 7, -3);
}

#[test]
fn test_a_is_congruent_to_b_modulo_m() {
  assert_eq!(a_is_congruent_to_b_modulo_m(101, -101, 7), false);
  assert_eq!(a_is_congruent_to_b_modulo_m(101, 101, 7), true)
}
