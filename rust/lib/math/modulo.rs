use num::Integer;
use std::ops::Mul;
use std::ops::Add;
use std::ops::Sub;



static MOD_ARITH_DISJOINT_SETS: &'static str = "ModArith's do not belong to the same set modulo m";



/*
 * A number belonging to some modulo arithmetic set
 */
// https://doc.rust-lang.org/std/marker/trait.Copy.html
// https://doc.rust-lang.org/core/clone/trait.Clone.html
// https://doc.rust-lang.org/std/fmt/trait.Debug.html
// https://doc.rust-lang.org/core/cmp/trait.Eq.html
// https://doc.rust-lang.org/core/cmp/trait.PartialEq.html
#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(Eq)]
#[derive(PartialEq)]
pub struct ModArith {
  v: i64,
  m: u64
}



impl ModArith {
  pub fn new(v: i64, m: u64) -> Self {
    ModArith {
      v: v,
      m: m
    }
  }
  pub fn congruent(self, rhs: Self) -> bool {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    a_is_congruent_to_b_modulo_m(self.v, rhs.v, self.m as i64)
  }
}



// https://doc.rust-lang.org/std/ops/trait.Mul.html
impl Mul for ModArith {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    let a_x_b = self.v * rhs.v;
    ModArith::new(a_x_b.modulo(self.m as i64), self.m)
  }
}



// https://doc.rust-lang.org/std/ops/trait.Add.html
impl Add for ModArith {
  type Output = Self;

  fn add(self, rhs: Self) -> Self {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    let a_plus_b = self.v + rhs.v;
    ModArith::new(a_plus_b.modulo(self.m as i64), self.m)
  }
}



// https://doc.rust-lang.org/std/ops/trait.Sub.html
impl Sub for ModArith {
  type Output = ModArith;

  fn sub(self, rhs: Self) -> Self {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    let a_sub_b = self.v - rhs.v;
    ModArith::new(a_sub_b.modulo(self.m as i64), self.m)
  }
}




// some arithmetic modulus I ripped
pub trait ModuloSignedExt {
  fn modulo(&self, n: Self) -> Self;
}

macro_rules! modulo_signed_ext_impl {
  ($($t:ty)*) => ($(
    impl ModuloSignedExt for $t {
      #[inline]
        fn modulo(&self, n: Self) -> Self {
          (self % n + n) % n
        }
      }
  )*)
}
modulo_signed_ext_impl! { i8 i16 i32 i64 }



/*
use std::ops::Rem;
pub fn modulo(a: i64, m: i64) -> i64 {
  // a % m
  a.rem(m)
}
*/



pub fn a_is_congruent_to_b_modulo_m(a: i64, b: i64, m: i64) -> bool {
  let r1 = a.modulo(m);
  let r2 = b.modulo(m);
  r1 == r2
}



pub fn a_is_congruent_to_b_modulo_m_(a: i64, b: i64, m: i64) -> bool {
  let (_, r1) = a.div_rem(&m);
  let (_, r2) = b.div_rem(&m);
  r1 == r2
}



#[test]
fn test_mod_arith_new() {
  assert_eq!(ModArith::new(101, 7), ModArith::new(101, 7));
  assert_ne!(ModArith::new(101, 7), ModArith::new(101, 8));
}

#[test]
fn test_mod_arith_congruent() {
  assert_eq!(ModArith::new(101, 7).congruent(ModArith::new(101, 7)), true);
  assert_eq!(ModArith::new(5, 10).congruent(ModArith::new(15, 10)), true);
  assert_ne!(ModArith::new(101, 7).congruent(ModArith::new(108, 7)), false);
}

#[test]
fn test_mod_arith_mul() {
  assert_eq!(ModArith::new(11, 16) * ModArith::new(13, 16), ModArith::new(15, 16));
  assert_eq!(ModArith::new(5, 10) * ModArith::new(10, 10), ModArith::new(0, 10));
}

#[test]
fn test_mod_arith_add() {
  assert_eq!(ModArith::new(11, 16) + ModArith::new(13, 16), ModArith::new(8, 16));
  assert_eq!(ModArith::new(5, 10) + ModArith::new(10, 10), ModArith::new(5, 10));
}

#[test]
fn test_mod_arith_subtraction() {
  /*
   * since additive inverses exist in Zm, we can also subtract elements in Zm.
   * we define a - b in Zm to be (a - b)mod m.
   */
   let a = ModArith::new(11, 31);
   let b = ModArith::new(18, 31);
   assert_eq!(a-b, ModArith::new(24, 31))
}


#[test]
fn test_modulo() {
  assert_eq!(101.modulo(7), 3);
  assert_eq!((-101).modulo(7), 4);
}

#[test]
fn test_a_is_congruent_to_b_modulo_m() {
  assert_eq!(a_is_congruent_to_b_modulo_m(101, -101, 7), false);
  assert_eq!(a_is_congruent_to_b_modulo_m(101, 101, 7), true)
}

#[test]
fn test_a_is_congruent_to_b_modulo_m_() {
  assert_eq!(a_is_congruent_to_b_modulo_m_(101, -101, 7), false);
  assert_eq!(a_is_congruent_to_b_modulo_m_(101, 101, 7), true)
}
