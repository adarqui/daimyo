use num::Integer;
use std::ops::Mul;
use std::ops::Add;
use std::ops::Sub;

use mod_shared::{ModValue,ModModulus};



static MOD_ARITH_DISJOINT_SETS: &'static str = "ModNum's do not belong to the same set modulo m";



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
pub struct ModNum {
  v: ModValue,
  m: ModModulus
}



impl ModNum {
  pub fn new(v: ModValue, m: ModModulus) -> Self {
    ModNum {
      v: v,
      m: m
    }
  }
  pub fn congruent(self, rhs: Self) -> bool {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    a_is_congruent_to_b_modulo_m(self.v, rhs.v, self.m as ModValue)
  }
  pub fn v(&self) -> ModValue {
    self.v
  }
  pub fn m(&self) -> ModModulus {
    self.m
  }
}



// https://doc.rust-lang.org/std/ops/trait.Mul.html
impl Mul for ModNum {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    let a_x_b = self.v * rhs.v;
    ModNum::new(a_x_b.modulo(self.m as ModValue), self.m)
  }
}



// https://doc.rust-lang.org/std/ops/trait.Add.html
impl Add for ModNum {
  type Output = Self;

  fn add(self, rhs: Self) -> Self {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    let a_plus_b = self.v + rhs.v;
    ModNum::new(a_plus_b.modulo(self.m as ModValue), self.m)
  }
}



// https://doc.rust-lang.org/std/ops/trait.Sub.html
impl Sub for ModNum {
  type Output = ModNum;

  fn sub(self, rhs: Self) -> Self {
    if self.m != rhs.m {
      panic!(MOD_ARITH_DISJOINT_SETS);
    }
    let a_sub_b = self.v - rhs.v;
    ModNum::new(a_sub_b.modulo(self.m as ModValue), self.m)
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
modulo_signed_ext_impl! { i8 i16 i32 i64 isize u8 u16 u32 u64 usize }



/*
use std::ops::Rem;
pub fn modulo(a: ModValue, m: ModValue) -> ModValue {
  // a % m
  a.rem(m)
}
*/



pub fn a_is_congruent_to_b_modulo_m(a: ModValue, b: ModValue, m: ModValue) -> bool {
  let r1 = a.modulo(m);
  let r2 = b.modulo(m);
  r1 == r2
}



pub fn a_is_congruent_to_b_modulo_m_(a: ModValue, b: ModValue, m: ModValue) -> bool {
  let (_, r1) = a.div_rem(&m);
  let (_, r2) = b.div_rem(&m);
  r1 == r2
}



#[test]
fn test_mod_num_new() {
  assert_eq!(ModNum::new(101, 7), ModNum::new(101, 7));
  assert_ne!(ModNum::new(101, 7), ModNum::new(101, 8));
}

#[test]
fn test_mod_num_congruent() {
  assert_eq!(ModNum::new(101, 7).congruent(ModNum::new(101, 7)), true);
  assert_eq!(ModNum::new(5, 10).congruent(ModNum::new(15, 10)), true);
  assert_ne!(ModNum::new(101, 7).congruent(ModNum::new(108, 7)), false);
}

#[test]
fn test_mod_num_mul() {
  assert_eq!(ModNum::new(11, 16) * ModNum::new(13, 16), ModNum::new(15, 16));
  assert_eq!(ModNum::new(5, 10) * ModNum::new(10, 10), ModNum::new(0, 10));
}

#[test]
fn test_mod_num_add() {
  assert_eq!(ModNum::new(11, 16) + ModNum::new(13, 16), ModNum::new(8, 16));
  assert_eq!(ModNum::new(5, 10) + ModNum::new(10, 10), ModNum::new(5, 10));
}

#[test]
fn test_mod_num_subtraction() {
  /*
   * since additive inverses exist in Zm, we can also subtract elements in Zm.
   * we define a - b in Zm to be (a - b)mod m.
   */
   let a = ModNum::new(11, 31);
   let b = ModNum::new(18, 31);
   assert_eq!(a-b, ModNum::new(24, 31))
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
