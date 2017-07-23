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
