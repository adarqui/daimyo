extern crate modulo;
extern crate crypto_system;

use std::vec::Vec;
use modulo::mod_num::{ModNum};
use modulo::mod_shared::{ModValue,ModModulus};
use crypto_system::CryptoSystem;



#[allow(dead_code)]
#[derive(Copy, Clone)]
pub struct ShiftCipher {
  key: ModModulus,
  m: ModModulus 
}




/// Shift Cipher
///
/// Let P = C = _KS_ = Z_m; For 0 <= K <= m, define
///   e_K(x) = (x + K) mod m
///  and
///   d_K(y) = (y - K) mod m
///  (x, y IN Z_m)
///
impl CryptoSystem for ShiftCipher {

  type P = ModValue;
  type C = ModValue;
  type K = ModNum;

  fn new(mn: &ModNum) -> Self {
    let key = mn.v() as u64;
    let m   = mn.m();
    if key >= m || key == 0 || m == 0 {
      panic!("ShiftCipher::new -> key {} >= m {}", key, m);
    }
    ShiftCipher {
      key: key,
      m: m
    }
  }

  fn encrypt(&self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let v: Vec<ModValue> =
      plaintext
      .into_iter()
      .map(|x| (ModNum::new(x.clone(), self.m) + ModNum::new(self.key.clone() as ModValue, self.m)).v())
      .collect();
    v
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let v: Vec<ModValue> =
      ciphertext
      .into_iter()
      .map(|x| (ModNum::new(x.clone(), self.m) - ModNum::new(self.key.clone() as ModValue, self.m)).v())
      .collect();
    v
  }
}
