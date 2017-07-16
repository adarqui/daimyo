#![allow(unused_imports)]
use std::io::Write;
use std::vec::Vec;
use math::mod_num::{ModNum};
use math::mod_shared::*;
use crypto::crypto_system::CryptoSystem;



#[allow(dead_code)]
#[derive(Copy, Clone)]
pub struct ShiftCipher {
  key: ModModulus,
  m: ModModulus 
}




/// Shift Cipher
///
/// Let P = C = _KS_ = Z_26; For 0 <= K <= 25, define
///   e_K(x) = (x + K) mod 26
///  and
///   d_K(y) = (y - K) mod 26
///  (x, y IN Z_26)
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



#[test]
#[should_panic]
fn test_shift_cipher_should_panic() {
  let _ = ShiftCipher::new(&ModNum::new(1, 0));
  let _ = ShiftCipher::new(&ModNum::new(0, 1));
  let _ = ShiftCipher::new(&ModNum::new(10, 9));
}

#[test]
fn test_shift_cipher_1() {

  let shift = ShiftCipher::new(&ModNum::new(1, 26));
  let p = vec![00,05,10,15,20,25];
  let c = vec![01,06,11,16,21,00];

  let encrypted = shift.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = shift.decrypt(c.to_owned());
  assert_eq!(decrypted, p);

  assert_eq!(shift.decrypt(shift.encrypt(p.clone())), p);
}

#[test]
fn test_shift_cipher_2() {

  let shift = ShiftCipher::new(&ModNum::new(11, 26));

  // WEWILLMEETATMIDNIGHT
  let mut p = vec![22,04,22,08,11,11,12,04,04,19];
  let mut p2 = vec![00,19,12,08,03,13,08,06,07,19];
  p.append(&mut p2);

  let encrypted = shift.encrypt(p.to_owned());

  let mut c = vec![07,15,07,19,22,22,23,15,15,04];
  let mut c2 = vec![11,04,23,19,14,24,19,17,18,04];
  c.append(&mut c2);

  assert_eq!(encrypted, c.to_owned());

  assert_eq!(shift.decrypt(shift.encrypt(p.to_owned())), p);
}
