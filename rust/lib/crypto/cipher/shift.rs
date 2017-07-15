#![allow(unused_imports)]
use math::mod_num::{ModNum};
use math::mod_shared::*;
use std::io::Write;
use std::vec::Vec;



#[allow(dead_code)]
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
impl ShiftCipher {
  pub fn new(key: ModModulus, m: ModModulus) -> Self {
    if key >= m || key == 0 || m == 0 {
      panic!("ShiftCipher::new -> key {} >= m {}", key, m);
    }
    ShiftCipher {
      key: key,
      m: m
    }
  }
  pub fn encrypt(self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let v: Vec<ModValue> =
      plaintext
      .into_iter()
      .map(|x| (ModNum::new(x.clone(), self.m) + ModNum::new(self.key.clone() as ModValue, self.m)).v())
      .collect();
    v
  }
  pub fn encrypt_broken(self, plaintext: &[ModValue]) -> &[ModValue] {
    // let v: Vec<u8> = plaintext.iter().map(|x| x ^ self.key).collect();
    plaintext
  }
  pub fn decrypt_broken(self, ciphertext: &[ModValue]) -> &[ModValue] {
    ciphertext
  }
}



#[test]
#[should_panic]
fn test_shift_cipher_should_panic() {
  let _ = ShiftCipher::new(1, 0);
  let _ = ShiftCipher::new(0, 1);
  let _ = ShiftCipher::new(10, 9);
}

#[test]
fn test_shift_cipher_1() {

  let shift = ShiftCipher::new(1, 26);
  let encrypted = shift.encrypt(vec![00,05,10,15,20,25]);
  assert_eq!(encrypted, vec![01,06,11,16,21,00]);
  // println_stderr!("{:?}", &encrypted);
}

#[test]
fn test_shift_cipher_2() {

  let shift = ShiftCipher::new(11, 26);

  // WEWILLMEETATMIDNIGHT
  let mut v1 = vec![22,04,22,08,11,11,12,04,04,19];
  let mut v2 = vec![00,19,12,08,03,13,08,06,07,19];
  v1.append(&mut v2);

  let encrypted = shift.encrypt(v1);

  let mut v1 = vec![07,15,07,19,22,22,23,15,15,04];
  let mut v2 = vec![11,04,23,19,14,24,19,17,18,04];
  v1.append(&mut v2);

  assert_eq!(encrypted, v1);
}
