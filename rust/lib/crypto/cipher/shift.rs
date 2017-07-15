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
      .map(|x| (ModNum::new(x.clone(), self.m) - ModNum::new(self.key.clone() as ModValue, self.m)).v())
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
fn test_shift_cipher() {
  println_stderr!("HELLO");
  assert_eq!(true, true);
  let shift = ShiftCipher::new(1, 26);
  let encrypted = shift.encrypt(vec![0,5,10,15,20,25]);
  println_stderr!("{:?}", &encrypted);
  assert_eq!(encrypted, vec![25,4,9,14,19,24]);

/*
  TODO: something like this

  let shift = ShiftCipher::new(key, 26)
  let plain = "wewillmeetatmidnight"
  let plain_u8s = plain.to_u8s()
  let e = shift.encrypt(plain_u8s);
  let d = shift.decrypt(e);
  assert_eq!(d, plain_u8s);
  assert_eq!(d.from_u8s(), plain);
  assert_eq!(shift.decrypt(shift.encrypt(plain_u8s)), plain_u8s);
*/
}
