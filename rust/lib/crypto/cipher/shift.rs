#![allow(unused_imports)]
use math::mod_num;
use std::io::Write;



#[allow(dead_code)]
pub struct ShiftCipher {
  key: u8,
  m: u8
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
  pub fn new(key: u8, m: u8) -> Self {
    ShiftCipher {
      key: key,
      m: m
    }
  }
  pub fn encrypt(self, plaintext: Vec<u8>) -> Vec<u8> {
    let v: Vec<u8> = plaintext.iter().map(|x| x ^ self.key).collect();
    v
  }
  pub fn encrypt_broken(self, plaintext: &[u8]) -> &[u8] {
    // let v: Vec<u8> = plaintext.iter().map(|x| x ^ self.key).collect();
    plaintext
  }
  pub fn decrypt_broken(self, ciphertext: &[u8]) -> &[u8] {
    ciphertext
  }
}



#[test]
fn test_shift_cipher() {
  println_stderr!("HELLO");
  assert_eq!(true, true);
  let shift = ShiftCipher::new(26, 13);
  // shift.encryptBroken("hello".as_bytes());

  let encrypted = shift.encrypt(vec![0,1,2,3,4,5,6,7,8]);
  println_stderr!("{:?}", &encrypted);



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
