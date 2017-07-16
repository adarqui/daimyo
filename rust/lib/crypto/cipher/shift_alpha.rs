#![allow(unused_imports)]
use std::ascii::AsciiExt;
use std::char;
use math::mod_num::ModNum;
use crypto::cipher::shift;
use crypto::crypto_system::CryptoSystem;



pub struct ShiftCipherAlpha {
  s: shift::ShiftCipher,
  k: char,
}



impl CryptoSystem for ShiftCipherAlpha {
  type P = char;
  type C = char;
  type K = char;

  fn new(key: &char) -> Self {

    let key_ = key.to_ascii_lowercase();
    if !key_.is_lowercase() {
      panic!("key {} is not an alphabet character", key);
    }

    let s = shift::ShiftCipher::new(&ModNum::new(key_ as i64, 26));
    ShiftCipherAlpha {
      s: s,
      k: key_
    }
  }

  fn encrypt(&self, plaintext: Vec<char>) -> Vec<char> {
   let p64 = 
     plaintext
      .into_iter()
      .map(|x| x.to_ascii_lowercase() as i64)
      .collect();
    let c64 = self.s.encrypt(p64);
    c64.into_iter().map(|x| char::from_u32(x as u32).unwrap()).collect()
  }

  fn decrypt(&self, ciphertext: Vec<char>) -> Vec<char> {
    ciphertext
  }


}



#[test]
fn test_shift_cipher_alpha_1() {
  let shift_alpha = ShiftCipherAlpha::new(&'d');

  let p: Vec<char> = "wewillmeetatmidnight".as_bytes().into_iter().map(|x| char::from_u32(*x as u32).unwrap()).collect();
  let encrypted = shift_alpha.encrypt(p.to_owned());
}
