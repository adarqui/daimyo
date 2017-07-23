extern crate modulo;
use modulo::mod_num::ModNum;

extern crate shift;

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate util;
use util::char;

use std::ascii::AsciiExt;



pub struct ShiftCipherAlpha {
  s: shift::ShiftCipher
}



/// Shift Cipher (mod 26)
///
/// Let P = C = _KS_ = Z_26; For 0 <= K <= 25, define
///   e_K(x) = (x + K) mod 26
///  and
///   d_K(y) = (y - K) mod 26
///  (x, y IN Z_26)
///
impl CryptoSystem for ShiftCipherAlpha {
  type P = char;
  type C = char;
  type K = char;

  fn new(key: &char) -> Self {

    let key_ = key.to_ascii_lowercase();
    if !key_.is_lowercase() {
      panic!("key {} is not an alphabet character", key);
    }

    let s = shift::ShiftCipher::new(&ModNum::new(char::char_to_base(key_) as i64, 26));
    ShiftCipherAlpha {
      s: s
    }
  }

  fn encrypt(&self, plaintext: Vec<char>) -> Vec<char> {
   let p64 = 
     plaintext
      .into_iter()
      .map(|x| char::char_to_base(x) as i64)
      .collect();
    let c64 = self.s.encrypt(p64);
    c64.into_iter().map(|x| char::base_to_char(x as u32)).collect()
  }

  fn decrypt(&self, ciphertext: Vec<char>) -> Vec<char> {
   let p64 = 
     ciphertext 
      .into_iter()
      .map(|x| char::char_to_base(x) as i64)
      .collect();
    let c64 = self.s.decrypt(p64);
    c64.into_iter().map(|x| char::base_to_char(x as u32)).collect()
  }
}
