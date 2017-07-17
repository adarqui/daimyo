#![allow(unused_imports)]
use std::ascii::AsciiExt;
use std::char;
use math::mod_num::ModNum;
use crypto::cipher::shift;
use crypto::crypto_system::CryptoSystem;
use util::vec;
use std::io::Write;



pub struct ShiftCipherAlpha {
  s: shift::ShiftCipher
}


fn char_to_base(c: char) -> u32 {
  (c.to_ascii_lowercase() as u32 - 97)
}



fn base_to_char(x: u32) -> char {
  char::from_u32(x as u32 + 97).unwrap()
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

    let s = shift::ShiftCipher::new(&ModNum::new(char_to_base(key_) as i64, 26));
    ShiftCipherAlpha {
      s: s
    }
  }

  fn encrypt(&self, plaintext: Vec<char>) -> Vec<char> {
   let p64 = 
     plaintext
      .into_iter()
      .map(|x| char_to_base(x) as i64)
      .collect();
    let c64 = self.s.encrypt(p64);
    c64.into_iter().map(|x| base_to_char(x as u32)).collect()
  }

  fn decrypt(&self, ciphertext: Vec<char>) -> Vec<char> {
   let p64 = 
     ciphertext 
      .into_iter()
      .map(|x| char_to_base(x) as i64)
      .collect();
    let c64 = self.s.decrypt(p64);
    c64.into_iter().map(|x| base_to_char(x as u32)).collect()
  }
}



#[test]
fn test_shift_cipher_alpha_1() {
  let shift_alpha = ShiftCipherAlpha::new(&base_to_char(11));

  let p: Vec<char> = vec::string_to_vec_of_char("wewillmeetatmidnight");
  let c: Vec<char> = vec::string_to_vec_of_char("hphtwwxppelextoytrse");

  let encrypted = shift_alpha.encrypt(p.to_owned());
  let decrypted = shift_alpha.decrypt(encrypted.to_owned());

  assert_eq!(encrypted, c);
  assert_eq!(decrypted, p);
}

#[test]
fn test_shift_cipher_alpha_exhaustive() {

  // a necessary condition for a crypto system to be secure is that an exhaustive key search should be infeasable

  let p: Vec<char> = vec::string_to_vec_of_char("astitchintimesavesnine");
  let c: Vec<char> = vec::string_to_vec_of_char("jbcrclqrwcrvnbjenbwrwn");

  let mut matched_i = 0;

  for i in 1..25 {
    let shift_alpha = ShiftCipherAlpha::new(&base_to_char(i));
    let decrypted = shift_alpha.decrypt(c.to_owned());
    if decrypted == p {
      matched_i = i
    }
  }

  assert_eq!(matched_i, 9);
}
