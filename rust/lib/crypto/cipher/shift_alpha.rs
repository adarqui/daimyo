#![allow(unused_imports)]
use std::ascii::AsciiExt;
use std::char;
use math::mod_num::ModNum;
use crypto::cipher::shift;
use crypto::crypto_system::CryptoSystem;



pub struct ShiftCipherAlpha {
  s: shift::ShiftCipher
}


fn char_to_base(c: char) -> u32 {
  (c.to_ascii_lowercase() as u32 - 97)
}



fn base_to_char(x: u32) -> char {
  char::from_u32(x as u32 + 97).unwrap()
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

  let p: Vec<char> = "wewillmeetatmidnight".as_bytes().into_iter().map(|x| char::from_u32(*x as u32).unwrap()).collect();
  let c: Vec<char> = "hphtwwxppelextoytrse".as_bytes().into_iter().map(|x| char::from_u32(*x as u32).unwrap()).collect();

  let encrypted = shift_alpha.encrypt(p.to_owned());
  let decrypted = shift_alpha.decrypt(encrypted.to_owned());

  assert_eq!(encrypted, c);
  assert_eq!(decrypted, p);
}
