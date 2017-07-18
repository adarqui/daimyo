use math::mod_shared::*;
use math::mod_num::{ModuloSignedExt};
use crypto::crypto_system::CryptoSystem;

#[allow(unused_imports)]
// tests
use util::vec;



#[allow(dead_code)]
pub struct VigenereCipher {
  k: Vec<ModValue>,
  k_len: u64,
  m: ModModulus,
}



/// Vigenere Cipher
///
/// Let n be a positive integer. Define P = C = _KS_ = (Z_m)^n.
/// For a key K = (k_1, k_2, ... k_n), we defined:
///   e_k(x_1, x_2, ... x_n) = (x_1 + k_1, x_2 + k_2, ... x_n + k_n)
///  and
///   d_k(y_1, y_2, ... y_n) = (y_1 - k_1, y_2 - k_2, ... y_n - k_n)
/// where all operations are performed in Z_m
///
/// Additional notes:
/// - polyalphabetic crypto system
///
impl CryptoSystem for VigenereCipher {
  type P = ModValue;
  type C = ModValue;
  type K = (Vec<ModValue>, ModModulus);

  fn new(k_m: &(Vec<ModValue>, ModModulus)) -> Self {
    VigenereCipher {
      k: k_m.0.clone(),
      k_len: k_m.0.len() as u64,
      m: k_m.1
    }
  }

  fn encrypt(&self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let mut v: Vec<ModValue> = Vec::new();
    for (i, p) in plaintext.into_iter().enumerate() {
      let z = i as u64;
      let m = z.modulo(self.k_len);
      let k_ = self.k.get(m as usize).unwrap();
      v.push((p + k_).modulo(self.m as i64));
    }
    v
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let mut v: Vec<ModValue> = Vec::new();
    for (i, c) in ciphertext.into_iter().enumerate() {
      let z = i as u64;
      let m = z.modulo(self.k_len);
      let k_ = self.k.get(m as usize).unwrap();
      v.push((c - k_).modulo(self.m as i64));
    }
    v
  }
}



#[test]
fn test_vigenere_cipher() {
  let key = vec![02,08,15,07,04,17]; // CIPHER
  let vigenere = VigenereCipher::new(&(key, 26));

  let p = vec::string_to_vec_of_i64_m26("thiscryptosystemisnotsecure");
  let c = vec::string_to_vec_of_i64_m26("vpxzgiaxivwpubttmjpwizitwzt");

  let encrypted = vigenere.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = vigenere.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
