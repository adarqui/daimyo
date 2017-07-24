extern crate modulo;
use modulo::mod_shared::{ModValue,ModModulus};
use modulo::mod_num::{ModuloSignedExt};

extern crate crypto_system;
use crypto_system::SynchronousStreamCipher;



#[allow(dead_code)]
pub struct VigenereSynchronousStreamCipher {
  k: Vec<ModValue>,
  k_len: u64,
  m: ModModulus,
  zi: u64,
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
impl SynchronousStreamCipher for VigenereSynchronousStreamCipher {
  type P = ModValue;
  type C = ModValue;
  type K = (Vec<ModValue>, ModModulus);
  type L = ModValue;

  fn new(k_m: &(Vec<ModValue>, ModModulus)) -> Self {
    VigenereSynchronousStreamCipher {
      k: k_m.0.clone(),
      k_len: k_m.0.len() as u64,
      m: k_m.1,
      zi: 0,
    }
  }

  fn encrypt(&mut self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let mut v: Vec<ModValue> = Vec::with_capacity(plaintext.len());
    for p in plaintext.into_iter() {
      let k_ = self.g();
      v.push((p + k_).modulo(self.m as i64));
    }
    v
  }

  fn decrypt(&mut self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let mut v: Vec<ModValue> = Vec::with_capacity(ciphertext.len());
    for c in ciphertext.into_iter() {
      let k_ = self.g();
      v.push((c - k_).modulo(self.m as i64));
    }
    v
  }

  /// z_i = { k_i      if 1 <= i <= m
  ///       { z_i - m  if i >= m + 1
  ///
  fn g(&mut self) -> ModValue {
    let z = self.zi.modulo(self.k_len);
    self.zi += 1;
    self.k.get(z as usize).unwrap().to_owned()
  }
}
