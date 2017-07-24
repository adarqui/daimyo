extern crate modulo;

use modulo::mod_shared::ModValue;
use modulo::mod_num::ModuloSignedExt;

extern crate crypto_system;
use crypto_system::NonSynchronousStreamCipher;



#[allow(dead_code)]
pub struct AutoKeyNonSynchronousStreamCipher {
  k: ModValue
}



/// Autokey Non-Synchronous Stream Cipher
///
/// Let P = C = _KS_ = L = Z_26. Let z_1 = K, and define z_i = x_(i-1) for all i >= 2.
///
/// For 0 <= z <= 25, define
///  e_z(x) = (x + z) mod 26
///  d_z(y) = (y - z) mod 26
/// (x, y IN Z_26)
///
impl NonSynchronousStreamCipher for AutoKeyNonSynchronousStreamCipher {
  type P = ModValue;
  type C = ModValue;
  type K = ModValue;
  type L = ModValue;

  fn new(k: &ModValue) -> Self {

    assert!(k >= &0 && k <= &25, "k out of bounds");

    AutoKeyNonSynchronousStreamCipher {
      k: k.to_owned(),
    }
  }

  fn encrypt(&mut self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let mut v: Vec<ModValue> = Vec::with_capacity(plaintext.len());
    for p in plaintext.into_iter() {
      let g = self.g();
      // set the new k for g()
      self.k = p;
      v.push((p + g).modulo(26));
    }      
    v
  }

  fn decrypt(&mut self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let mut v: Vec<ModValue> = Vec::with_capacity(ciphertext.len());
    for c in ciphertext.into_iter() {
      let g = self.g();
      let p = (c - g).modulo(26);
      self.k = p;
      v.push(p);
    }
    v
  }

  fn g(&mut self) -> ModValue {
    self.k
  }
}
