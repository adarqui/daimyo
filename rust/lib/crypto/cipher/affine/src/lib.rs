extern crate math;
use math::divisibility;

extern crate modulo;
use modulo::mod_shared::{ModValue, ModModulus};
use modulo::mod_num::{ModuloSignedExt};

extern crate crypto_system;
use crypto_system::CryptoSystem;



pub struct AffineCipher {
  e: (ModValue, ModValue),
  d: (ModValue, ModValue),
  m: ModModulus
}


/// Affine Cipher
///
/// Let P = C = Z_m and let
///  _KS_ = {{a, b} IN Z_m x Z_m : gcd(a, m) = 1}.
///
/// For K = (a,b) in _KS_, define:
///   e_k(x) = (ax + b) mod m
///  and
///   d_k(y) = inv_a * (y - b) mod m
///
/// (x, y IN Z_m)
///
impl CryptoSystem for AffineCipher {
  type P = ModValue;
  type C = ModValue;
  type K = ((ModValue, ModValue), ModModulus);

  fn new(k_m: &((ModValue, ModValue), ModModulus)) -> Self {
    let k = k_m.0;
    let m = k_m.1;
    AffineCipher {
      e: (k.0, k.1),
      d: (divisibility::mod_inv(k.0 as isize, m as isize) as ModValue, k.1),
      m: m
    }
  }

  fn encrypt(&self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let (a,b) = self.e;
    let v: Vec<ModValue> =
      plaintext
      .into_iter()
      // e_k(x) = ax + b (mod m)
      .map(|x| (a * x + b).modulo(self.m as i64))
      .collect();
    v
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let (inv_a,b) = self.d;
    let v: Vec<ModValue> =
      ciphertext
      .into_iter()
      // d_k(y) = inv_a * (y - b) (mod m)
      .map(|y| (inv_a * (y - b)).modulo(self.m as i64))
      .collect();
    v
  }
}



#[test]
fn test_affine_cipher() {
  let affine = AffineCipher::new(&((7, 3), 26));

  let p = vec![07, 14, 19];
  let c = vec![00, 23, 06];

  let encrypted = affine.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = affine.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
