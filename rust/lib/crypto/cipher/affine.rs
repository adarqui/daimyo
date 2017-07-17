use math::mod_shared::{ModValue, ModModulus};
use math::mod_num::ModNum;
use crypto::crypto_system::CryptoSystem;



pub struct AffineCipher {
}


/// Affine Cipher
///
/// Let P = C = Z_m and let
///  _KS_ = {{a, b} IN Z_m x Z_m : gcd(a, m) = 1}.
///
/// For K = (a,b) in _KS_, define:
///   e_k(x) = (ax + b) mod m
///  and
///   d_k(y) = a^-1(y - b) mod m
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
    }
  }

  fn encrypt(&self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    plaintext
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    ciphertext
  }
}



#[test]
fn test_affine_cipher() {
  let affine = AffineCipher::new(&((7, 3), 26));
}
