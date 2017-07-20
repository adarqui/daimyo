#[allow(unused_imports)]
use num::Integer;
use math::mod_shared::{ModValue, ModModulus};
use math::mod_num::{ModuloSignedExt};
use math::matrix;
use crypto::crypto_system::CryptoSystem;



pub struct HillCipher {
  key: matrix::Matrix,
  m: ModModulus
}



/// Hill Cipher
///
/// Let n >= 2 be an integer. Let P = C = (Z_m)^n and let
///  _KS_ = {m x m invertible matrices over Z_m}.
/// For a key K, we define:
///  e_k(x) = x*K
///  d_k(y) = y*K_inv
/// where all operations are performed in Z_m
///
impl CryptoSystem for HillCipher {
  type P = ModValue;
  type C = ModValue;
  type K = (matrix::Matrix, ModModulus);

  fn new(k_m: &(matrix::Matrix, ModModulus)) -> Self {
    let k = k_m.0.to_owned();
    let m = k_m.1;
    HillCipher {
      key: k,
      m: m
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
fn test_hill_cipher() {

  let m_v: Vec<isize> = vec![
    11,8,
    3,7];

  let hill = HillCipher::new(&(matrix::Matrix::new(2, 2, m_v), 26));

  let p = vec![9,20,11,24];
  let c = vec![3,4,11,22];

  let encrypted = hill.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = hill.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
