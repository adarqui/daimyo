#[allow(unused_imports)]
use num::Integer;
// use std::io::Write;
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
    assert_eq!(plaintext.len().modulo(self.key.cols()), 0); 
    let pcopy = plaintext.to_owned();
    let mut ciphertext: Vec<ModValue> = Vec::with_capacity(plaintext.len());

    let mut iter = pcopy.chunks(self.key.cols());
    loop {
      match iter.next() {
        Some(e) => {
          let mut v: Vec<i64> = Vec::with_capacity(self.key.cols());
          v.extend_from_slice(e);
          let v: Vec<isize> = v.iter().map(|x| x.to_owned() as isize).collect();
          let m = &matrix::Matrix::new(1, self.key.cols(), v) * &self.key;

          for i in m.entries() {
            ciphertext.push(i.modulo(self.m as isize) as ModValue)
          }

        }
        _       => break
      }
    }
    ciphertext
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    assert_eq!(ciphertext.len().modulo(self.key.cols()), 0); 
    let pcopy = ciphertext.to_owned();
    let mut plaintext: Vec<ModValue> = Vec::with_capacity(ciphertext.len());

    let mut iter = pcopy.chunks(self.key.cols());
    loop {
      match iter.next() {
        Some(e) => {
          let mut v: Vec<i64> = Vec::with_capacity(self.key.cols());
          v.extend_from_slice(e);
          let v: Vec<isize> = v.iter().map(|x| x.to_owned() as isize).collect();
          let m = &matrix::Matrix::new(1, self.key.cols(), v) * &self.key.inverse_mod_unsafe(self.m as usize);

          for i in m.entries() {
            plaintext.push(i.modulo(self.m as isize) as ModValue)
          }

        }
        _       => break
      }
    }
    plaintext
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

#[test]
fn test_hill_cipher() {

  let m_v: Vec<isize> = vec![
    10,05,12,
    03,14,21,
    08,09,11];,

  let hill = HillCipher::new(&(matrix::Matrix::new(3, 3, m_v), 26));

  let p = vec![9,20,11,24];
  let c = vec![3,4,11,22];

  let encrypted = hill.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = hill.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
