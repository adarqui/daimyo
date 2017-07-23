extern crate modulo;
use modulo::mod_shared::{ModValue, ModModulus};
use modulo::mod_num::{ModuloSignedExt};

extern crate matrix;

extern crate crypto_system;
use crypto_system::CryptoSystem;



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
