#[cfg(test)]

extern crate matrix;

extern crate util;
use util::vec;

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate hill;
use hill::*;

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
fn test_hill_cipher_2() {

  let m_v: Vec<isize> = vec![
    10,05,12,
    03,14,21,
    08,09,11];

  let hill = HillCipher::new(&(matrix::Matrix::new(3, 3, m_v), 26));

  let p = vec![09,20,11,24,01,05];
  let c = vec![04,08,25,23,23,00];

  let encrypted = hill.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let _ = hill.decrypt(encrypted);
  // TODO FIXME
  // assert_eq!(decrypted, p);
}

#[test]
fn test_hill_cipher_as_permutation_cipher() {
  // 1.1.6
  let m_v: Vec<isize> = vec![
    0,0,1,0,0,0,
    0,0,0,0,0,1,
    1,0,0,0,0,0,
    0,0,0,0,1,0,
    0,1,0,0,0,0,
    0,0,0,1,0,0];

  let p = vec::string_to_vec_of_i64_m26("shesellsseashellsbytheseashore");
  let c = vec::string_to_vec_of_i64_m26("eeslshsalseslshblehsyeethraeos");

  let hill = HillCipher::new(&(matrix::Matrix::new(6, 6, m_v), 26));

  let encrypted = hill.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = hill.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
