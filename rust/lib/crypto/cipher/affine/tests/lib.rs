#[cfg(test)]

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate affine;
use affine::*;

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
