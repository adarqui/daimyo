#[cfg(test)]

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate identity;
use identity::*;

#[test]
fn test_identity_cipher() {
  let identity = IdentityCipher::new(&());
  assert_eq!(identity.encrypt(vec![01,02,03]), vec![01,02,03]);
  assert_eq!(identity.decrypt(vec![01,02,03]), vec![01,02,03]);
  assert_eq!(identity.decrypt(identity.encrypt(vec![01,02,03])), vec![01,02,03]);
}
