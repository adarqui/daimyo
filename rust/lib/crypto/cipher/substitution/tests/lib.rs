#[cfg(test)]

extern crate modulo;
use modulo::mod_shared::ModValue;

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate substitution;
use substitution::*;

#[test]
fn test_substitution_cipher() {
  let key_map = vec![(0,9),(1,8),(2,7),(3,6),(4,5)];
  let sub = <SubstitutionCipher as CryptoSystem>::new(&key_map);

  let p: Vec<ModValue> = key_map.to_owned().into_iter().map(|x|x.0).collect();
  let c: Vec<ModValue> = key_map.to_owned().into_iter().map(|x|x.1).collect();

  let encrypted = sub.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = sub.decrypt(encrypted);
  assert_eq!(decrypted, p.to_owned());
}

#[test]
fn test_substitution_cipher_2() {
  // TODO
  // mgzvyzlghcmhjmyxssfmnhahycdlmha
}
