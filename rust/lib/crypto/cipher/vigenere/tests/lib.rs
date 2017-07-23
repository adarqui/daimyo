#[cfg(test)]

extern crate util;
use util::vec;

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate vigenere;
use vigenere::*;

#[test]
fn test_vigenere_cipher() {
  let key = vec![02,08,15,07,04,17]; // CIPHER
  let vigenere = VigenereCipher::new(&(key, 26));

  let p = vec::string_to_vec_of_i64_m26("thiscryptosystemisnotsecure");
  let c = vec::string_to_vec_of_i64_m26("vpxzgiaxivwpubttmjpwizitwzt");

  let encrypted = vigenere.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = vigenere.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
