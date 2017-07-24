#[cfg(test)]

// #[macro_use]
extern crate util;
use util::vec;

extern crate crypto_system;
use crypto_system::SynchronousStreamCipher;

extern crate vigenere_synchronous_stream;
use vigenere_synchronous_stream::*;

#[test]
fn test_vigenere_synchronous_stream_cipher() {
  let key = vec![02,08,15,07,04,17]; // CIPHER
  let mut vigenere_enc = VigenereSynchronousStreamCipher::new(&(key.to_owned(), 26));
  let mut vigenere_dec = VigenereSynchronousStreamCipher::new(&(key.to_owned(), 26));

  let p = vec::string_to_vec_of_i64_m26("thiscryptosystemisnotsecure");
  let c = vec::string_to_vec_of_i64_m26("vpxzgiaxivwpubttmjpwizitwzt");

  let encrypted = vigenere_enc.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = vigenere_dec.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
