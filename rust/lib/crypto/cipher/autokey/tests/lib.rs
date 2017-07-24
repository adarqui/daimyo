#[cfg(test)]

extern crate crypto_system;
use crypto_system::NonSynchronousStreamCipher;

extern crate autokey;
use autokey::*;

#[test]
fn test_autokey_cipher() {

  let mut autokey_enc = AutoKeyNonSynchronousStreamCipher::new(&8);
  let mut autokey_dec = AutoKeyNonSynchronousStreamCipher::new(&8);

  let p = vec![17,04,13,03,04,25,21,14,20,18];
  let c = vec![25,21,17,16,07,03,20,09,08,12];

  let encrypted = autokey_enc.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = autokey_dec.decrypt(encrypted);
  assert_eq!(decrypted, p);

  let c_2 = vec![09,21,17,16,07,03,20,09,08,12];
  let encrypted_2 = autokey_enc.encrypt(p.to_owned());
  assert_eq!(encrypted_2, c_2);

  let decrypted_2 = autokey_dec.decrypt(encrypted_2);
  assert_eq!(decrypted_2, p);
}
