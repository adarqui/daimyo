#[cfg(test)]

extern crate util;
use util::vec;

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate permutation;
use permutation::*;

#[test]
fn test_permutation_cipher() {
  let key = vec![3,5,1,6,4,2];

  let permut = PermutationCipher::new(&(key, 1));

  let p = vec::string_to_vec_of_i64_m26("shesellsseashellsbytheseashore");
  let c = vec::string_to_vec_of_i64_m26("eeslshsalseslshblehsyeethraeos");

  let encrypted = permut.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = permut.decrypt(encrypted);
  assert_eq!(decrypted, p);
}
