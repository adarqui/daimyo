#[allow(unused_imports)]
use std::io::Write;
use math::mod_shared::{ModValue};
#[allow(unused_imports)]
use util::vec;
use crypto::crypto_system::CryptoSystem;



#[allow(dead_code)]
pub struct PermutationCipher {
  d: Vec<(u64,u64)>,
  e: Vec<(u64,u64)>,
  start_at: u64,
  block_size: usize,
}



/// Permutation Cipher
///
/// Let n be a positive integer. Let P = C = (Z_m)^n and let _KS_ consist of all permutations of {1,...,n}.
///  For a key (i.e., a permutations) pi, we defined:
///   e_pi(x_1,...,x_n) = (x_pi(1),...,x_pi(n))
///  and
///   d_pi(y_1,...,y_n) = (y_pi(1),...,y_inv_pi(n)),
///  where inv_pi is the inverse permutation of pi
///
///
///  key:
///   e: 351642
///   d: 361524
///
impl CryptoSystem for PermutationCipher {
  type P = ModValue;
  type C = ModValue;
  type K = (Vec<u64>, u64);

  fn new(k_s: &(Vec<u64>,u64)) -> Self {
    let key = k_s.0.to_owned();
    let start_at = k_s.1.to_owned();

    let range: Vec<u64> = (0 .. key.len()).map(|x| x as u64).collect();

    let mut e_key: Vec<(u64,u64)> = key.iter().zip(range.into_iter()).map(|(k,r)| (k - start_at, r)).collect();
    e_key.sort_by(|&(k1,_),&(k2,_)| k1.cmp(&k2));
    
    // inverse key
    let mut d_key: Vec<(u64,u64)> = e_key.to_owned();
    d_key.sort_by(|&(_,r1),&(_,r2)| r1.cmp(&r2));

    PermutationCipher {
      d: d_key,
      e: e_key,
      start_at: start_at,
      block_size: key.len(),
    }
  }

  fn encrypt(&self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let pcopy = plaintext.to_owned();
    let mut ciphertext: Vec<ModValue> = vec![0; plaintext.len()];
    let mut iter = pcopy.chunks(self.block_size);
    let mut block_index = 0;
    loop {
      match iter.next() {
        Some(block) => {
          for (i, elt) in block.iter().enumerate() {
            let (_, new_index) = self.e.get(i).unwrap().to_owned();
            ciphertext[(block_index * self.block_size) + (new_index as usize)] = elt.to_owned();
          }
          block_index += 1
        },
        _ => break
      }
    }
    ciphertext
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let ccopy = ciphertext.to_owned();
    let mut plaintext: Vec<ModValue> = vec![0; ciphertext.len()];
    let mut iter = ccopy.chunks(self.block_size);
    let mut block_index = 0;
    loop {
      match iter.next() {
        Some(block) => {
          for (i, elt) in block.iter().enumerate() {
            let (new_index, _) = self.d.get(i).unwrap().to_owned();
            plaintext[(block_index * self.block_size) + (new_index as usize)] = elt.to_owned();
          }
          block_index += 1
        },
        _ => break
      }
    }
    plaintext
  }
}



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
