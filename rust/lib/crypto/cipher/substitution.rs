#[allow(unused_imports)]
use std::io::Write;
use std::collections::BTreeMap;
use math::mod_shared::*;
use crypto::crypto_system::CryptoSystem;
use crypto::crypto_system::KeySpace;



#[allow(dead_code)]
pub struct SubstitutionCipher {
  d: BTreeMap<ModValue, ModValue>,
  e: BTreeMap<ModValue, ModValue>
}



/// Substitution Cipher
///
/// Let P = C = Z_m. _KS_ consists of all possible permutations of the m symbols, 0,1,...,m. For each permutations q in _KS_, define:
///   e_q(x) = q(x)
///  and
///   d_q(y) = q^-1(y)
///  where q^-1 is the inverse permutation of q.
///
impl CryptoSystem for SubstitutionCipher {
  type P = ModValue;
  type C = ModValue;
  type K = Vec<(i64,i64)>;

  fn new(key_map: &Vec<(i64, i64)>) -> Self {
    let mut d_map = BTreeMap::new();
    let mut e_map = BTreeMap::new();
    for x in key_map.into_iter() {
      e_map.insert(x.0, x.1);
      d_map.insert(x.1, x.0);
    }
    SubstitutionCipher {
      d: d_map,
      e: e_map
    }
  }

  fn encrypt(&self, plaintext: Vec<ModValue>) -> Vec<ModValue> {
    let v: Vec<i64> =
      plaintext
      .into_iter()
      .map(|x| self.e.get(&x).unwrap().to_owned())
      .collect();
    v
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let v: Vec<i64> =
      ciphertext
      .into_iter()
      .map(|x| self.d.get(&x).unwrap().to_owned())
      .collect();
    v
  }
}




impl KeySpace for SubstitutionCipher {
  type K = Vec<(i64,i64)>;

  fn new(s: SubstitutionCipher) -> Self {
    s
  }
  fn key_space(&self) -> u64 {
    0
  }
  fn min_bound(&self) -> Option<&Self::K> {
    None
  }
  fn max_bound(&self) -> Option<&Self::K> {
    None
  }
}



#[test]
fn test_substitution_cipher() {
  let key_map = vec![(0,9),(1,8),(2,7),(3,6),(4,5)];
  let sub = <SubstitutionCipher as CryptoSystem>::new(&key_map);

  let p: Vec<i64> = key_map.to_owned().into_iter().map(|x|x.0).collect();
  let c: Vec<i64> = key_map.to_owned().into_iter().map(|x|x.1).collect();

  let encrypted = sub.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = sub.decrypt(encrypted);
  assert_eq!(decrypted, p.to_owned());
}
