extern crate modulo;
use modulo::mod_shared::{ModValue};

extern crate crypto_system;
use crypto_system::CryptoSystem;
use crypto_system::KeySpace;

use std::collections::BTreeMap;



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
  type K = Vec<(ModValue,ModValue)>;

  fn new(key_map: &Vec<(ModValue, ModValue)>) -> Self {
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
    let v: Vec<ModValue> =
      plaintext
      .into_iter()
      .map(|x| self.e.get(&x).unwrap().to_owned())
      .collect();
    v
  }

  fn decrypt(&self, ciphertext: Vec<ModValue>) -> Vec<ModValue> {
    let v: Vec<ModValue> =
      ciphertext
      .into_iter()
      .map(|x| self.d.get(&x).unwrap().to_owned())
      .collect();
    v
  }
}




impl KeySpace for SubstitutionCipher {
  type K = Vec<(ModValue,ModValue)>;

  fn new(s: SubstitutionCipher) -> Self {
    s
  }
  fn key_space(&self) -> u64 {
    // factorial(length of map)
    // ie: 26 keys in map, 26!
    0
  }
  fn min_bound(&self) -> Option<&Self::K> {
    None
  }
  fn max_bound(&self) -> Option<&Self::K> {
    None
  }
}
