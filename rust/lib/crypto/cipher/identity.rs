#[allow(unused_imports)]
use crypto::crypto_system::CryptoSystem;



/*
pub struct IdentityCipher<T> {
  v: T
}



impl<T> CryptoSystem for IdentityCipher<T> {
  type P = T;
  type C = T;
  type K = T;
  fn new(k: &T) -> Self {
    IdentityCipher {
      v: k
    }
  }
  fn encipher(&self, p: &T) -> &T {
    p.clone()
  }
  fn decipher(&self, c: &T) -> &T {
    c
  }
}
*/



pub struct IdentityCipher {
}



impl CryptoSystem for IdentityCipher {
  type P = i64;
  type C = i64;
  type K = ();
  fn new(_: &()) -> Self {
    IdentityCipher {
    }
  }
  fn encrypt(&self, p: Vec<i64>) -> Vec<i64> {
    p.clone()
  }
  fn decrypt(&self, c: Vec<i64>) -> Vec<i64> {
    c.clone()
  }
}



#[test]
fn test_identity_cipher() {
  let identity = IdentityCipher::new(&());
  assert_eq!(identity.encrypt(vec![01,02,03]), vec![01,02,03]);
  assert_eq!(identity.decrypt(vec![01,02,03]), vec![01,02,03]);
  assert_eq!(identity.decrypt(identity.encrypt(vec![01,02,03])), vec![01,02,03]);
}
