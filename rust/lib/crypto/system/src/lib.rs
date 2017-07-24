/// A crypto_system is a five-tuple (P,C,_KS_,E,D), where the following conditions are satisfied:
///
/// 1. P is a finite set of possible plaintexts;
/// 2. C is a finite set of possible ciphertexts;
/// 3. _KS_, the keyspace, is a finite set of possible keys;
/// 4. For each K IN _KS_, there is an encryption rule e_K in E and a corresponding decryption rule d_K in D. Each e_K : P -> C and d_K : C -> P are functions such that d_K(e_K(x)) = x for every plaintext element x IN P
///
/// ok!
pub trait CryptoSystem {
  type P; // plaintext
  type C; // ciphertext
  type K; // key
  fn new(&Self::K) -> Self;
  fn encrypt(&self, Vec<Self::P>) -> Vec<Self::C>;
  fn decrypt(&self, Vec<Self::C>) -> Vec<Self::P>;
}



/// Synchronous Stream Cipher
///
/// A synchronous stream cipher is a tuple (P,C,_KS_,L,E,D), together with a function
/// g, such that the following conditions are satisfied:
///
/// 1. P is a finite set of possible plaintexts
/// 2. C is a finite set of possible ciphertexts
/// 3. _KS_, the keyspace, is a finite set of possible keys
/// 4. L is a finite set called the keystream alphabet
/// 5. g is the keystream generator. g takes a key K as input, and generates an infinite string z_1z_2... called the keystream, where z_i IN L for all i >= 1.
/// 6. For each z IN L, there is an encryption rule, e_z IN E and a corresponding decryption rule d_z IN D. e_z : P -> C, and d_z : C -> P are functions such that d_z(e_z(x)) = x for every plaintext element x IN P.
///
/// We can think of a block cipher as a special case of a stream cipher where the keystream is constant:
/// z_i = K for all i >= 1
///
/// A stream cipher is a periodic stream cipher with period d if z_i+d = z_i for all integers i >= 1.
///
/// Stream ciphers are often described in terms of binary alphabets, i.e., P = C = L = Z_2. In this situation, encryption and decryption operations are just addition modulo 2:
///
///   e_z(x) = (x + z) mod 2
///   d_z(y) = (y + z) mod 2
///
///   If we tink of 0 as false and 1 as true, then addition modulo 2 corresponds to the exclusive or (XOR) operation.
///
/// TODO FIXME:
/// - would be nice to extend CryptoSystem
/// - pub trait SynchronousStreamCipher : CryptoSystem {
pub trait SynchronousStreamCipher {
  type P; // plaintext
  type C; // ciphertext
  type K; // key
  type L; // keystream alphabet
  fn new(&Self::K) -> Self;
  fn encrypt(&mut self, Vec<Self::P>) -> Vec<Self::C>;
  fn decrypt(&mut self, Vec<Self::C>) -> Vec<Self::P>;
  fn g(&mut self) -> Self::L;
}



/// A non-synchronous strea cipher is a stream cipher in which,
/// each keystream element z_i depends on previous plaintext or ciphertext
/// elements (x_1,...,x_i-1 and/or y_1,...y_i-1) as well as the key K.
///
pub trait NonSynchronousStreamCipher {
  type P; // plaintext
  type C; // ciphertext
  type K; // key
  type L; // keystream alphabet
  fn new(&Self::K) -> Self;
  fn encrypt(&mut self, Vec<Self::P>) -> Vec<Self::C>;
  fn decrypt(&mut self, Vec<Self::C>) -> Vec<Self::P>;
  fn g(&mut self) -> Self::L;
}



pub trait KeySpace {
  type K;
  // TODO FIXME: Turn into iterator
  fn new(Self) -> Self;
  fn key_space(&self) -> u64;
  fn min_bound(&self) -> Option<&Self::K>;
  fn max_bound(&self) -> Option<&Self::K>;
}


pub fn crypto_system() {
}
