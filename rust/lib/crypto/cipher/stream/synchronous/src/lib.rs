extern crate modulo;
use modulo::mod_num::ModNum;
use modulo::mod_shared::{ModValue,ModModulus};

extern crate crypto_system;
use crypto_system::CryptoSystem;



pub struct SynchronousStreamCipher {
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
impl CryptoSystem for SynchronousStreamCipher {
}
