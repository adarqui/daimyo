extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate util;
use util::vec;
use util::char;

extern crate shift_alpha;
use shift_alpha::*;

#[test]
fn test_shift_cipher_alpha_1() {
  let shift_alpha = ShiftCipherAlpha::new(&char::base_to_char(11));

  let p: Vec<char> = vec::string_to_vec_of_char("wewillmeetatmidnight");
  let c: Vec<char> = vec::string_to_vec_of_char("hphtwwxppelextoytrse");

  let encrypted = shift_alpha.encrypt(p.to_owned());
  let decrypted = shift_alpha.decrypt(encrypted.to_owned());

  assert_eq!(encrypted, c);
  assert_eq!(decrypted, p);
}

#[test]
fn test_shift_cipher_alpha_exhaustive() {

  // a necessary condition for a crypto system to be secure is that an exhaustive key search should be infeasable

  let p: Vec<char> = vec::string_to_vec_of_char("astitchintimesavesnine");
  let c: Vec<char> = vec::string_to_vec_of_char("jbcrclqrwcrvnbjenbwrwn");

  let mut matched_i = 0;

  for i in 1..25 {
    let shift_alpha = ShiftCipherAlpha::new(&char::base_to_char(i));
    let decrypted = shift_alpha.decrypt(c.to_owned());
    if decrypted == p {
      matched_i = i
    }
  }

  assert_eq!(matched_i, 9);
}
