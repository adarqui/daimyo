#[cfg(test)]

extern crate modulo;
use modulo::mod_num::*;

extern crate crypto_system;
use crypto_system::CryptoSystem;

extern crate shift;
use shift::*;

#[test]
#[should_panic]
fn test_shift_cipher_should_panic() {
  let _ = ShiftCipher::new(&ModNum::new(1, 0));
  let _ = ShiftCipher::new(&ModNum::new(0, 1));
  let _ = ShiftCipher::new(&ModNum::new(10, 9));
}

#[test]
fn test_shift_cipher_1() {

  let shift = ShiftCipher::new(&ModNum::new(1, 26));
  let p = vec![00,05,10,15,20,25];
  let c = vec![01,06,11,16,21,00];

  let encrypted = shift.encrypt(p.to_owned());
  assert_eq!(encrypted, c);

  let decrypted = shift.decrypt(c.to_owned());
  assert_eq!(decrypted, p);

  assert_eq!(shift.decrypt(shift.encrypt(p.clone())), p);
}

#[test]
fn test_shift_cipher_2() {

  let shift = ShiftCipher::new(&ModNum::new(11, 26));

  // WEWILLMEETATMIDNIGHT
  let mut p = vec![22,04,22,08,11,11,12,04,04,19];
  let mut p2 = vec![00,19,12,08,03,13,08,06,07,19];
  p.append(&mut p2);

  let mut c = vec![07,15,07,19,22,22,23,15,15,04];
  let mut c2 = vec![11,04,23,19,14,24,19,17,18,04];
  c.append(&mut c2);

  let encrypted = shift.encrypt(p.to_owned());
  let decrypted = shift.decrypt(c.to_owned());

  assert_eq!(encrypted, c.to_owned());
  assert_eq!(decrypted, p.to_owned());

  assert_eq!(shift.decrypt(shift.encrypt(p.to_owned())), p);
}
