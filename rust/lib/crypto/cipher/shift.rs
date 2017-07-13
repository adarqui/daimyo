#[allow(unused_imports)]
use math::modulo;

pub struct ShiftCipher {
}

#[test]
fn test_cipher_shit() {
  assert_eq!(true, true);
/*
  TODO: something like this

  let shift = ShiftCipher::new(key, 26)
  let plain = "wewillmeetatmidnight"
  let plain_u8s = plain.to_u8s()
  let e = shift.encrypt(plain_u8s);
  let d = shift.decrypt(e);
  assert_eq!(d, plain_u8s);
  assert_eq!(d.from_u8s(), plain);
  assert_eq!(shift.decrypt(shift.encrypt(plain_u8s)), plain_u8s);
*/
}
