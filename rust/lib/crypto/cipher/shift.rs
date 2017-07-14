#[allow(unused_imports)]
use math::modulo;
use std::io::Write;



#[allow(dead_code)]
pub struct ShiftCipher {
  key: u8,
  m: u8
}




impl ShiftCipher {
  pub fn new(key: u8, m: u8) -> Self {
    ShiftCipher {
      key: key,
      m: m
    }
  }
  pub fn encrypt(self, plaintext: &[u8]) -> &[u8] {
    /*
    let mut ciphertext: &[u8] = plaintext;
    for x in plaintext {
      let c = plaintext[x];
    }
    */
    let ciphertext: &[u8] = plaintext;
    println_stderr!("{}", ciphertext[0]);
    println_stderr!("{:?}", plaintext);
    println_stderr!("{}", plaintext.len());
    let v: Vec<u8> = plaintext.iter().map(|x| x ^ self.key).collect();
    plaintext
  }
  pub fn decrypt(self, ciphertext: &[u8]) -> &[u8] {
    ciphertext
  }
}



#[test]
fn test_shift_cipher() {
  println_stderr!("HELLO");
  assert_eq!(true, true);
  let shift = ShiftCipher::new(45, 13);
  shift.encrypt("hello".as_bytes());
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
