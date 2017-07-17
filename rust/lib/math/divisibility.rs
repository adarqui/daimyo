use num::integer;
use num::Integer;



/// broken?
///
pub fn euclidean_algorithm(a: u64, b: u64) -> u64 {
  let tup = a.div_rem(&b);
  let q = tup.0;
  let r = tup.1;

  match r {
    0 => q,
    _ => euclidean_algorithm(b, r)
  }
}

#[test]
fn test_euclidean_algorithm() {
  assert_eq!(euclidean_algorithm(13, 5), 2);
}



/// ripped: https://rosettacode.org/wiki/Modular_inverse#Rust
///
fn mod_inv(a: isize, module: isize) -> isize {
  let mut mn = (module, a);
  let mut xy = (0, 1);
 
  while mn.1 != 0 {
    xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
    mn = (mn.1, mn.0 % mn.1);
  }
 
  while xy.0 < 0 {
    xy.0 += module;
  }
  xy.0
}

#[test]
fn test_mod_inv() {
  assert_eq!(mod_inv(1, 26), 1);
  assert_eq!(mod_inv(3, 26), 9);
  assert_eq!(mod_inv(5, 26), 21);
  assert_eq!(mod_inv(7, 26), 15);
  assert_eq!(mod_inv(11, 26), 19);
  assert_eq!(mod_inv(25, 26), 25);
}
