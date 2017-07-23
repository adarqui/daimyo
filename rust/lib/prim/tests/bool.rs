#[cfg(test)]

extern crate prim;

use prim::bool::*;

#[test]
fn test_prim_true() {
  assert_eq!(prim_true(), true);
}

#[test]
fn test_prim_false() {
  assert_eq!(prim_false(), false);
}
