pub fn prim_true() -> bool {
  true
}



pub fn prim_false() -> bool {
  false
}



#[test]
fn test_prim_true() {
  assert_eq!(prim_true(), true);
}

#[test]
fn test_prim_false() {
  assert_eq!(prim_false(), false);
}
