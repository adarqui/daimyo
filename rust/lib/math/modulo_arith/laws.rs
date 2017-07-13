#[allow(unused_imports)]
use math::modulo;

#[test]
fn test_mod_arith_closed_addition() {
  /*
   * addition is closed, i,e., for any a, b in Zm, a + b in Zm
   */
}


#[test]
fn test_mod_arith_commutative_addition() {
  /*
   * addition is commutative, i.e., for any a, b in Zm, a + b = b + a
   */
   let a = modulo::ModArith::new(5, 10);
   let b = modulo::ModArith::new(77, 10);

   assert_eq!(a+b, b+a);
}


#[test]
fn test_mod_arith_associative_addition() {
  /*
   * addition is associative, i.e., for any a, b, c in Zm, (a + b) + c = a + (b + c)
   */
   let a = modulo::ModArith::new(5, 10);
   let b = modulo::ModArith::new(77, 10);
   let c = modulo::ModArith::new(132, 10);

   assert_eq!((a+b)+c, a+(b+c));
}

#[test]
fn test_mod_arith_0_additive_identity() {
  /*
   * 0 is an additive identity, i.e., for any a in Zm, a + 0 = 0 + a = a
   */
   let zero = modulo::ModArith::new(0, 10);
   let a = modulo::ModArith::new(5, 10);

   assert_eq!(a+zero, zero+a);
   assert_eq!(a+zero, a);
   assert_eq!(a, a+zero);
}
