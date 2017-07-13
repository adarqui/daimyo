#[allow(unused_imports)]
use math::modulo;

#[test]
fn test_p1_mod_arith_closed_addition() {
  /*
   * addition is closed, i,e., for any a, b in Zm, a + b in Zm
   */
}


#[test]
fn test_p2_mod_arith_commutative_addition() {
  /*
   * addition is commutative, i.e., for any a, b in Zm, a + b = b + a
   */
   let a = modulo::ModArith::new(5, 10);
   let b = modulo::ModArith::new(77, 10);

   assert_eq!(a+b, b+a);
}


#[test]
fn test_p3_mod_arith_associative_addition() {
  /*
   * addition is associative, i.e., for any a, b, c in Zm, (a + b) + c = a + (b + c)
   */
   let a = modulo::ModArith::new(5, 10);
   let b = modulo::ModArith::new(77, 10);
   let c = modulo::ModArith::new(132, 10);

   assert_eq!((a+b)+c, a+(b+c));
}

#[test]
fn test_p4_mod_arith_0_additive_identity() {
  /*
   * 0 is an additive identity, i.e., for any a in Zm, a + 0 = 0 + a = a
   */
   let zero = modulo::ModArith::new(0, 10);
   let a = modulo::ModArith::new(5, 10);

   assert_eq!(a+zero, zero+a);
   assert_eq!(a+zero, a);
   assert_eq!(a, a+zero);
}

#[test]
fn test_p5_mod_arith_additive_inverse() {
  /*
   * the additive inverse of any a in Zm is m-a, i.e., a+(m-a) = (m-a)+a = 0, for any a in Zm
   */
   let a = 3;
   let m = 11;
   assert_eq!(a+(m-a), (m-a)+a);
   assert_eq!((a+(m-a)) - ((m-a)+a), 0);
}

#[test]
fn test_p6_mod_arith_closed_multiplication() {
  /*
   * multiplication is closed, i.e., for any a, b in Zm, ab in Zm
   */
}

#[test]
fn test_p7_mod_arith_commutative_multiplication() {
  /*
   * multiplication is commutative, i.e., for any a, b in Zm, ab = ba
   */
   let a = modulo::ModArith::new(5, 10);
   let b = modulo::ModArith::new(77, 10);

   assert_eq!(a*b, b*a);
}

#[test]
fn test_p8_mod_arith_associative_multiplication() {
  /*
   * multiplication is associative, i.e., for any a, b, c in Zm, (ab)c = a(bc)
   */
   let a = modulo::ModArith::new(5, 10);
   let b = modulo::ModArith::new(77, 10);
   let c = modulo::ModArith::new(132, 10);

   assert_eq!((a*b)*c, a*(b*c));
}

#[test]
fn test_p9_mod_arith_1_multiplicative_identity() {
  /*
   * 1 is a multiplicative identity, i.e., for any a in Zm, a x 1 = 1 x a = a
   */
   let one = modulo::ModArith::new(1, 10);
   let a = modulo::ModArith::new(5, 10);

   assert_eq!(a*one, one*a);
   assert_eq!(a*one, a);
   assert_eq!(a, a*one);
}

#[test]
fn test_p10_mod_arith_distributive_multiplication() {
  /*
   * the distributative property is satisfied, i.e., for any a, b, c in Zm, (a + b)c = (ac) + (bc) and a(b + c) = (ab) + (ac)
   */
   let a = modulo::ModArith::new(5, 10);
   let b = modulo::ModArith::new(77, 10);
   let c = modulo::ModArith::new(132, 10);

   assert_eq!((a+b)*c, (a*c)+(b*c));
   assert_eq!(a*(b+c), (a*b)+(a*c));
}

#[test]
fn test_mod_arith_subtraction() {
  /*
   * since additive inverses exist in Zm, we can also subtract elements in Zm.
   * we define a - b in Zm to be (a - b)mod m.
   */
   let a = modulo::ModArith::new(11, 31);
   let b = modulo::ModArith::new(18, 31);
   assert_eq!(a-b, modulo::ModArith::new(24, 31))
}

#[test]
fn test_mod_arith_group() {
  /*
   * properties p1, p3, p4, p5 say that Zm forms an algebraic structure called a:
   * group with respect to the addition property
   *
   * since p2 also holds, the group is said to be an albelian group
   */
}

#[test]
fn test_mod_arith_ring() {
  /*
   * properties p1 through p10 establish that Zm is a ring.
   */
}
