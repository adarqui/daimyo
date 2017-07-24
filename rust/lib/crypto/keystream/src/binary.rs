#![allow(dead_code)]



/// A synchronous keystream over binary alphabets
///
/// Suppose we start with a binary m-tuple (k_1,...,k_m) and
/// let z_i = k_i, 1 <= i <= m. We generate the keystream using a linear
/// recurrence of degree m:
///
///  z_(i+m) = SUM FROM (j = 0) TO (m - 1) OF (c_j * z_(i+j)) mod 2
///  or all i >= 1, where c_0,...,c_(m-1) IN Z_2 are specified constants
/// 
/// This recurrence is said to have degree m since each term depends on the previous m terms.
/// It is linear because z_i+m is a linear function of previous terms.
/// Note that we can take c_0 = 1 without loss of generality, for otherwise
/// the recurrence will be of degree (at most) m - 1
///
/// Here, the key K consists of the 2m values k_1,...,k_m, c_0,...,c_(m-1).
/// If (k_1,...,k_m) = (0,...,0), then the keystream consists entirely of 0's.
/// This should be avoided: ciphertext = plaintext
///
/// If the constants c_0,...,c_(m-1) are chosen in a suitable way, then any other initializtion vecotor (k_1,...,k_m) will give rise to the periodic keystream having a period of 2^m - 1.
///
pub struct BinaryKeyStream {
  constants: Vec<usize>,
  iv: Vec<usize>,
  m: usize,
  z_prev: usize,
}



impl BinaryKeyStream {
  pub fn new(constants: &Vec<usize>, iv: &Vec<usize>) -> BinaryKeyStream {
    BinaryKeyStream {
      constants: constants.to_owned(),
      iv: iv.to_owned(),
      m: constants.len(),
      z_prev: 1,
    }
  }
}



///
/// c = (1,0,0,0)
///
/// z_(i+m) = SUM FROM j=0 TO m-1 OF c_j*z_(i+j) mod 2
/// m = 4:
/// i = 1:    SUM FROM j=0 TO 4-1 OF c_j*z_(1+j) mod 2
///     j = 0 SUM FROM j=0 TO 4-1 OF 1*z_1 mod 2
///                        z_(1+1) = SUM FROM j=0 to 2-1 OF c_j*z_(i+j) mod 2
///                        z_2     = SUM FROM j=0 to 1 OF 1*z_1 mod 2
///                    j=1 TO 4-1 OF 0*z_2 mod 2
///                    j=2 TO 4-1 OF 0*z_3 mod 2
///                    j=3 TO 4-1 OF 0*z_4 mod 2
///
/// z_i     =
/// z_(i+1) =
/// z_(i+2) =
/// z_(i+3) =
/// z_(i+4) = (z_i + z_(i+1)) mod 2
///
/// c = (1,0,0,0)
/// z_(i+m) = j = 0 = c_0 * z_(i+0) = 1 * z_i
/// z_(i+m) = j = 1 = c_1 * z_(i+1) = 0 * z_(i+1) = 0
/// z_(i+m) = j = 2 = c_2 * z_(i+2) = 0 * z_(i+2) = 0
/// z_(i+m) = j = 3 = c_3 * z_(i+3) = 0 * z_(i+3) = 0
///
/// z_1 + z_2 + z_3 + z_4 + z_5 + z_6 + z_7 + z_8 + z_9 + ...
/// 1     0     0     0     1?    0?    0?   1?     1?
///
impl Iterator for BinaryKeyStream {
  type Item = usize;
  fn next(&mut self) -> Option<usize> {
    None
  }
}
