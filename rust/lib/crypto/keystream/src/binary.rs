/// A synchronous keystream over binary alphabets
///
/// Suppose we start with a binary m-tuple (k_1,...,k_m) and
/// let z_i = k_i, 1 <= i <= m. We generate the keystream using a linear
/// recurrence of degree m:
///
///  z_i + m = SUM FROM (j = 0) TO (m - 1) OF (c_j * z_i + j) mod 2
///  or all i >= 1, where c_0,...,c_m-1 IN Z_2 are specified constants
/// 
/// This recurrence is said to have degree m since each term depends on the previous m terms.
/// It is linear because z_i+m is a linear function of previous terms.
/// Note that we can take c_0 = 1 without loss of generality, for otherwise
/// the recurrence will be of degree (at most) m - 1
///
/// Here, the key K consists of the 2m values k_1,...,k_m, c_0,...,c_m-1.
/// If (k_1,...,k_m) = (0,...,0), then the keystream consists entirely of 0's.
/// This should be avoided: ciphertext = plaintext
///
/// If the constants c_0,...,c_m-1 are chosen in a suitable way, then any other initializtion vecotor (k_1,...,k_m) will give rise to the periodic keystream having a period of 2^m - 1.
///
pub struct BinaryKeyStream {
  constants: Vec<usize>,
  m: usize,
  iter_index: usize,
  z_prev: usize,
}



impl BinaryKeyStream {
  pub fn new(constants: &Vec<usize>) -> BinaryKeyStream {
    BinaryKeyStream {
      constants: constants.to_owned(),
      m: constants.len(),
      iter_index: 1,
      z_prev: 0,
    }
  }
}



impl Iterator for BinaryKeyStream {
  type Item = usize;
  fn next(&mut self) -> Option<usize> {
    let j: usize = self.iter_index;
    let c: usize = self.constants.get(j % self.m).unwrap().to_owned();
    let z_cur = ((c * self.z_prev) + j) % 2;
    self.z_prev += z_cur;
    self.iter_index += 1;
    Some(z_cur)
  }
}
