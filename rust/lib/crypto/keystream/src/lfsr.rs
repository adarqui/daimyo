// https://en.wikipedia.org/wiki/Linear-feedback_shift_register
// http://www-math.ucdenver.edu/~wcherowi/courses/m5410/m5410fsr.html



/// LFSR - Linear Feedback Shift Register
///
/// Shift register with m stages.
/// Vector (k_1,...,k_m) initializes the shift register.
/// At each time unit, the following operations would be performed concurrently:
/// 1. k_1 would be tapped as the next keystream bit
/// 2. k_2,...,k_m would each be shifted one stage to the left
/// 3. the "new" value of k_m would be computed to be:
///  SUM FROM j=0 TO m-1 OF c_j*k_(j+1)
///
/// At any given point in time, the shift egister contains m consecutive keystream elemts, say z_1,...,z_(i+m-1).
/// After each time unit, the shift register contains z_(i+1),...,z_(i+m).
///
pub struct LFSRKeyStream {
  constants: Vec<usize>,
  register: Vec<usize>,
  m: usize
}



impl LFSRKeyStream {
  pub fn new(constants: &Vec<usize>, iv: &Vec<usize>) -> LFSRKeyStream {
    LFSRKeyStream {
      constants: constants.to_owned().into_iter().map(|x| x % 2).collect(),
      register: iv.to_owned().into_iter().map(|x| x % 2).collect(),
      m: iv.len()
    }
  }
}



impl Iterator for LFSRKeyStream {
  type Item = usize;
  fn next(&mut self) -> Option<usize> {

    let k_1 = self.register.get(0).unwrap().to_owned();
    for i in 1..self.m {
      self.register[i-1] = self.register[i];
    }

    let mut k_m = 0;
    for j in 0..self.m-1 {
      k_m += self.constants.get(j).unwrap() * self.register.get(j).unwrap(); 
    }

    self.register[self.m-1] = k_m % 2;

    Some(k_1)
  }
}
