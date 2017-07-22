// ripped
#![allow(dead_code)]



use std::ops::Add;



pub struct SimpleStepRange(pub isize, pub isize, pub isize);  // start, end, and step

impl Iterator for SimpleStepRange {
  type Item = isize;

  #[inline]
  fn next(&mut self) -> Option<isize> {
    if self.0 < self.1 {
      let v = self.0;
      self.0 = v + self.2;
      Some(v)
    } else {
      None
    }
  }
}



pub struct StepRange<T>(T, T, T)
  where for<'a> &'a T: Add<&'a T, Output = T>,
        T: PartialOrd,
        T: Clone;

impl<T> Iterator for StepRange<T>
  where for<'a> &'a T: Add<&'a T, Output = T>,
        T: PartialOrd,
        T: Clone {
  type Item = T;

  #[inline]
  fn next(&mut self) -> Option<T> {
    if self.0 < self.1 {
      let v = self.0.clone();
      self.0 = &v + &self.2;
      Some(v)
    } else {
      None
    }
  }
}
