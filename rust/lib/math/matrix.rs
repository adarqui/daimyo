// https://en.wikipedia.org/wiki/Matrix_(mathematics)



use std::ops::Mul;



#[allow(dead_code)]
pub struct Matrix<T> {
  rows: usize,
  cols: usize,
  size: usize,
  v: Vec<T>
}

#[allow(dead_code)]
impl<T> Matrix<T> {
  fn new(rows: usize, cols: usize, v: Vec<T>) -> Self {
    assert_eq!(rows * cols, v.len());
    Matrix {
      rows: rows,
      cols: cols,
      size: rows * cols,
      v: v
    }
  }
}

/// Matrix Multiplication
///
/// (C_11 C_12) = (A_11 A_12) * (B_11 B_12) = (A_11*B_11+A12*B_21 A_11*B_12+A_12+B_22)
/// (C_21 C_22) = (A_21 A_22)   (B_21 B_22)   (A_21*B_11+A22*B_21 A_21*B_12+A_22+B_22)
///
impl<T> Mul for Matrix<T> {
  type Output = Self;
  fn mul(self, rhs:Self) -> Self {
    // if C = AB for an n × m matrix A and an m × p matrix B, then C is an n × p matrix with entries
    // C_ij = SUM from k=1 to m {a_ik * b_kj}
    let (n, m, m_, p) = (self.rows, self.cols, rhs.rows, rhs.cols);
    assert_eq!(m, m_);
    let mut v: Vec<T> = Vec::new();
    for i in 1 .. m {
      let a_ik = self.v.get(i - 1).unwrap();
      // v.push(a_ik.to_owned());
    }
    self
  }
}

#[test]
fn test_matrix() {
  let mat = Matrix::new(2, 2, vec![
    01, 02,
    03, 04]);
  assert_eq!(mat.rows, 2);
  assert_eq!(mat.cols, 2);
  assert_eq!(mat.size, 4);
}
