// https://en.wikipedia.org/wiki/Matrix_(mathematics)



use std::ops::Mul;
#[allow(unused_imports)]
use std::io::Write;
use util::range;



#[allow(dead_code)]
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Matrix {
  rows: usize,
  cols: usize,
  size: usize,
  entries: Vec<isize>
}

#[allow(dead_code)]
impl Matrix {

  fn new(rows: usize, cols: usize, entries: Vec<isize>) -> Self {
    assert_eq!(rows * cols, entries.len());
    Matrix {
      rows: rows,
      cols: cols,
      size: rows * cols,
      entries: entries
    }
  }

  /// nth:
  ///
  /// 01 00 00 01
  ///
  /// row = 2, col = 2
  /// nth = row * col
  ///
  fn nth(&self, row: usize, col: usize) -> &isize {
    assert!(row > 0 && col > 0, "row > 0 && col > 0");
    assert_eq!(row, col);
    self.entries.get(row * col).unwrap()
  }

  /// row:
  ///
  /// 01 02 03 04
  ///
  /// row 1: 01 02
  /// row 2: 03 04
  ///
  fn row(&self, row: usize) -> Vec<isize> {
    assert!(row > 0, "row > 0");
    let start = (row-1) * self.cols;
    let end = start + self.cols;
    let mut v = Vec::new();
    v.extend_from_slice(&self.entries[start .. end]);
    v
  }

  /// col:
  ///
  /// 01 02 03 04
  ///
  /// row 1: 01 03
  /// row 2: 02 04
  ///
  fn col(&self, col: usize) -> Vec<isize> {
    assert!(col > 0, "col > 0");
    let mut v = Vec::new();
    for i in range::SimpleStepRange((col - 1) as isize, self.size as isize, self.cols as isize) {
      let e = self.entries.get(i as usize).unwrap();
      v.push(e.to_owned());
    }
    v
  }

  /// identity:
  ///
  /// 01 00
  /// 00 01
  ///
  /// 01 00 00 01
  ///
  /// a_ij = 1
  ///
  fn identity(&self) -> Self {
    let mut entries: Vec<isize> = Vec::with_capacity(self.size);
    for row in 1 .. (self.rows+1) {
      for col in 1 .. (self.cols+1) {
        if row == col {
          entries.push(1);
        } else {
          entries.push(0);
        }
      }
    }
    Matrix {
      rows: self.rows,
      cols: self.cols,
      size: self.size,
      entries: entries
    }
  }
}

/// Matrix Multiplication
///
/// (C_11 C_12) = (A_11 A_12) * (B_11 B_12) = (A_11*B_11+A12*B_21 A_11*B_12+A_12+B_22)
/// (C_21 C_22) = (A_21 A_22)   (B_21 B_22)   (A_21*B_11+A22*B_21 A_21*B_12+A_22+B_22)
///
/// 
/// A = 2|3|4   B = 0|1000
///     1|0|0       1|100
///                 0|10
///
/// C = 2*0+3*1+4*0 | 2*1000+3*100+4*10
///     1*0+0*1+0*0 | 1*1000+0*100+0*10
///
impl Mul for Matrix {
  type Output = Self;
  fn mul(self, rhs:Self) -> Self {
    // if C = AB for an n × m matrix A and an m × p matrix B, then C is an n × p matrix with entries
    // C_ij = SUM from k=1 to m {a_ik * b_kj}
    let (ar, ac, br, bc) = (self.rows, self.cols, rhs.rows, rhs.cols);
    assert_eq!(ac, br);
    let (cr, cc, c_size) = (ar, bc, ar * bc);
    let mut entries: Vec<isize> = Vec::with_capacity(c_size);
    for ci in 1 .. (cr+1) {
      for cj in 1 .. (cc+1) {
        let row = self.row(ci);
        let col = rhs.col(cj);
        let value = row.iter().zip(col.iter()).map(|(x,y)| x*y).sum();
        entries.push(value);
      }
    }
    Matrix::new(cr, cc, entries)
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

#[test]
fn test_identity_matrix() {
  let mat = Matrix::new(2, 2, vec![
    01, 02,
    03, 04]);
  assert_eq!(mat.rows, 2);
  assert_eq!(mat.cols, 2);
  assert_eq!(mat.size, 4);
  assert_eq!(mat.identity().entries, vec![
    01, 00,
    00, 01]);
}

#[test]
fn test_row_matrix() {
  let mat = Matrix::new(2, 2, vec![
    01, 02,
    03, 04]);
  let row = mat.row(1);
  assert_eq!(row, vec![01, 02]);

  let row = mat.row(2);
  assert_eq!(row, vec![03, 04]);
}

#[test]
fn test_col_matrix() {
  let mat = Matrix::new(2, 2, vec![
    01, 02,
    03, 04]);
  let col = mat.col(1);
  assert_eq!(col, vec![01, 03]);

  let col = mat.col(2);
  assert_eq!(col, vec![02, 04]);
}

#[test]
fn test_matrix_multiply() {
  let ma = Matrix::new(2, 3, vec![
    2, 3, 4,
    1, 0, 0]);
  let mb = Matrix::new(3, 2, vec![
    0, 1000,
    1, 100,
    0, 10]);
  let mc = ma * mb;
  assert_eq!(mc.entries, vec![3, 2340, 0, 1000]);
}
