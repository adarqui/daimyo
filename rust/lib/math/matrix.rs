// https://en.wikipedia.org/wiki/Matrix_(mathematics)



#[allow(unused_imports)]
use std::io::Write;
use std::ops::Add;
use std::ops::Mul;
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
  /// 01 02 03 04
  ///
  /// (1,1) = 01 = 0
  /// (1,2) = 02 = 1
  /// (2,1) = 03 = 2
  /// (2,2) = 04 = 3
  ///
  /// nth = (((row-1) * cols) + col) - 1
  ///
  /// (1,1) (((1  -1) * 2) + 1) - 1
  ///       (((0) * 2) + 1) - 1
  ///       1 - 1
  ///       0
  /// (1,2) (((1  -1) * 2) + 2) - 1
  ///       (((0) * 2) + 2) - 1
  ///       2 - 1
  ///       1
  /// (2,1) (((2  -1) * 2) + 1) - 1
  ///       (((1) * 2) + 1) - 1
  ///       3 - 1
  ///       2
  /// (2,2) (((2  -1) * 2) + 2) - 1
  ///       (((1) * 2) + 2) - 1
  ///       4 - 1
  ///       3
  ///
  fn nth(&self, row: usize, col: usize) -> &isize {
    assert!(row > 0 && col > 0, "row > 0 && col > 0");
    let offset = (((row-1) * self.cols) + col) - 1;
    self.entries.get(offset).unwrap()
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



/*
impl<'a> Mul for &'a Matrix {
  type Output = Matrix;
  fn mul(self, rhs: &'a Matrix) -> Matrix {
    self * rhs
  }
}
*/



/// Matrix Addition
///
/// A = 01|02   B = 05|06
///     03|04       07|08
///
/// C = 01+05 | 02+06
///     03+07 | 04+08
///
impl Add for Matrix {
  type Output = Self;
  fn add(self, rhs:Self) -> Self {
    let (ar, ac, br, bc) = (self.rows, self.cols, rhs.rows, rhs.cols);
    assert_eq!(ar, br);
    assert_eq!(ac, bc);
    let mut entries: Vec<isize> = Vec::with_capacity(self.size);
    for ci in 1 .. (ar+1) {
      for cj in 1 .. (ac+1) {
        let a = self.nth(ci, cj);
        let b = rhs.nth(ci, cj);
        entries.push(a + b);
      }
    }
    Matrix::new(ar, ac, entries)
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
fn test_nth_matrix() {
  let mat = Matrix::new(2, 2, vec![
    01, 02,
    03, 04]);
  assert_eq!(mat.nth(1,1), &1);
  assert_eq!(mat.nth(1,2), &2);
  assert_eq!(mat.nth(2,1), &3);
  assert_eq!(mat.nth(2,2), &4);
}

#[test]
fn test_matrix_addition() {
  let ma = Matrix::new(2, 2, vec![
    1, 2,
    3, 4]);
  let mb = Matrix::new(2, 2, vec![
    5, 6,
    7, 8]);
  let mc = ma + mb;
  assert_eq!(mc.entries, vec![6, 8, 10, 12]);
}

#[test]
fn test_matrix_multiplication() {
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

#[test]
fn test_matrix_multiplication_associativity() {
  let ma = Matrix::new(2, 3, vec![
    2, 3, 4,
    1, 0, 0]);
  let mb = Matrix::new(3, 2, vec![
    0, 1000,
    1, 100,
    0, 10]);
  let mc = Matrix::new(2, 3, vec![
    5, 6, 7,
    1, 0, 0]);
  // 2x3 * 3x2 * 2x3
  // (2x3 * 3x2) = (2x2 * 2x3) = 2x3
  // 2x3 * (3x2 * 2x3) = 2x3 * 3x3 = 2x3
  // assert_eq!(&(&ma*&mb)*&mc, &ma*&(&mb*&mc));
  // TODO FIXME: how to get borrowed impl working? i'm clueless
  assert_eq!((ma.to_owned()*mb.to_owned())*mc.to_owned(), ma*(mb*mc));
}

#[test]
fn test_matrix_left_distributivity() {
  let ma = Matrix::new(2, 3, vec![
    2, 3, 4,
    1, 0, 0]);
  let mb = Matrix::new(3, 2, vec![
    0, 1000,
    1, 100,
    0, 10]);
  let mc = Matrix::new(2, 3, vec![
    5, 6, 7,
    1, 0, 0]);
  assert_eq!(mc*(ma+mb), (mc*ma) + (mc*mb));
}
