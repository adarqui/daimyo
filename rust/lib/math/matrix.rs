// https://en.wikipedia.org/wiki/Matrix_(mathematics)



#[allow(unused_imports)]
use std::io::Write;
use std::ops::Add;
use std::ops::Sub;
use std::ops::Mul;
#[allow(unused_imports)]
use num::PrimInt;
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

  /// new()
  ///
  /// new rows x cols sized matrix with a flat representation of entries
  ///
  /// new(2, 2, vec![1, 2, 3, 4])
  ///
  /// 1 2
  /// 3 4
  ///
  fn new(rows: usize, cols: usize, entries: Vec<isize>) -> Self {
    assert_eq!(rows * cols, entries.len());
    Matrix {
      rows: rows,
      cols: cols,
      size: rows * cols,
      entries: entries
    }
  }

  fn nth_offset(&self, row: usize, col: usize) -> usize {
    (((row-1) * self.cols) + col) - 1
  }

  /// nth()
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
  fn nth(&self, row: usize, col: usize) -> isize {
    assert!(row > 0 && col > 0, "row > 0 && col > 0");
    let offset = self.nth_offset(row, col);
    self.entries.get(offset).unwrap().to_owned()
  }

  /// set_nth()
  ///
  /// set_nth(1, 2, v):
  /// 0 0 = 0 v
  /// 0 0   0 0
  ///
  fn set_nth(&mut self, row: usize, col: usize, value: isize) -> &Self {
    let offset = self.nth_offset(row, col);
    self.entries[offset] = value;
    self
  }

  /// row()
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

  /// col()
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

  /// identity()
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

  /// is_square()
  ///
  fn is_square(&self) -> bool {
    self.rows == self.cols
  }

  /// is_invertible()
  ///
  fn is_invertible(&self) -> bool {
    // TODO
    false
  }

  /// transpose()
  ///
  /// 1 2 3 = 1 4 = 1 4 2 5 3 6
  /// 4 5 6   2 5
  ///         3 6
  ///
  /// (1,1),(2,1),(1,2),(2,2),(1,3),(2,3)
  ///
  fn transpose(&self) -> Matrix {
    let mut entries = Vec::with_capacity(self.size);
    for j in 1 .. self.cols+1 {
      for i in 1 .. self.rows+1 {
        let e = self.nth(i,j);
        entries.push(e.to_owned());
      }
    }
    Matrix::new(self.cols, self.rows, entries)
  }

  /// diagonal()
  ///
  /// 1 2 3 = 1 0 0
  /// 4 5 6   0 5 0
  /// 7 8 9   0 0 9
  ///
  fn diagonal(&self) -> Matrix {
    assert!(self.is_square() == true, "must be a square matrix");
    let mut entries: Vec<isize> = Vec::with_capacity(self.size);
    for r in 1 .. self.rows+1 {
      for c in 1 .. self.cols+1 {
        if r == c {
          let e = self.nth(r, c);
          entries.push(e.to_owned())
        } else {
          entries.push(0)
        }
      }
    }
    Matrix::new(self.rows, self.cols, entries)
  }

  /// right_diagonal()
  ///
  fn right_diagonal(&self) -> Matrix {
    self.diagonal()
  }

  /// left_diagonal()
  ///
  /// 1,2,3 = 0,0,3
  /// 4,5,6   0,5,0
  /// 7,8,9   7,0,0
  ///
  fn left_diagonal(&self) -> Matrix {
    assert!(self.is_square() == true, "must be a square matrix");
    let mut entries: Vec<isize> = Vec::with_capacity(self.size);
    let mut i_r = 1;
    let mut i_c = self.cols;
    for r in 1 .. self.rows+1 {
      for c in 1 .. self.cols+1 {
        if r == i_r && c == i_c {
          let e = self.nth(r, c);
          entries.push(e.to_owned());
          i_r += 1;
          i_c -= 1;
        } else {
          entries.push(0)
        }
      }
    }
    Matrix::new(self.rows, self.cols, entries)
  }

  /// det()
  ///
  /// determinant - TODO: extend to arbitrary sized matrix
  ///
  fn det(&self) -> isize {
    assert!(self.is_square(), "must be a square matrix");
    match (self.rows, self.cols) {
      (1,1) => self.det_1x1(),
      (2,2) => self.det_2x2(),
      _     => self.det_NxN()
    }
  }

  /// det_1x1()
  ///
  /// det a = a
  ///
  fn det_1x1(&self) -> isize {
    self.nth(1,1)
  }

  /// det_2x2()
  ///
  /// det a b = ad - bc
  ///     c d
  ///
  fn det_2x2(&self) -> isize {
    let (a, b, c, d) = (self.nth(1,1), self.nth(1,2), self.nth(2,1), self.nth(2,2));
    a*d - b*c
  }

  /// det_NxN()
  ///
  /// Definition 1.5 of Cryptography Theory & Practice:
  ///
  /// Suppose that A = (a_ij) is an m x m matrix.
  ///  For
  ///   1 <= i <= m,
  ///   1 <= j <= m,
  ///  Define A_ij to be the matrix obtained from A by:
  ///   deleting the ith row
  ///   deleting the jth column
  ///
  ///  The determinant of A, denoted det_A, is:
  ///   the value a_1,1 if m = 1.
  ///   if m > 1, then det_A is computed recursively from the formula:
  ///    det A = SUM(j=1 to m) of ((-1)^i+j)a_i,j(det A_ij),
  ///     where i is any fixed integer between 1 and m.
  ///
  #[allow(non_snake_case)]
  fn det_NxN(&self) -> isize {
    self.det_NxN_helper(1 as isize, 1 as isize)
  }

  #[allow(non_snake_case)]
  fn det_NxN_helper(&self, i: isize, j: isize) -> isize {
    if j > self.cols {
      return 0
    }
    ((-1).pow((i+j) as u32)) * self.nth(i, j) * self.det_NxN_helper(i, j+1)
  }

  /// zeroes()
  ///
  fn zeroes(&self) -> Matrix {
    zero_fill_matrix(self.rows, self.cols)
  }

  /// negate()
  ///
  fn negate(&self) -> Matrix {
    // let entries: Vec<isize> = self.entries.to_owned().into_iter().map(|x| x * (-1)).collect();
    // Matrix::new(self.rows, self.cols, entries)
    (-1) * self.to_owned()
  }

  /// swap_rows()
  ///
  #[allow(unused_variables)]
  fn swap_rows(&mut self, row_a: usize, row_b: usize) -> &Self {
    self
  }

  /// add_rows()
  ///
  #[allow(unused_variables)]
  fn add_rows(&mut self, row_a: usize, row_b: usize) -> &Self {
    self
  }

  /// add_row_by()
  ///
  #[allow(unused_variables)]
  fn add_row_by(&mut self, row: usize, scalar: isize) -> &Self {
    self
  }

  /// mul_rows()
  ///
  #[allow(unused_variables)]
  fn mul_rows(&mut self, row_a: usize, row_b: usize) -> &Self {
    self
  }

  /// mul_row_by()
  ///
  #[allow(unused_variables)]
  fn mul_row_by(&mut self, row_a: usize, scalar: isize) -> &Self {
    self
  }

  /// swap_cols()
  ///
  #[allow(unused_variables)]
  fn swap_cols(&mut self, col_a: usize, col_b: usize) -> &Self {
    self
  }

  /// add_cols()
  ///
  #[allow(unused_variables)]
  fn add_cols(&mut self, col_a: usize, col_b: usize) -> &Self {
    self
  }

  /// add_col_by()
  ///
  #[allow(unused_variables)]
  fn add_col_by(&mut self, col: usize, scalar: isize) -> &Self {
    self
  }

  /// mul_cols()
  ///
  #[allow(unused_variables)]
  fn mul_cols(&mut self, col_a: usize, col_b: usize) -> &Self {
    self
  }

  /// mul_col_by()
  ///
  #[allow(unused_variables)]
  fn mul_col_by(&mut self, col_a: usize, scalar: isize) -> &Self {
    self
  }

  /// submatrix()
  ///
  /// submatrix(3, 2)
  /// 1 2  3  4  = 1 3 4
  /// 5 6  7  8    5 7 8
  /// 9 10 11 12
  fn submatrix(&self, row: usize, col: usize) -> Matrix {
    let row_count = if row > 0 { 1 } else { 0 };
    let col_count = if col > 0 { 1 } else { 0 };
    let new_size = (self.rows - row_count) * (self.cols - col_count);
    let mut entries: Vec<isize> = Vec::with_capacity(new_size);
    for i in (1 .. self.rows+1).filter(|r| r.to_owned() != row) {
      for j in (1 .. self.cols+1).filter(|c| c.to_owned() != col) {
        let e = self.nth(i, j);
        entries.push(e);
      }
    }
    Matrix::new(self.rows - row_count, self.cols - col_count, entries)
  }

  /// remove_row()
  ///
  fn remove_row(&self, _: usize) -> Matrix {
    Matrix::new(1,1,vec![1])
  }

  /// remove_col()
  ///
  fn remove_col(&self, _: usize) -> Matrix {
    Matrix::new(1,1,vec![1])
  }

  // TODO FIXME: These should probably be in another trait
  // filter, map, .. etc

  /// filter()
  ///
  fn filter<F>(&self, _: F) -> Matrix where F: FnOnce() {
    Matrix::new(1,1,vec![0])
  }

  /// map()
  ///
  fn map<F>(&self, _: F) -> Matrix where F: FnOnce() {
    Matrix::new(1,1,vec![0])
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



/// Matrix Subtraction
///
/// A = 01|02   B = 05|06
///     03|04       07|08
///
/// C = 01-05 | 02-06
///     03-07 | 04-08
///
impl Sub for Matrix {
  type Output = Self;
  fn sub(self, rhs:Self) -> Self {
    let (ar, ac, br, bc) = (self.rows, self.cols, rhs.rows, rhs.cols);
    assert_eq!(ar, br);
    assert_eq!(ac, bc);
    let mut entries: Vec<isize> = Vec::with_capacity(self.size);
    for ci in 1 .. ar+1 {
      for cj in 1 .. ac+1 {
        let a = self.nth(ci, cj);
        let b = rhs.nth(ci, cj);
        entries.push(a - b);
      }
    }
    Matrix::new(ar, ac, entries)
  }
}



/*
 * borrow impl?
 */
impl<'a> Mul for &'a Matrix {
  type Output = Matrix;
  fn mul(self, rhs: &'a Matrix) -> Matrix {
    // TODO FIXME: lol.
    // makes my life easier for now though..
    self.to_owned() * rhs.to_owned()
  }
}

impl<'a> Add for &'a Matrix {
  type Output = Matrix;
  fn add(self, rhs: &'a Matrix) -> Matrix {
    self.to_owned() + rhs.to_owned()
  }
}

/*
 * Scalar multiplication for a Matrix
 */
impl Mul<isize> for Matrix {
  type Output = Matrix;
  fn mul(self, scalar: isize) -> Matrix {
    let v: Vec<isize> = self.entries.into_iter().map(|x| scalar * x).collect();
    Matrix::new(self.rows, self.cols, v)
  }
}

/*
 * TODO FIXME: Why do I have to do this twice (for both sides)? WTF?
 *
 * Scalar multiplication for a Matrix
 */
impl Mul<Matrix> for isize {
  type Output = Matrix;
  fn mul(self, matrix: Matrix) -> Matrix {
    let scalar = self;
    let v: Vec<isize> = matrix.entries.into_iter().map(|x| scalar * x).collect();
    Matrix::new(matrix.rows, matrix.cols, v)
  }
}



/*
 * Helpers
 */

/// fill_matrix
///
fn fill_matrix(rows: usize, cols: usize, value: isize) -> Matrix {
  let mut entries: Vec<isize> = Vec::with_capacity(rows * cols);
  for _ in 1 .. (rows * cols)+1 {
    entries.push(value);
  }
  Matrix::new(rows, cols, entries)
}



/// zero_fill_matrix
///
#[allow(dead_code)]
fn zero_fill_matrix(rows: usize, cols:usize) -> Matrix {
  fill_matrix(rows, cols, 0)
}



/// one_fill_matrix
///
#[allow(dead_code)]
fn one_fill_matrix(rows: usize, cols:usize) -> Matrix {
  fill_matrix(rows, cols, 1)
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
fn test_identity_matrix_1() {
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
fn test_identity_matrix_2() {
  let mat = Matrix::new(3, 3, vec![
    01, 02, 05,
    03, 04, 06,
    07, 08, 09]);
  assert_eq!(mat.identity().entries, vec![
    01, 00, 00,
    00, 01, 00,
    00, 00, 01]);
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
  assert_eq!(mat.nth(1,1), 1);
  assert_eq!(mat.nth(1,2), 2);
  assert_eq!(mat.nth(2,1), 3);
  assert_eq!(mat.nth(2,2), 4);
}

#[test]
fn test_set_nth_matrix() {
  let mut m = Matrix::new(2, 2, vec![
    0,0,
    0,0]);
  assert_eq!(m.set_nth(1, 2, 1).entries, vec![0,1,0,0]);
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
fn test_matrix_associative_addition() {
  let ma = Matrix::new(2, 2, vec![
    1, 2,
    3, 4]);
  let mb = Matrix::new(2, 2, vec![
    5, 6,
    7, 8]);
  let mc = Matrix::new(2, 2, vec![
    1, 1,
    1, 1]);
  assert_eq!((&ma + &(&mb + &mc)).entries, vec![7, 9, 11, 13]);
  assert_eq!((&(&ma + &mb) + &mc).entries, vec![7, 9, 11, 13]);
}

#[test]
fn test_matrix_commutative_addition() {
  let ma = Matrix::new(2, 2, vec![
    1, 2,
    3, 4]);
  let mb = Matrix::new(2, 2, vec![
    5, 6,
    7, 8]);
  assert_eq!((&ma + &mb).entries, vec![6, 8, 10, 12]);
  assert_eq!((&mb + &ma).entries, vec![6, 8, 10, 12]);
  assert_eq!(&ma + &mb, &mb + &ma);
}

#[test]
fn test_matrix_additive_identity() {
  let m = Matrix::new(2, 2, vec![
    1,2,
    3,4]);
  let zero_m = zero_fill_matrix(2, 2);
  assert_eq!(m.to_owned() + zero_m, m);
  assert_eq!(m.to_owned() + m.zeroes(), m);
}

#[test]
fn test_matrix_additive_inverse() {
  let m = Matrix::new(2, 2, vec![
    1,2,
    3,4]);
  let inv_m = m.negate();
  let zero_m = zero_fill_matrix(2, 2);
  assert_eq!(m.to_owned() + inv_m, zero_m);
}

#[test]
fn test_matrix_subtraction() {
  let ma = Matrix::new(2, 2, vec![
    1, 2,
    3, 4]);
  let mb = Matrix::new(2, 2, vec![
    5, 6,
    7, 8]);
  let mc = ma - mb;
  assert_eq!(mc.entries, vec![-4, -4, -4, -4]);
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
fn test_matrix_multiplication_of_identity() {
  let ma = Matrix::new(3, 3, vec![
    2, 3, 4,
    9, 0, 2,
    1, 1, 0]);
  assert_eq!(ma.identity() * ma.to_owned(), ma);
}

#[test]
fn test_matrix_multiplication_associativity_borrow_impl() {
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
  assert_eq!(&(&ma*&mb)*&mc, &ma*&(&mb*&mc));
}

#[test]
fn test_matrix_scalar_multiplication() {
  let ma = Matrix::new(2, 2, vec![1, 1, 1, 1]);
  assert_eq!((ma*2).entries, vec![2, 2, 2, 2]);

  let ma = Matrix::new(2, 2, vec![1, 1, 1, 1]);
  assert_eq!((2*ma).entries, vec![2, 2, 2, 2]);
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
  assert_eq!((ma.to_owned()*mb.to_owned())*mc.to_owned(), ma*(mb*mc));
}

#[test]
fn test_matrix_left_distributivity() {
  /*
  let ma = Matrix::new(2, 3, vec![
    2, 3, 4,
    1, 0, 0]);
  let mb = Matrix::new(3, 2, vec![
    0, 1000, 1,
    1, 100, 2]);
  let mc = Matrix::new(2, 3, vec![
    5, 6, 7,
    1, 0, 0]);
  assert_eq!(&mc*&(&ma+&mb), (&mc*&ma) + (&mc*&mb));
  */
}

#[test]
fn test_matrix_is_square() {
  let ma = Matrix::new(2, 3, vec![
    2, 3, 4,
    1, 0, 0]);
  assert_eq!(ma.is_square(), false);

  let mb = Matrix::new(2, 2, vec![0, 1, 2, 3]);
  assert_eq!(mb.is_square(), true);
}

#[test]
fn test_transpose_matrix() {
  /// 1 2 3 = 1 4
  /// 4 5 6   2 5
  ///         3 6
  let ma = Matrix::new(2, 3, vec![
    1, 2, 3,
    4, 5, 6]);
  let mb = Matrix::new(3, 2, vec![
    1, 4,
    2, 5,
    3, 6]);
  assert_eq!(ma.to_owned().transpose(), mb);
  assert_eq!(mb.to_owned().transpose(), ma);
}

#[test]
fn test_diagonal_matrix() {
  let m = Matrix::new(3, 3, vec![
    1,2,3,
    4,5,6,
    7,8,9]);
  assert_eq!(m.diagonal().entries, vec![
    1,0,0,
    0,5,0,
    0,0,9]);
  assert_eq!(m.diagonal().entries, m.right_diagonal().entries);
  assert_eq!(m.diagonal(), m.diagonal());
}

#[test]
fn test_left_diagonal_matrix() {
  let m = Matrix::new(3, 3, vec![
    1,2,3,
    4,5,6,
    7,8,9]);
  assert_eq!(m.left_diagonal().entries, vec![
    0,0,3,
    0,5,0,
    7,0,0]);
  assert_eq!(m.left_diagonal().entries, m.left_diagonal().entries);
  assert_eq!(m.left_diagonal(), m.left_diagonal());
}

#[test]
fn test_det_matrix() {
  let m_1x1 = Matrix::new(1, 1, vec![1]);
  assert_eq!(m_1x1.det(), 1);

  let m_2x2 = Matrix::new(2, 2, vec![
    11,8,
     3,7]);
  assert_eq!(m_2x2.det(), 53);

  let m_3x3 = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  assert_eq!(m_3x3.det(), 17);
}

#[test]
fn test_fill_matrix() {
  assert_eq!(fill_matrix(2, 2, 0), Matrix::new(2, 2, vec![0,0,0,0]));
  assert_eq!(zero_fill_matrix(2, 2), Matrix::new(2, 2, vec![0,0,0,0]));
  assert_eq!(one_fill_matrix(2, 2), Matrix::new(2, 2, vec![1,1,1,1]));
}

#[test]
fn test_submatrix() {
  let m = Matrix::new(3, 4, vec![
    1,2, 3, 4,
    5,6, 7, 8,
    9,10,11,12]);
  let sub = Matrix::new(2, 3, vec![
    1,3,4,
    5,7,8]);
  assert_eq!(m.submatrix(3, 2), sub);
}

#[test]
fn test_matrix_misc_pow() {
  assert_eq!((-1).pow(1), -1);
  assert_eq!((-1).pow(2), 1);
  assert_eq!((-1).pow(3), -1);
  assert_eq!((-1).pow(4), 1);

  let i: isize = 2;
  assert_eq!((-1).pow(i as u32), 1);
}

#[test]
fn test_matrix_misc_isize_coerce() {
  let i: isize = 1;
  let j: isize = 1;
  assert_eq!(i as u32, 1);
  assert_eq!((i + j) as u32, 2);
}
