#[cfg(test)]

extern crate num;

extern crate util;
extern crate matrix;
extern crate modulo;

use num::PrimInt;

use modulo::mod_num::ModuloSignedExt;

use matrix::matrix::Matrix;
use matrix::matrix::*;

#[test]
fn test_matrix() {
  let mat = Matrix::new(2, 2, vec![
    01, 02,
    03, 04]);
  assert_eq!(mat.rows(), 2);
  assert_eq!(mat.cols(), 2);
  assert_eq!(mat.size(), 4);
}

#[test]
fn test_identity_matrix_1() {
  let mat = Matrix::new(2, 2, vec![
    01, 02,
    03, 04]);
  assert_eq!(mat.rows(), 2);
  assert_eq!(mat.cols(), 2);
  assert_eq!(mat.size(), 4);
  assert_eq!(mat.identity().entries(), vec![
    01, 00,
    00, 01]);
}

#[test]
fn test_identity_matrix_2() {
  let mat = Matrix::new(3, 3, vec![
    01, 02, 05,
    03, 04, 06,
    07, 08, 09]);
  assert_eq!(mat.identity().entries(), vec![
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
  assert_eq!(m.set_nth(1, 2, 1).entries(), vec![0,1,0,0]);
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
  assert_eq!(mc.entries(), vec![6, 8, 10, 12]);
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
  assert_eq!((&ma + &(&mb + &mc)).entries(), vec![7, 9, 11, 13]);
  assert_eq!((&(&ma + &mb) + &mc).entries(), vec![7, 9, 11, 13]);
}

#[test]
fn test_matrix_commutative_addition() {
  let ma = Matrix::new(2, 2, vec![
    1, 2,
    3, 4]);
  let mb = Matrix::new(2, 2, vec![
    5, 6,
    7, 8]);
  assert_eq!((&ma + &mb).entries(), vec![6, 8, 10, 12]);
  assert_eq!((&mb + &ma).entries(), vec![6, 8, 10, 12]);
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
  assert_eq!(mc.entries(), vec![-4, -4, -4, -4]);
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
  assert_eq!(mc.entries(), vec![3, 2340, 0, 1000]);
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
  assert_eq!((ma*2).entries(), vec![2, 2, 2, 2]);

  let ma = Matrix::new(2, 2, vec![1, 1, 1, 1]);
  assert_eq!((2*ma).entries(), vec![2, 2, 2, 2]);
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
fn test_matrix_is_invertible() {
  let m = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  assert_eq!(m.is_invertible(), true);
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
  assert_eq!(m.diagonal().entries(), vec![
    1,0,0,
    0,5,0,
    0,0,9]);
  assert_eq!(m.diagonal().entries(), m.right_diagonal().entries());
  assert_eq!(m.diagonal(), m.diagonal());
}

#[test]
fn test_left_diagonal_matrix() {
  let m = Matrix::new(3, 3, vec![
    1,2,3,
    4,5,6,
    7,8,9]);
  assert_eq!(m.left_diagonal().entries(), vec![
    0,0,3,
    0,5,0,
    7,0,0]);
  assert_eq!(m.left_diagonal().entries(), m.left_diagonal().entries());
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

  let m_3x3 = Matrix::new(3, 3, vec![
    6,  1, 1, 
    4, -2, 5,
    2,  8, 7]);
  assert_eq!(m_3x3.det(), -306);

  let m_3x3 = Matrix::new(3, 3, vec![
    10,05,12,
    03,14,21,
    08,09,11]);
  assert_eq!(m_3x3.det().modulo(26), 7);

  let m_4x4 = Matrix::new(4, 4, vec![
    3,2,0,1,
    4,0,1,2,
    3,0,2,1,
    9,2,3,1]);
  assert_eq!(m_4x4.det(), 24);
}

#[test]
fn test_det_matrix_multiplication_rule() {
  let m_2x2 = Matrix::new(3, 3, vec![
    11,8,9,
     3,7,5,
     2,5,8]);

  let m_3x2 = Matrix::new(3, 3, vec![
    1,3,6,
    4,1,1,
    2,5,7]);

  assert_eq!(m_3x2.det()*m_2x2.det(), (m_3x2*m_2x2).det());
}

#[test]
fn test_det_of_identity_matrix() {
  let m = Matrix::new(3, 3, vec![
    1,3,6,
    4,1,1,
    2,5,7]);
  assert_eq!(m.identity().det(), 1);
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
fn test_minor_matrix() {
  let m = Matrix::new(3, 3, vec![
    1, 4, 7,
    3, 0, 5,
   -1, 9, 11]);
  assert_eq!(m.minor(2, 3), 13);
}

#[test]
fn test_cofactor_matrix() {
  let m = Matrix::new(3, 3, vec![
    1, 4, 7,
    3, 0, 5,
   -1, 9, 11]);
  assert_eq!(m.cofactor(2, 3), -13);
}

#[test]
fn test_minors_matrix() {
  let m = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  assert_eq!(m.minors().entries(), vec![
    -13,2,18,
    -4,-2,-1,
    7,-5,-11]);
}

#[test]
fn test_cofactors_matrix() {
  let m = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  assert_eq!(m.cofactors().entries(), vec![
    -13,-2,18,
    4,-2,1,
    7,5,-11]);
}

#[test]
fn test_adjugate_matrix() {
  let m = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  assert_eq!(m.adjugate().entries(), vec![
    -13,4,7,
    -2,-2,5,
    18,1,-11]);
}

#[test]
fn test_adjugate_matrix_law_1() {
  let m = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  assert_eq!(&m * &m.adjugate(), m.det() * m.identity());
}

#[test]
fn test_adjugate_matrix_law_identity() {
  let m = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  assert_eq!(m.identity().adjugate(), m.identity());
}

#[test]
fn test_adjugate_matrix_law_multiplication() {
  let ma = Matrix::new(3, 3, vec![
    9,1,5,
    4,6,4,
    1,2,3]);
  let mb = Matrix::new(3, 3, vec![
    1,3,2,
    4,1,3,
    2,5,2]);
  // adj(AB) = adj(B)*adj(A) -- NOT adj(A)*adj(B)
  assert_eq!((&ma * &mb).adjugate(), mb.adjugate() * ma.adjugate());
}

#[test]
fn test_inverse_matrix() {
  let m = Matrix::new(2, 2, vec![
    11,08,
    03,07]);
  assert_eq!(m.minors().entries(), vec![
    07,03,
    08,11]);
  assert_eq!(m.cofactors().entries(), vec![
    07,-3,
    -8,11]);
  assert_eq!(m.adjugate().entries(), vec![
    07,-8,
    -3,11]);
  assert_eq!(m.inverse_unsafe().entries(), vec![
    07/53,-8/53,
    -3/53,11/53]);
}

#[test]
fn test_inverse_mod_matrix() {
  let m = Matrix::new(2, 2, vec![
    11,08,
    03,07]);
  assert_eq!(m.minors().entries(), vec![
    07,03,
    08,11]);
  assert_eq!(m.cofactors().entries(), vec![
    07,-3,
    -8,11]);
  assert_eq!(m.adjugate().entries(), vec![
    07,-8,
    -3,11]);
  assert_eq!(m.inverse_mod_unsafe(26).entries(), vec![
    07,18,
    23,11]);
}

#[test]
fn test_inverse_mod_matrix_identity_inverse_law() {
  let m = Matrix::new(2, 2, vec![
    11,08,
    03,07]);
  let m_inv = Matrix::new(2, 2, vec![
    7,18,
    23,11]);
  assert_eq!(&m * &m_inv, Matrix::new(2, 2, vec![
    261,286,
    182,131]));

  let v: Vec<isize> = (&m * &m_inv).entries().into_iter().map(|x| x.modulo(26)).collect();
  assert_eq!(v, identity_matrix(2, 2).entries());

  assert_eq!((&m * &m_inv).map(|x| x.modulo(26)), identity_matrix(2, 2));
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
