#[cfg(test)]

extern crate keystream;
use keystream::binary::*;

#[test]
fn test_binary_keystream() {
  let bks: BinaryKeyStream = BinaryKeyStream::new(&vec![1,0,0,0]);
  let bits: Vec<usize> = bks.into_iter().take(15).collect();
  assert_eq!(bits, vec![1,0,0,0,1,0,0,1,1,0,1,0,1,1,1]);
}

#[test]
fn test_binary_keystream_zeroes() {
  let bks: BinaryKeyStream = BinaryKeyStream::new(&vec![0,0,0,0]);
  let bits: Vec<usize> = bks.into_iter().take(15).collect();
  assert_eq!(bits, vec![1,0,0,0,1,0,0,1,1,0,1,0,1,1,1]);
}
