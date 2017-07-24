#[cfg(test)]

extern crate keystream;
use keystream::lfsr::*;

#[test]
fn test_lfsr_keystream_1() {
  let constants = vec![1,0,1,1];
  let iv = vec![0,1,1,0];

  let bks: LFSRKeyStream = LFSRKeyStream::new(&constants, &iv);
  let bits: Vec<usize> = bks.into_iter().take(7).collect();
  assert_eq!(bits, vec![0,1,1,0,1,0,0]);
}

#[test]
fn test_lfsr_keystream_2() {
  let constants = vec![1,0,0,1];
  let iv = vec![0,0,0,1];

  let bks: LFSRKeyStream = LFSRKeyStream::new(&constants, &iv);
  let bits: Vec<usize> = bks.into_iter().take(15).collect();
  assert_eq!(bits, vec![0,0,0,1,1,1,1,0,1,0,1,1,0,0,1]);
}
