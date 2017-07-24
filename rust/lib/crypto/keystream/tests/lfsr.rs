#[cfg(test)]

extern crate keystream;
use keystream::lfsr::*;

#[test]
fn test_lfsr_keystream_1() {
  let constants = vec![1,0,1,1];
  let iv = vec![0,1,1,0];

  let lfsr: LFSRKeyStream = LFSRKeyStream::new(&constants, &iv);
  let bits: Vec<usize> = lfsr.into_iter().take(7).collect();
  assert_eq!(bits, vec![0,1,1,0,1,0,0]);
}

#[test]
fn test_lfsr_keystream_1_period() {
  let constants = vec![1,0,1,1];
  let iv = vec![0,1,1,0];

  let lfsr: LFSRKeyStream = LFSRKeyStream::new(&constants, &iv);
  assert_eq!(lfsr.period(), Some(7));
}

#[test]
fn test_lfsr_keystream_2() {
  let constants = vec![1,0,0,1];
  let iv = vec![0,0,0,1];

  let lfsr: LFSRKeyStream = LFSRKeyStream::new(&constants, &iv);
  let bits: Vec<usize> = lfsr.into_iter().take(15).collect();
  assert_eq!(bits, vec![0,0,0,1,1,1,1,0,1,0,1,1,0,0,1]);
}

#[test]
fn test_lfsr_keystream_2_period() {
  let constants = vec![1,0,0,1];
  let iv = vec![0,0,0,1];

  let lfsr: LFSRKeyStream = LFSRKeyStream::new(&constants, &iv);
  assert_eq!(lfsr.period(), Some(15));
}
