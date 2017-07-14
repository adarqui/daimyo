#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(unused_assignments)]



fn basic_variables() {
  let immutable_u8: u8 = 1;
  let mut mutable_u8: u8 = 0;
  mutable_u8 = immutable_u8;

  let imm_u8_binary: u8 = 0b10010;
  let imm_u8_hex: u8 = 0xFE;
  let imm_u8_octal: u8 = 0o100;

  let t: bool = true;
  let f = !t;

  let underscore_separated_u64 = 1_000_000_000_000u64;

  let all_bits = 0b11111111u8;

  let unit = ();
}
