// https://doc.rust-lang.org/book/first-edition/casting-between-types.html



// safe casts: as
// dangerous casts: transmute



fn basic_casting() {
  let n32 = 12345678u32;
  let n64 = n32 as u64;

  let a_hex = 0x41u64;
  let a_char = a_hex as u8;

  // most common case of casting, removing mutability from a ref
  let mut q: &bool = true;
  let r = &q as q;
}
