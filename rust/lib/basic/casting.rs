// https://doc.rust-lang.org/book/first-edition/casting-between-types.html



use std::mem;



// safe casts: as
// dangerous casts: transmute



fn basic_casting() {
  let n32 = 12345678u32;
  let n64 = n32 as u64;

  let a_hex = 0x41u64;
  let a_char = a_hex as u8;

  // most common case of casting, removing mutability from a ref
  let mut q: &bool = &true;
  let r = q as &bool;

  let i: i32 = 0;
  let j: f64 = i as f64;
  let one = true as u8;
  let at_sign = 64 as char;
  let two_hundred = -56i8 as u8;
}



fn basic_pointer_casting() {
  // a is a pointer to location 300
  let a = 300 as *const char;
  let b = a as u32;
}



fn basic_transmute() {
  let a = [0u8, 0u8, 0u8, 0u8];
  let mut z: u32;
  // error:
  // let b = a as u32; // Four u8s makes a u32.
  unsafe {
    let b = mem::transmute::<[u8; 4], u32>(a);
    z = b;
  }

  // transmute makes sure types are the same size
  // error:
  unsafe {
   let a = [0u8, 0u8, 0u8, 0u8];
   // let b = mem::transmute::<[u8; 4], u64>(a);
  }
}
