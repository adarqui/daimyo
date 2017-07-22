use std::ascii::AsciiExt;
use std::char;



pub fn char_to_base(c: char) -> u32 {
    (c.to_ascii_lowercase() as u32 - 97)
}



pub fn base_to_char(x: u32) -> char {
    char::from_u32(x as u32 + 97).unwrap()
}
