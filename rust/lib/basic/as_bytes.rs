#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(unused_mut)]
// https://doc.rust-lang.org/std/primitive.str.html
fn basic_as_bytes() {
  let msg = "hello";
  let msg_bytes = msg.as_bytes();
  let msg_bytes_: &[u8] = msg.as_bytes();

  let _ = b"hello" == msg_bytes;
  let _ = msg_bytes.len() == 5;
  let _ = "hello".len() == 5;

  let h = msg_bytes[0];

  let mut m = msg_bytes.to_vec();
  m[0] = 0x41;

  for elem in m.iter_mut() {
    *elem = *elem ^ 0x3;
  }

  let m2: Vec<u8> = msg_bytes.iter().map(|x| x ^ 0x3).collect();
}
