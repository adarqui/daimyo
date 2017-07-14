#![allow(dead_code)]
#![allow(unused_variables)]



enum Color {
  Red = 0x01,
  Green = 0x02,
  Blue = 0x04
}



fn basic_matching() {
  let v = 0;
  match v {
    1 => println!("1"),
    _ => println!("x")
  }

  let color = Color::Red;
  match color {
    Color::Red => println!("red"),
    Color::Green => println!("green"),
    Color::Blue => println!("blue")
  }

  let n = 10;
  match n {
    x if n > 10 => x,
    _ => n
  };

  match n {
    x @ 0...5 => (),
    x @ 6...10 => (),
    x => ()
  }

  let opt = Some(true);
  if let Some(b) = opt {
    println!("{}", b);
  }

  let mut z = Some(0);
  while let Some(z_) = z {
    z = None;
  }

  ()
}
