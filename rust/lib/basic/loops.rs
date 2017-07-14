use std::iter::Iterator;



fn basic_loops() {
  let mut i: u32 = 0;
  loop {
    if i == 10 {
      break
    }
    i += 1
  }

  i = 0;
  while i < 10 {
    i+=1
  }

  // upper bound is inclusive
  for _ in 0..10 {
  }

  // enumerating the range
  for (i, v) in (5..10).enumerate() {
  }

  // enumerating on iterators
  let lines = "1\n2\n\n".lines();
  for (line_no, line) in lines.enumerate() {
  }
}



fn basic_nested_loops() {
  'loop1: loop {
    'loop2: loop {
      break 'loop1;
    }
  }
}
