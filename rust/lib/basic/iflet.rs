// https://doc.rust-lang.org/book/first-edition/if-let.html



fn basic_iflet() {
  let option = Some(true);

  match option {
    Some(x) => x,
    _       => false
  };

  if option.is_some() {
    option.unwrap();
  }

  // better
  if let Some(x) = option {
    x
  } else {
    false
  };

  ()
}



fn basic_whilelet() {
  let mut v = vec![1..5];
  while let Some(x) = v.pop() {
  }
}
