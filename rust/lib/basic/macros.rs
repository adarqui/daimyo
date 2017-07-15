// https://doc.rust-lang.org/book/first-edition/macros.html



use std;
use std::io::Result;



macro_rules! foo {
  (x => $e:expr) => (println!("mode X: {}", $e));
  (y => $e:expr) => (println!("mode Y: {}", $e));
}



macro_rules! my_vec {
  ( $( $x:expr ),* ) => {
    {
      let mut temp_vec = Vec::new();
      $(
        temp_vec.push($x);
      )*
      temp_vec
    }
  };
}



fn basic_macros() -> std::io::Result<()> {
  foo!(y => 3);
  let v = my_vec![1,2,3];

  // better than below
  let r: Result<()> = Ok(());
  let f = try!(r);

  // messy
  let _ = match r {
     Ok(t) => t,
     Err(e) => return Err(e),
   };


  Ok(())
}
