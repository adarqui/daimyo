// https://doc.rust-lang.org/book/first-edition/type-aliases.html



use std::result;



type CamelCase = u8;



type snake_case = u8;



type U8Result<T> = result::Result<T, u8>;
type Result<T> = result::Result<T, u8>;



fn basic_aliases() {
  let x: CamelCase = 0;
  let y: snake_case = 1;
}
