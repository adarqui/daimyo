// https://doc.rust-lang.org/book/first-edition/strings.html



fn basic_strings() {
  // two main string types: &str, String
  let a: &str = "string slice. fixed size. immutable.";
  let b: String = String::from("heap allocated string. gowable. mutable.");

  // convert &str into String
  let b_: String = a.to_string();
  // coerce String into &str
  let a_: &str = &b;

  // contains newlines
  let s = "foo
           bar";

  // doesn't contain newlines
  let s = "foo\
           bar";

  // Because strings are valid UTF-8, they do not support indexing:
  // error: let _ = s[0];
}



fn basic_string_indexing() {
  let s = "hello";
  let _ = s.chars().nth(3);
}



fn basic_string_concatenation() {
  // error: let _ = "hello" + "ok"
  let s = "hello".to_owned() + "ok";
  let t = s + "hey";
}
