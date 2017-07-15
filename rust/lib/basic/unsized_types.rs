// https://doc.rust-lang.org/book/first-edition/unsized-types.html



// unsized types
// example: [T] - no idea how big T's are



struct Foo<T: ?Sized> {
  f: T
}
