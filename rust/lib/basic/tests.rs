// https://doc.rust-lang.org/book/first-edition/testing.html



#[test]
#[should_panic]
fn test_should_panic() {
  assert_eq!("Hello", "world");
}



#[test]
#[ignore]
fn test_expensive() {
  // this test is ignored with: cargo test
  // it is executed with: cargo test -- --ignored
}



// doctest!



/// This function adds two to its argument.
///
/// # Examples
///
/// ```
/// use daimyo::basic;
/// assert_eq!(4, basic::tests::add_two(2));
/// ```
pub fn add_two(a: i32) -> i32 {
  a + 2
}
