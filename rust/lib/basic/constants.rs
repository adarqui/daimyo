// static: not inlined.
static LANGUAGE: &'static str = "rust";
static mut COUNTER: u32 = 0;



fn basic_mut_constants() {
  let current_counter: u32;

  unsafe {
    COUNTER += 1;
  }

  unsafe {
    current_counter = COUNTER;
  }
}



// const: inlined, doesn't occupy memory
const SOME_CONSTANT: bool = true;
