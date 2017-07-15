// https://doc.rust-lang.org/book/first-edition/closures.html



fn basic_closures() {
  fn cb_fn (i: u32) -> u32 { i + 1 };
  let cb_closure = |i| i + 1;
  let cb_closure_ = |i: u32| -> u32 { i+1 };
  let cb_closure_no_arguments = || ();

  let mut n = 0;
  n = cb_fn(n); 
  n = cb_closure(n);
  n = cb_closure_(n);

  let mutable_closure = |mut i| i += 1;
  mutable_closure(n);
}




fn basic_closures_illegal() {
  // illegal
  let mut num = 5;
  let plus_num = |x: i32| x + num;

  // let y = &mut num;

  // legal
  let mut num = 5;
  { let plus_num = |x: i32| x + num; }
  let y = &mut num;
}




fn basic_closures_move() {
  let mut num = 5;
  {
    // mutable refernence to num
    let mut add_num = |x| num += x;
    add_num(5)
  }
  let _ = num == 6;

  {
    // ownership copy of num
    // gets its own stack
    let mut add_num = move |x| num += x;
    add_num(5);
  }
  let _ = num == 5;
}



// function pointer
fn nop() {
}

fn basic_function_pointer() {
  let f = nop;
          // call: defined below
  let _ = call(&f);
}



// returning closures
fn factory() -> Box<Fn(bool) -> bool> {
  // error: |x| !x
  // error: |x: bool| -> bool { !x }
  Box::new(move |x| x)
}

fn basic_returning_closures() {
  let f = factory();
  let _ = f(true) == false;
}



fn call<F: Fn()>(f: F) {
  f();
}

fn func() {
}

fn call_example() {
  let closure = || ();
  call(func);
  call(closure);
}



// trait system overloads operators
// three separate traits to overload with: Fn, FnMut, FnOnce
// || {} is syntax for closures is sugar for these three traits ^^



// static dispatch with Fn* traits
fn static_apply<F>(f: F) where F: FnOnce() {
  f()
}

fn static_apply_op<F>(n: i32, f: F) -> i32 where F: Fn(i32) -> i32 {
  f(n)
}

fn static_apply_mut_op<F>(mut f: F) where F: FnMut() {
  f()
}

fn static_apply_example() {
  let mut x = 0;
  static_apply(||x+=1);
  static_apply_op(x, |i| i*2);
  static_apply_mut_op(|| x=10);
}




// dynamic dispatch with Fn* traits
fn dynamic_apply_op(n: i32, f: &Fn(i32) -> i32) -> i32 {
  f(n)
}

fn dynamic_apply_example() {
  let mut x = 0;
  dynamic_apply_op(x, &|i| i*2);
}
