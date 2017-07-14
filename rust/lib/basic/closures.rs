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



fn apply<F>(f: F) where F: FnOnce() {
  f()
}

fn apply_op<F>(n: i32, f: F) -> i32 where F: Fn(i32) -> i32 {
  f(n)
}

fn apply_mut_op<F>(mut f: F) where F: FnMut() {
  f()
}

fn apply_example() {
  let mut x = 0;
  apply(||x+=1);
  apply_op(x, |i| i*2);
  apply_mut_op(|| x=10);
}
