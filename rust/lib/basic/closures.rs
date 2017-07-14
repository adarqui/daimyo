#[allow(dead_code)]
#[allow(unused_assignments)]
#[allow(unused_variables)]
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



#[allow(dead_code)]
fn call<F: Fn()>(f: F) {
  f();
}

#[allow(dead_code)]
fn func() {
}

#[allow(dead_code)]
fn call_example() {
  let closure = || ();
  call(func);
  call(closure);
}



#[allow(dead_code)]
fn apply<F>(f: F) where F: FnOnce() {
  f()
}

#[allow(dead_code)]
fn apply_op<F>(n: i32, f: F) -> i32 where F: Fn(i32) -> i32 {
  f(n)
}

#[allow(dead_code)]
fn apply_mut_op<F>(mut f: F) where F: FnMut() {
  f()
}

#[allow(dead_code)]
fn apply_example() {
  let mut x = 0;
  apply(||x+=1);
  apply_op(x, |i| i*2);
  apply_mut_op(|| x=10);
}
