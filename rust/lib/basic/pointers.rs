#[allow(dead_code)]
#[allow(unused_variables)]
fn basics_pointers() {
  let ref_n: &i32 = &4;
  let n = *ref_n;

  match ref_n {
    &x => ()
  }

  match *ref_n {
    x => ()
  }

  match n {
    ref x => ()
  }

  let ref ref_n_ = 4;
  let n_ = *ref_n_;
}
