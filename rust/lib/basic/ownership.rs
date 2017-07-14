fn basic_ownership() {
  // variables "have ownership" of what they are bound to
  let v = vec![1,2,3];

  // rust ensures EXACTLY one binding to any given resource
  let v2 = v; // v2 owns v -> v moved to v2

  // can't use v directly anymore
  // let _ = v[0];

  let v = vec![4,5,6];
  take(v); // takes ownership -> v moved to take(v)
  // let _ = v[0];
}




fn take<T>(v: Vec<T>) {
}



fn basic_ownership_primitives() {

  // All primitive types implement the Copy trait and their ownership
  // is therefore not moved like one would assume,

  // since this u8 is not heap allocated, the "move" from v to v2,
  // is really a copy (Copy)
  let v: u8 = 10;
  let v2 = v;
  // works because v was never moved into v2, which would have transferred ownership
  let _ = v + 1;
}
