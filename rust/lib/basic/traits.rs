// https://doc.rust-lang.org/book/first-edition/traits.html

struct Phantom;

trait Trait {
  fn implement_me(&self) -> bool {
    // default method
    false
  }
}

trait TraitBar : Trait {
  fn nop(&self);
}

impl Trait for Phantom {
  fn implement_me(&self) -> bool {
    true
  }
}

impl TraitBar for Phantom {
  fn nop(&self) {}
}

fn bounded_function<T: Trait>(t: T) {
  let _ = t.implement_me();
}



struct Wrapped<T>(T);

impl<T: PartialEq> Wrapped<T> {
  fn nop(&self) -> bool {
    self.0 == self.0
  }
}



trait ApproxEqual {
  fn approx_equal(&self, rhs: &Self) -> bool;
}

impl ApproxEqual for f32 {
  fn approx_equal(&self, rhs: &Self) -> bool {
    (self - rhs).abs() <= ::std::f32::EPSILON
  }
}



fn multiple_bounds<T: Clone + PartialEq + Eq>(x: T) {
}



fn multiple_bounds_where<T>(x: T) where T: Clone + PartialEq + Eq {
}
