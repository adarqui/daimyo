use math::mod_shared::*;
use math::mod_num::{ModNum};



#[derive(Clone)]
#[derive(Debug)]
#[derive(Eq)]
#[derive(PartialEq)]
pub struct ModVec {
  v: Vec<ModNum>,
  m: ModModulus,
}



impl ModVec {
  pub fn new(m: ModModulus) -> ModVec {
    ModVec {
      v: Vec::new(),
      m: m
    }
  }
}
