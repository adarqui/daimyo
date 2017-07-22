use mod_shared::*;
use mod_num::{ModNum};



// TODO FIXME
#[allow(unused_variables)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(Eq)]
#[derive(PartialEq)]
pub struct ModVec {
  v: Vec<ModNum>,
  m: ModModulus,
}



// TODO FIXME
#[allow(unused_variables)]
impl ModVec {
  pub fn new(m: ModModulus) -> ModVec {
    ModVec {
      v: Vec::new(),
      m: m
    }
  }
  pub fn to_v(&mut self, v: Vec<ModValue>) {
  }
  // pub fn from_v(&self) -> Vec<ModValue> {
  //  self.v
  // }
}
