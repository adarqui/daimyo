#[allow(dead_code)]
struct Nil;



#[allow(dead_code)]
struct Tuple(u8, bool);



#[allow(dead_code)]
struct Record {
  member: u8
}



impl Record {
  pub fn new(value: u8) -> Self {
    Record {
      member: value
    }
  }
}



#[allow(dead_code)]
#[allow(unused_variables)]
fn basic_record() {
  let rec = Record::new(0);
}
