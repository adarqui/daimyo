use std::collections::BTreeMap;



pub fn english_alphabet_probabilities() -> BTreeMap<char, f64> {
  let mut eap: BTreeMap<char, f64> = BTreeMap::new();
  let pmap = vec![
    ('a', 0.082), ('b', 0.015), ('c', 0.028), ('d', 0.043), ('e', 0.127), ('f', 0.022),
    ('g', 0.020), ('h', 0.061), ('i', 0.070), ('j', 0.002), ('k', 0.008), ('l', 0.040),
    ('m', 0.024), ('n', 0.067), ('o', 0.075), ('p', 0.019), ('q', 0.001), ('r', 0.060),
    ('s', 0.063), ('t', 0.091), ('u', 0.028), ('v', 0.010), ('w', 0.023), ('x', 0.001),
    ('y', 0.020), ('z', 0.001)];
  for (c, f) in pmap {
    eap.insert(c, f);
  }
  eap
}
