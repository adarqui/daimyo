#[cfg(test)]

  extern crate modulo;
  use modulo::mod_num::*;

  #[test]
  fn test_mod_num_new() {
    assert_eq!(ModNum::new(101, 7), ModNum::new(101, 7));
    assert_ne!(ModNum::new(101, 7), ModNum::new(101, 8));
  }

  #[test]
  fn test_mod_num_congruent() {
    assert_eq!(ModNum::new(101, 7).congruent(ModNum::new(101, 7)), true);
    assert_eq!(ModNum::new(5, 10).congruent(ModNum::new(15, 10)), true);
    assert_ne!(ModNum::new(101, 7).congruent(ModNum::new(108, 7)), false);
  }

  #[test]
  fn test_mod_num_mul() {
    assert_eq!(ModNum::new(11, 16) * ModNum::new(13, 16), ModNum::new(15, 16));
    assert_eq!(ModNum::new(5, 10) * ModNum::new(10, 10), ModNum::new(0, 10));
  }

  #[test]
  fn test_mod_num_add() {
    assert_eq!(ModNum::new(11, 16) + ModNum::new(13, 16), ModNum::new(8, 16));
    assert_eq!(ModNum::new(5, 10) + ModNum::new(10, 10), ModNum::new(5, 10));
  }

  #[test]
  fn test_mod_num_subtraction() {
    /*
     * since additive inverses exist in Zm, we can also subtract elements in Zm.
     * we define a - b in Zm to be (a - b)mod m.
     */
     let a = ModNum::new(11, 31);
     let b = ModNum::new(18, 31);
     assert_eq!(a-b, ModNum::new(24, 31))
  }

  #[test]
  fn test_modulo() {
    assert_eq!(101.modulo(7), 3);
    assert_eq!((-101).modulo(7), 4);
  }

  #[test]
  fn test_a_is_congruent_to_b_modulo_m() {
    assert_eq!(a_is_congruent_to_b_modulo_m(101, -101, 7), false);
    assert_eq!(a_is_congruent_to_b_modulo_m(101, 101, 7), true)
  }

  #[test]
  fn test_a_is_congruent_to_b_modulo_m_() {
    assert_eq!(a_is_congruent_to_b_modulo_m_(101, -101, 7), false);
    assert_eq!(a_is_congruent_to_b_modulo_m_(101, 101, 7), true)
  }
