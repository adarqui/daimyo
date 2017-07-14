// https://doc.rust-lang.org/book/first-edition/enums.html



enum Color {
  Red,
  Green,
  Blue
}



enum SumType {
  Color,
  Colors(Vec<Color>),
  Coloring{color: Color, intensity: f64}
}



enum ScopedNoProblem {
  Color
}



fn basic_enums_sumtype() {
  let coloring = SumType::Coloring{color: Color::Red, intensity: 77.6};
}
