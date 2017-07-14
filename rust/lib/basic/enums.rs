#[allow(dead_code)]
enum Color {
  Red,
  Green,
  Blue
}



#[allow(dead_code)]
enum SumType {
  Color,
  Colors(Vec<Color>),
  Coloring{color: Color, intensity: u64}
}
