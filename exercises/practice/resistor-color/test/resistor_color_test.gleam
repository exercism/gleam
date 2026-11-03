import exercism/should
import exercism/test_runner
import resistor_color

pub fn main() {
  test_runner.main()
}

pub fn color_codes_black_test() {
  resistor_color.color_code("black")
  |> should.equal(0)
}

pub fn color_codes_white_test() {
  resistor_color.color_code("white")
  |> should.equal(9)
}

pub fn color_codes_orange_test() {
  resistor_color.color_code("orange")
  |> should.equal(3)
}

pub fn colors_test() {
  resistor_color.colors()
  |> should.equal([
    "black", "brown", "red", "orange", "yellow", "green", "blue", "violet",
    "grey", "white",
  ])
}
