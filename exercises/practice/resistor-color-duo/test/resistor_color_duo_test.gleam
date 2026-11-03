import exercism/should
import exercism/test_runner
import resistor_color_duo

pub fn main() {
  test_runner.main()
}

pub fn brown_and_black_test() {
  resistor_color_duo.value(["brown", "black"])
  |> should.equal(10)
}

pub fn blue_and_grey_test() {
  resistor_color_duo.value(["blue", "grey"])
  |> should.equal(68)
}

pub fn yellow_and_violet_test() {
  resistor_color_duo.value(["yellow", "violet"])
  |> should.equal(47)
}

pub fn white_and_red_test() {
  resistor_color_duo.value(["white", "red"])
  |> should.equal(92)
}

pub fn orange_and_orange_test() {
  resistor_color_duo.value(["orange", "orange"])
  |> should.equal(33)
}

pub fn ignore_additional_colors_test() {
  resistor_color_duo.value(["green", "brown", "orange"])
  |> should.equal(51)
}

pub fn black_and_brown_one_digit_test() {
  resistor_color_duo.value(["black", "brown"])
  |> should.equal(1)
}
