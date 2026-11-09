import exercism/should
import exercism/test_runner
import resistor_color_trio.{Resistance, label}

pub fn main() {
  test_runner.main()
}

pub fn orange_and_orange_and_black_test() {
  label(["orange", "orange", "black"])
  |> should.equal(Ok(Resistance(unit: "ohms", value: 33)))
}

pub fn blue_and_grey_and_brown_test() {
  label(["blue", "grey", "brown"])
  |> should.equal(Ok(Resistance(unit: "ohms", value: 680)))
}

pub fn red_and_black_and_red_test() {
  label(["red", "black", "red"])
  |> should.equal(Ok(Resistance(unit: "kiloohms", value: 2)))
}

pub fn green_and_brown_and_orange_test() {
  label(["green", "brown", "orange"])
  |> should.equal(Ok(Resistance(unit: "kiloohms", value: 51)))
}

pub fn yellow_and_violet_and_yellow_test() {
  label(["yellow", "violet", "yellow"])
  |> should.equal(Ok(Resistance(unit: "kiloohms", value: 470)))
}

pub fn blue_and_violet_and_blue_test() {
  label(["blue", "violet", "blue"])
  |> should.equal(Ok(Resistance(unit: "megaohms", value: 67)))
}

pub fn minimum_possible_value_test() {
  label(["black", "black", "black"])
  |> should.equal(Ok(Resistance(unit: "ohms", value: 0)))
}

pub fn maximum_possible_value_test() {
  label(["white", "white", "white"])
  |> should.equal(Ok(Resistance(unit: "gigaohms", value: 99)))
}

pub fn first_two_colors_make_an_invalid_octal_number_test() {
  label(["black", "grey", "black"])
  |> should.equal(Ok(Resistance(unit: "ohms", value: 8)))
}

pub fn ignore_extra_colors_test() {
  label(["blue", "green", "yellow", "orange"])
  |> should.equal(Ok(Resistance(unit: "kiloohms", value: 650)))
}
