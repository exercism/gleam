import gleeunit
import resistor_color_duo.{
  Black, Blue, Brown, Green, Grey, Orange, Red, Violet, White, Yellow,
}

pub fn main() {
  gleeunit.main()
}

pub fn brown_and_black_test() {
  let assert 10 = resistor_color_duo.value([Brown, Black])
}

pub fn blue_and_grey_test() {
  let assert 68 = resistor_color_duo.value([Blue, Grey])
}

pub fn yellow_and_violet_test() {
  let assert 47 = resistor_color_duo.value([Yellow, Violet])
}

pub fn white_and_red_test() {
  let assert 92 = resistor_color_duo.value([White, Red])
}

pub fn orange_and_orange_test() {
  let assert 33 = resistor_color_duo.value([Orange, Orange])
}

pub fn ignore_additional_colors_test() {
  let assert 51 = resistor_color_duo.value([Green, Brown, Orange])
}

pub fn black_and_brown_one_digit_test() {
  let assert 1 = resistor_color_duo.value([Black, Brown])
}
