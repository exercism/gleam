import exercism/should
import exercism/test_runner
import resistor_color.{
  Black, Blue, Brown, Green, Grey, Orange, Red, Violet, White, Yellow,
}

pub fn main() {
  test_runner.main()
}

pub fn color_codes_black_test() {
  resistor_color.code(Black)
  |> should.equal(0)
}

pub fn color_codes_white_test() {
  resistor_color.code(White)
  |> should.equal(9)
}

pub fn color_codes_orange_test() {
  resistor_color.code(Orange)
  |> should.equal(3)
}

pub fn colors_test() {
  resistor_color.colors()
  |> should.equal([
    Black, Brown, Red, Orange, Yellow, Green, Blue, Violet,
    Grey, White,
  ])
}
