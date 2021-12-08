import resistor_color.{
  Black, Blue, Brown, Green, Grey, Orange, Red, Violet, White, Yellow,
}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn black_test() {
  resistor_color.code(Black)
  |> should.equal(0)
}

pub fn brown_test() {
  resistor_color.code(Brown)
  |> should.equal(1)
}

pub fn red_test() {
  resistor_color.code(Red)
  |> should.equal(2)
}

pub fn orange_test() {
  resistor_color.code(Orange)
  |> should.equal(3)
}

pub fn yellow_test() {
  resistor_color.code(Yellow)
  |> should.equal(4)
}

pub fn green_test() {
  resistor_color.code(Green)
  |> should.equal(5)
}

pub fn blue_test() {
  resistor_color.code(Blue)
  |> should.equal(6)
}

pub fn violet_test() {
  resistor_color.code(Violet)
  |> should.equal(7)
}

pub fn grey_test() {
  resistor_color.code(Grey)
  |> should.equal(8)
}

pub fn white_test() {
  resistor_color.code(White)
  |> should.equal(9)
}

pub fn colors_test() {
  let colors = [
    Black,
    Brown,
    Red,
    Orange,
    Yellow,
    Green,
    Blue,
    Violet,
    Grey,
    White,
  ]

  resistor_color.colors()
  |> should.equal(colors)
}
