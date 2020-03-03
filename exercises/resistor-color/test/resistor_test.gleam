import resistor.{Black, Brown, Red, Orange, Yellow, Green, Blue, Violet, Grey, White}
import gleam/expect

pub fn black_test() {
  resistor.code(Black)
  |> expect.equal(_, 0)
}

pub fn brown_test() {
  resistor.code(Brown)
  |> expect.equal(_, 1)
}

pub fn red_test() {
  resistor.code(Red)
  |> expect.equal(_, 2)
}

pub fn orange_test() {
  resistor.code(Orange)
  |> expect.equal(_, 3)
}

pub fn yellow_test() {
  resistor.code(Yellow)
  |> expect.equal(_, 4)
}

pub fn green_test() {
  resistor.code(Green)
  |> expect.equal(_, 5)
}

pub fn blue_test() {
  resistor.code(Blue)
  |> expect.equal(_, 6)
}

pub fn violet_test() {
  resistor.code(Violet)
  |> expect.equal(_, 7)
}

pub fn grey_test() {
  resistor.code(Grey)
  |> expect.equal(_, 8)
}

pub fn white_test() {
  resistor.code(White)
  |> expect.equal(_, 9)
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
    White
  ]

  resistor.colors()
  |> expect.equal(_, colors)
}
