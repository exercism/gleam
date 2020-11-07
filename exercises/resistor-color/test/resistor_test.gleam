import resistor.{Black, Brown, Red, Orange, Yellow, Green, Blue, Violet, Grey, White}
import gleam/expect

pub fn black_test() {
  resistor.code(Black)
  |> should.equal(_, 0)
}

pub fn brown_test() {
  resistor.code(Brown)
  |> should.equal(_, 1)
}

pub fn red_test() {
  resistor.code(Red)
  |> should.equal(_, 2)
}

pub fn orange_test() {
  resistor.code(Orange)
  |> should.equal(_, 3)
}

pub fn yellow_test() {
  resistor.code(Yellow)
  |> should.equal(_, 4)
}

pub fn green_test() {
  resistor.code(Green)
  |> should.equal(_, 5)
}

pub fn blue_test() {
  resistor.code(Blue)
  |> should.equal(_, 6)
}

pub fn violet_test() {
  resistor.code(Violet)
  |> should.equal(_, 7)
}

pub fn grey_test() {
  resistor.code(Grey)
  |> should.equal(_, 8)
}

pub fn white_test() {
  resistor.code(White)
  |> should.equal(_, 9)
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
  |> should.equal(_, colors)
}
