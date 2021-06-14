import resistor.{
  Black, Blue, Brown, Green, Grey, Orange, Red, Violet, White, Yellow,
}
import gleam/expect

pub fn black_test() {
  resistor.code(Black)
  |> should.equal(0)
}

pub fn brown_test() {
  resistor.code(Brown)
  |> should.equal(1)
}

pub fn red_test() {
  resistor.code(Red)
  |> should.equal(2)
}

pub fn orange_test() {
  resistor.code(Orange)
  |> should.equal(3)
}

pub fn yellow_test() {
  resistor.code(Yellow)
  |> should.equal(4)
}

pub fn green_test() {
  resistor.code(Green)
  |> should.equal(5)
}

pub fn blue_test() {
  resistor.code(Blue)
  |> should.equal(6)
}

pub fn violet_test() {
  resistor.code(Violet)
  |> should.equal(7)
}

pub fn grey_test() {
  resistor.code(Grey)
  |> should.equal(8)
}

pub fn white_test() {
  resistor.code(White)
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

  resistor.colors()
  |> should.equal(colors)
}
