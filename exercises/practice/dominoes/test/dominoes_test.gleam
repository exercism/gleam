import dominoes.{arrange}
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn empty_test() {
  []
  |> arrange
  |> list.length
  |> should.equal(0)
}

pub fn impossible_one_tile_test() {
  [#(2, 4)]
  |> arrange
  |> list.length
  |> should.equal(0)
}

pub fn another_impossible_one_tile_test() {
  [#(5, 3)]
  |> arrange
  |> list.length
  |> should.equal(0)
}

pub fn impossible_two_tile_test() {
  [#(4, 2), #(5, 3)]
  |> arrange
  |> list.length
  |> should.equal(0)
}

pub fn another_impossible_two_tile_test() {
  [#(3, 5), #(2, 4)]
  |> arrange
  |> list.length
  |> should.equal(0)
}

pub fn impossible_three_tile_test() {
  [#(5, 3), #(4, 2), #(2, 5)]
  |> arrange
  |> list.length
  |> should.equal(0)
}

pub fn another_impossible_three_tile_test() {
  [#(5, 3), #(4, 5), #(2, 3)]
  |> arrange
  |> list.length
  |> should.equal(0)
}

pub fn one_tile_test() {
  {
    [#(2, 2)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn two_tile_test() {
  {
    [#(2, 4), #(2, 4)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn another_two_tile_test() {
  {
    [#(5, 0), #(5, 0)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn three_tile_test() {
  {
    [#(2, 1), #(2, 3), #(1, 3)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn another_three_tile_test() {
  {
    [#(6, 4), #(6, 0), #(4, 0)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn four_tile_test() {
  {
    [#(2, 1), #(2, 3), #(1, 3), #(1, 1)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn another_four_tile_test() {
  {
    [#(6, 4), #(6, 2), #(2, 0), #(4, 0)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn five_tile_test() {
  {
    [#(2, 5), #(2, 1), #(1, 0), #(0, 5), #(5, 5)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}

pub fn another_file_tile_test() {
  {
    [#(4, 3), #(6, 2), #(4, 3), #(3, 6), #(2, 3)]
    |> arrange
    |> list.length > 0
  }
  |> should.be_true
}
