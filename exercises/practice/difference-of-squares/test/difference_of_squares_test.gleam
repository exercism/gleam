import difference_of_squares
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn square_of_sum_1_test() {
  difference_of_squares.square_of_sum(1)
  |> should.equal(1)
}

pub fn square_of_sum_5_test() {
  difference_of_squares.square_of_sum(5)
  |> should.equal(225)
}

pub fn square_of_sum_100_test() {
  difference_of_squares.square_of_sum(100)
  |> should.equal(25_502_500)
}

pub fn sum_of_squares_1_test() {
  difference_of_squares.sum_of_squares(1)
  |> should.equal(1)
}

pub fn sum_of_squares_5_test() {
  difference_of_squares.sum_of_squares(5)
  |> should.equal(55)
}

pub fn sum_of_squares_100_test() {
  difference_of_squares.sum_of_squares(100)
  |> should.equal(338_350)
}

pub fn difference_of_squares_1_test() {
  difference_of_squares.difference(1)
  |> should.equal(0)
}

pub fn difference_of_squares_5_test() {
  difference_of_squares.difference(5)
  |> should.equal(170)
}

pub fn difference_of_squares_100_test() {
  difference_of_squares.difference(100)
  |> should.equal(25_164_150)
}
