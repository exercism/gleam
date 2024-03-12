import exercism/should
import exercism/test_runner
import grains.{InvalidSquare}

pub fn main() {
  test_runner.main()
}

pub fn returns_the_number_of_grains_on_the_square_grains_on_square_1_test() {
  grains.square(1)
  |> should.equal(Ok(1))
}

pub fn returns_the_number_of_grains_on_the_square_grains_on_square_2_test() {
  grains.square(2)
  |> should.equal(Ok(2))
}

pub fn returns_the_number_of_grains_on_the_square_grains_on_square_3_test() {
  grains.square(3)
  |> should.equal(Ok(4))
}

pub fn returns_the_number_of_grains_on_the_square_grains_on_square_4_test() {
  grains.square(4)
  |> should.equal(Ok(8))
}

pub fn returns_the_number_of_grains_on_the_square_grains_on_square_16_test() {
  grains.square(16)
  |> should.equal(Ok(32_768))
}

pub fn returns_the_number_of_grains_on_the_square_grains_on_square_32_test() {
  grains.square(32)
  |> should.equal(Ok(2_147_483_648))
}

pub fn returns_the_number_of_grains_on_the_square_grains_on_square_64_test() {
  grains.square(64)
  |> should.equal(Ok(9_223_372_036_854_775_808))
}

pub fn returns_the_number_of_grains_on_the_square_square_0_raises_an_exception_test() {
  grains.square(0)
  |> should.equal(Error(InvalidSquare))
}

pub fn returns_the_number_of_grains_on_the_square_negative_square_raises_an_exception_test() {
  grains.square(-1)
  |> should.equal(Error(InvalidSquare))
}

pub fn returns_the_number_of_grains_on_the_square_square_greater_than_64_raises_an_exception_test() {
  grains.square(65)
  |> should.equal(Error(InvalidSquare))
}

pub fn returns_the_total_number_of_grains_on_the_board_test() {
  grains.total()
  |> should.equal(18_446_744_073_709_551_615)
}
