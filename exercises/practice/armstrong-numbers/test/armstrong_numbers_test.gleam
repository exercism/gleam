import armstrong_numbers
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn zero_is_an_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(0)
  |> should.equal(True)
}

pub fn single_digit_numbers_are_armstrong_numbers_test() {
  armstrong_numbers.is_armstrong_number(5)
  |> should.equal(True)
}

pub fn there_are_no_two_digit_armstrong_numbers_test() {
  armstrong_numbers.is_armstrong_number(10)
  |> should.equal(False)
}

pub fn three_digit_number_that_is_an_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(153)
  |> should.equal(True)
}

pub fn three_digit_number_that_is_not_an_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(100)
  |> should.equal(False)
}

pub fn four_digit_number_that_is_an_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(9474)
  |> should.equal(True)
}

pub fn four_digit_number_that_is_not_an_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(9475)
  |> should.equal(False)
}

pub fn seven_digit_number_that_is_an_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(9_926_315)
  |> should.equal(True)
}

pub fn seven_digit_number_that_is_not_an_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(9_926_314)
  |> should.equal(False)
}

// The numeric size of this input number is 108 bits. Consider skipping this test if appropriate.
pub fn armstrong_number_containing_seven_zeroes_test() {
  armstrong_numbers.is_armstrong_number(
    186_709_961_001_538_790_100_634_132_976_990,
  )
  |> should.equal(True)
}

// The numeric size of this input number is 127 bits. Consider skipping this test if appropriate.
pub fn the_largest_and_last_armstrong_number_test() {
  armstrong_numbers.is_armstrong_number(
    115_132_219_018_763_992_565_095_597_973_971_522_401,
  )
  |> should.equal(True)
}
