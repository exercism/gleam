import armstrong_numbers
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn zero_is_an_armstrong_number_test() {
  let assert True = armstrong_numbers.is_armstrong_number(0)
}

pub fn single_digit_numbers_are_armstrong_numbers_test() {
  let assert True = armstrong_numbers.is_armstrong_number(5)
}

pub fn there_are_no_two_digit_armstrong_numbers_test() {
  let assert False = armstrong_numbers.is_armstrong_number(10)
}

pub fn three_digit_number_that_is_an_armstrong_number_test() {
  let assert True = armstrong_numbers.is_armstrong_number(153)
}

pub fn three_digit_number_that_is_not_an_armstrong_number_test() {
  let assert False = armstrong_numbers.is_armstrong_number(100)
}

pub fn four_digit_number_that_is_an_armstrong_number_test() {
  let assert True = armstrong_numbers.is_armstrong_number(9474)
}

pub fn four_digit_number_that_is_not_an_armstrong_number_test() {
  let assert False = armstrong_numbers.is_armstrong_number(9475)
}

pub fn seven_digit_number_that_is_an_armstrong_number_test() {
  let assert True = armstrong_numbers.is_armstrong_number(9_926_315)
}

pub fn seven_digit_number_that_is_not_an_armstrong_number_test() {
  let assert False = armstrong_numbers.is_armstrong_number(9_926_314)
}

pub fn armstrong_number_containing_seven_zeroes_test() {
  let assert True =
    armstrong_numbers.is_armstrong_number(
      186_709_961_001_538_790_100_634_132_976_990,
    )
}

pub fn the_largest_and_last_armstrong_number_test() {
  let assert True =
    armstrong_numbers.is_armstrong_number(
      115_132_219_018_763_992_565_095_597_973_971_522_401,
    )
}
