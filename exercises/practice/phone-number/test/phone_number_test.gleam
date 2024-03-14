import exercism/should
import exercism/test_runner
import phone_number

pub fn main() {
  test_runner.main()
}

pub fn cleans_the_number_test() {
  phone_number.clean("(223) 456-7890")
  |> should.equal(Ok("2234567890"))
}

pub fn cleans_numbers_with_dots_test() {
  phone_number.clean("223.456.7890")
  |> should.equal(Ok("2234567890"))
}

pub fn cleans_numbers_with_multiple_spaces_test() {
  phone_number.clean("223 456   7890   ")
  |> should.equal(Ok("2234567890"))
}

pub fn invalid_when_9_digits_test() {
  phone_number.clean("123456789")
  |> should.equal(Error("must not be fewer than 10 digits"))
}

pub fn invalid_when_11_digits_does_not_start_with_a_1_test() {
  phone_number.clean("22234567890")
  |> should.equal(Error("11 digits must start with 1"))
}

pub fn valid_when_11_digits_and_starting_with_1_test() {
  phone_number.clean("12234567890")
  |> should.equal(Ok("2234567890"))
}

pub fn valid_when_11_digits_and_starting_with_1_even_with_punctuation_test() {
  phone_number.clean("+1 (223) 456-7890")
  |> should.equal(Ok("2234567890"))
}

pub fn invalid_when_more_than_11_digits_test() {
  phone_number.clean("321234567890")
  |> should.equal(Error("must not be greater than 11 digits"))
}

pub fn invalid_with_letters_test() {
  phone_number.clean("523-abc-7890")
  |> should.equal(Error("letters not permitted"))
}

pub fn invalid_with_punctuations_test() {
  phone_number.clean("523-@:!-7890")
  |> should.equal(Error("punctuations not permitted"))
}

pub fn invalid_if_area_code_starts_with_0_test() {
  phone_number.clean("(023) 456-7890")
  |> should.equal(Error("area code cannot start with zero"))
}

pub fn invalid_if_area_code_starts_with_1_test() {
  phone_number.clean("(123) 456-7890")
  |> should.equal(Error("area code cannot start with one"))
}

pub fn invalid_if_exchange_code_starts_with_0_test() {
  phone_number.clean("(223) 056-7890")
  |> should.equal(Error("exchange code cannot start with zero"))
}

pub fn invalid_if_exchange_code_starts_with_1_test() {
  phone_number.clean("(223) 156-7890")
  |> should.equal(Error("exchange code cannot start with one"))
}

pub fn invalid_if_area_code_starts_with_0_on_valid_11_digit_number_test() {
  phone_number.clean("1 (023) 456-7890")
  |> should.equal(Error("area code cannot start with zero"))
}

pub fn invalid_if_area_code_starts_with_1_on_valid_11_digit_number_test() {
  phone_number.clean("1 (123) 456-7890")
  |> should.equal(Error("area code cannot start with one"))
}

pub fn invalid_if_exchange_code_starts_with_0_on_valid_11_digit_number_test() {
  phone_number.clean("1 (223) 056-7890")
  |> should.equal(Error("exchange code cannot start with zero"))
}

pub fn invalid_if_exchange_code_starts_with_1_on_valid_11_digit_number_test() {
  phone_number.clean("1 (223) 156-7890")
  |> should.equal(Error("exchange code cannot start with one"))
}
