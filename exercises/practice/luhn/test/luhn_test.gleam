import gleeunit
import gleeunit/should
import luhn

pub fn main() {
  gleeunit.main()
}

pub fn single_digit_strings_can_not_be_valid_test() {
  luhn.valid("1")
  |> should.equal(False)
}

pub fn a_single_zero_is_invalid_test() {
  luhn.valid("0")
  |> should.equal(False)
}

pub fn a_simple_valid_sin_that_remains_valid_if_reversed_test() {
  luhn.valid("059")
  |> should.equal(True)
}

pub fn a_simple_valid_sin_that_becomes_invalid_if_reversed_test() {
  luhn.valid("59")
  |> should.equal(True)
}

pub fn a_valid_canadian_sin_test() {
  luhn.valid("055 444 285")
  |> should.equal(True)
}

pub fn invalid_canadian_sin_test() {
  luhn.valid("055 444 286")
  |> should.equal(False)
}

pub fn invalid_credit_card_test() {
  luhn.valid("8273 1232 7352 0569")
  |> should.equal(False)
}

pub fn invalid_long_number_with_an_even_remainder_test() {
  luhn.valid("1 2345 6789 1234 5678 9012")
  |> should.equal(False)
}

pub fn invalid_long_number_with_a_remainder_divisible_by_5_test() {
  luhn.valid("1 2345 6789 1234 5678 9013")
  |> should.equal(False)
}

pub fn valid_number_with_an_even_number_of_digits_test() {
  luhn.valid("095 245 88")
  |> should.equal(True)
}

pub fn valid_number_with_an_odd_number_of_spaces_test() {
  luhn.valid("234 567 891 234")
  |> should.equal(True)
}

pub fn valid_strings_with_a_non_digit_added_at_the_end_become_invalid_test() {
  luhn.valid("059a")
  |> should.equal(False)
}

pub fn valid_strings_with_punctuation_included_become_invalid_test() {
  luhn.valid("055-444-285")
  |> should.equal(False)
}

pub fn valid_strings_with_symbols_included_become_invalid_test() {
  luhn.valid("055# 444$ 285")
  |> should.equal(False)
}

pub fn single_zero_with_space_is_invalid_test() {
  luhn.valid(" 0")
  |> should.equal(False)
}

pub fn more_than_a_single_zero_is_valid_test() {
  luhn.valid("0000 0")
  |> should.equal(True)
}

pub fn input_digit_9_is_correctly_converted_to_output_digit_9_test() {
  luhn.valid("091")
  |> should.equal(True)
}

pub fn very_long_input_is_valid_test() {
  luhn.valid("9999999999 9999999999 9999999999 9999999999")
  |> should.equal(True)
}

pub fn valid_luhn_with_an_odd_number_of_digits_and_non_zero_first_digit_test() {
  luhn.valid("109")
  |> should.equal(True)
}

pub fn using_ascii_value_for_non_doubled_non_digit_isn_t_allowed_test() {
  luhn.valid("055b 444 285")
  |> should.equal(False)
}

pub fn using_ascii_value_for_doubled_non_digit_isn_t_allowed_test() {
  luhn.valid(":9")
  |> should.equal(False)
}

pub fn non_numeric_non_space_char_in_the_middle_with_a_sum_that_s_divisible_by_10_isn_t_allowed_test() {
  luhn.valid("59%59")
  |> should.equal(False)
}
