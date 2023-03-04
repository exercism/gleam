import gleeunit
import gleam/float
import rational_numbers.{RationalNumber}

pub fn main() {
  gleeunit.main()
}

pub fn arithmetic_addition_add_two_positive_rational_numbers_test() {
  let assert RationalNumber(7, 6) =
    rational_numbers.add(RationalNumber(1, 2), RationalNumber(2, 3))
}

pub fn arithmetic_addition_add_a_positive_rational_number_and_a_negative_rational_number_test() {
  let assert RationalNumber(-1, 6) =
    rational_numbers.add(RationalNumber(1, 2), RationalNumber(-2, 3))
}

pub fn arithmetic_addition_add_two_negative_rational_numbers_test() {
  let assert RationalNumber(-7, 6) =
    rational_numbers.add(RationalNumber(-1, 2), RationalNumber(-2, 3))
}

pub fn arithmetic_addition_add_a_rational_number_to_its_additive_inverse_test() {
  let assert RationalNumber(0, 1) =
    rational_numbers.add(RationalNumber(1, 2), RationalNumber(-1, 2))
}

pub fn arithmetic_subtraction_subtract_two_positive_rational_numbers_test() {
  let assert RationalNumber(-1, 6) =
    rational_numbers.subtract(RationalNumber(1, 2), RationalNumber(2, 3))
}

pub fn arithmetic_subtraction_subtract_a_positive_rational_number_and_a_negative_rational_number_test() {
  let assert RationalNumber(7, 6) =
    rational_numbers.subtract(RationalNumber(1, 2), RationalNumber(-2, 3))
}

pub fn arithmetic_subtraction_subtract_two_negative_rational_numbers_test() {
  let assert RationalNumber(1, 6) =
    rational_numbers.subtract(RationalNumber(-1, 2), RationalNumber(-2, 3))
}

pub fn arithmetic_subtraction_subtract_a_rational_number_from_itself_test() {
  let assert RationalNumber(0, 1) =
    rational_numbers.subtract(RationalNumber(1, 2), RationalNumber(1, 2))
}

pub fn arithmetic_multiplication_multiply_two_positive_rational_numbers_test() {
  let assert RationalNumber(1, 3) =
    rational_numbers.multiply(RationalNumber(1, 2), RationalNumber(2, 3))
}

pub fn arithmetic_multiplication_multiply_a_negative_rational_number_by_a_positive_rational_number_test() {
  let assert RationalNumber(-1, 3) =
    rational_numbers.multiply(RationalNumber(-1, 2), RationalNumber(2, 3))
}

pub fn arithmetic_multiplication_multiply_two_negative_rational_numbers_test() {
  let assert RationalNumber(1, 3) =
    rational_numbers.multiply(RationalNumber(-1, 2), RationalNumber(-2, 3))
}

pub fn arithmetic_multiplication_multiply_a_rational_number_by_its_reciprocal_test() {
  let assert RationalNumber(1, 1) =
    rational_numbers.multiply(RationalNumber(1, 2), RationalNumber(2, 1))
}

pub fn arithmetic_multiplication_multiply_a_rational_number_by_1_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.multiply(RationalNumber(1, 2), RationalNumber(1, 1))
}

pub fn arithmetic_multiplication_multiply_a_rational_number_by_0_test() {
  let assert RationalNumber(0, 1) =
    rational_numbers.multiply(RationalNumber(1, 2), RationalNumber(0, 1))
}

pub fn arithmetic_division_divide_two_positive_rational_numbers_test() {
  let assert RationalNumber(3, 4) =
    rational_numbers.divide(RationalNumber(1, 2), RationalNumber(2, 3))
}

pub fn arithmetic_division_divide_a_positive_rational_number_by_a_negative_rational_number_test() {
  let assert RationalNumber(-3, 4) =
    rational_numbers.divide(RationalNumber(1, 2), RationalNumber(-2, 3))
}

pub fn arithmetic_division_divide_two_negative_rational_numbers_test() {
  let assert RationalNumber(3, 4) =
    rational_numbers.divide(RationalNumber(-1, 2), RationalNumber(-2, 3))
}

pub fn arithmetic_division_divide_a_rational_number_by_1_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.divide(RationalNumber(1, 2), RationalNumber(1, 1))
}

pub fn absolute_value_absolute_value_of_a_positive_rational_number_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.absolute_value(RationalNumber(1, 2))
}

pub fn absolute_value_absolute_value_of_a_positive_rational_number_with_negative_numerator_and_denominator_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.absolute_value(RationalNumber(-1, -2))
}

pub fn absolute_value_absolute_value_of_a_negative_rational_number_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.absolute_value(RationalNumber(-1, 2))
}

pub fn absolute_value_absolute_value_of_a_negative_rational_number_with_negative_denominator_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.absolute_value(RationalNumber(1, -2))
}

pub fn absolute_value_absolute_value_of_zero_test() {
  let assert RationalNumber(0, 1) =
    rational_numbers.absolute_value(RationalNumber(0, 1))
}

pub fn absolute_value_absolute_value_of_a_rational_number_is_reduced_to_lowest_terms_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.absolute_value(RationalNumber(2, 4))
}

pub fn exponentiation_of_a_rational_number_raise_a_positive_rational_number_to_a_positive_integer_power_test() {
  let assert RationalNumber(1, 8) =
    rational_numbers.power_of_rational(number: RationalNumber(1, 2), to: 3)
}

pub fn exponentiation_of_a_rational_number_raise_a_negative_rational_number_to_a_positive_integer_power_test() {
  let assert RationalNumber(-1, 8) =
    rational_numbers.power_of_rational(number: RationalNumber(-1, 2), to: 3)
}

pub fn exponentiation_of_a_rational_number_raise_a_positive_rational_number_to_a_negative_integer_power_test() {
  let assert RationalNumber(25, 9) =
    rational_numbers.power_of_rational(number: RationalNumber(3, 5), to: -2)
}

pub fn exponentiation_of_a_rational_number_raise_a_negative_rational_number_to_an_even_negative_integer_power_test() {
  let assert RationalNumber(25, 9) =
    rational_numbers.power_of_rational(number: RationalNumber(-3, 5), to: -2)
}

pub fn exponentiation_of_a_rational_number_raise_a_negative_rational_number_to_an_odd_negative_integer_power_test() {
  let assert RationalNumber(-125, 27) =
    rational_numbers.power_of_rational(number: RationalNumber(-3, 5), to: -3)
}

pub fn exponentiation_of_a_rational_number_raise_zero_to_an_integer_power_test() {
  let assert RationalNumber(0, 1) =
    rational_numbers.power_of_rational(number: RationalNumber(0, 1), to: 5)
}

pub fn exponentiation_of_a_rational_number_raise_one_to_an_integer_power_test() {
  let assert RationalNumber(1, 1) =
    rational_numbers.power_of_rational(number: RationalNumber(1, 1), to: 4)
}

pub fn exponentiation_of_a_rational_number_raise_a_positive_rational_number_to_the_power_of_zero_test() {
  let assert RationalNumber(1, 1) =
    rational_numbers.power_of_rational(number: RationalNumber(1, 2), to: 0)
}

pub fn exponentiation_of_a_rational_number_raise_a_negative_rational_number_to_the_power_of_zero_test() {
  let assert RationalNumber(1, 1) =
    rational_numbers.power_of_rational(number: RationalNumber(-1, 2), to: 0)
}

pub fn exponentiation_of_a_real_number_to_a_rational_number_raise_a_real_number_to_a_positive_rational_number_test() {
  let assert Ok(power) =
    rational_numbers.power_of_real(number: 8.0, to: RationalNumber(4, 3))

  let assert True = float.loosely_equals(power, with: 16.0, tolerating: 0.001)
}

pub fn exponentiation_of_a_real_number_to_a_rational_number_raise_a_real_number_to_a_negative_rational_number_test() {
  let assert Ok(power) =
    rational_numbers.power_of_real(number: 9.0, to: RationalNumber(-1, 2))

  let assert True =
    float.loosely_equals(power, with: 0.3333333333333333, tolerating: 0.001)
}

pub fn exponentiation_of_a_real_number_to_a_rational_number_raise_a_real_number_to_a_zero_rational_number_test() {
  let assert Ok(power) =
    rational_numbers.power_of_real(number: 2.0, to: RationalNumber(0, 1))

  let assert True = float.loosely_equals(power, with: 1.0, tolerating: 0.001)
}

pub fn reduction_to_lowest_terms_reduce_a_positive_rational_number_to_lowest_terms_test() {
  let assert RationalNumber(1, 2) =
    rational_numbers.reduce(RationalNumber(2, 4))
}

pub fn reduction_to_lowest_terms_reduce_places_the_minus_sign_on_the_numerator_test() {
  let assert RationalNumber(-3, 4) =
    rational_numbers.reduce(RationalNumber(3, -4))
}

pub fn reduction_to_lowest_terms_reduce_a_negative_rational_number_to_lowest_terms_test() {
  let assert RationalNumber(-2, 3) =
    rational_numbers.reduce(RationalNumber(-4, 6))
}

pub fn reduction_to_lowest_terms_reduce_a_rational_number_with_a_negative_denominator_to_lowest_terms_test() {
  let assert RationalNumber(-1, 3) =
    rational_numbers.reduce(RationalNumber(3, -9))
}

pub fn reduction_to_lowest_terms_reduce_zero_to_lowest_terms_test() {
  let assert RationalNumber(0, 1) =
    rational_numbers.reduce(RationalNumber(0, 6))
}

pub fn reduction_to_lowest_terms_reduce_an_integer_to_lowest_terms_test() {
  let assert RationalNumber(-2, 1) =
    rational_numbers.reduce(RationalNumber(-14, 7))
}

pub fn reduction_to_lowest_terms_reduce_one_to_lowest_terms_test() {
  let assert RationalNumber(1, 1) =
    rational_numbers.reduce(RationalNumber(13, 13))
}
