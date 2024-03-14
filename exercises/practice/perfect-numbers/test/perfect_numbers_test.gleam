import exercism/should
import exercism/test_runner
import perfect_numbers.{Abundant, Deficient, NonPositiveInt, Perfect}

pub fn main() {
  test_runner.main()
}

pub fn perfect_numbers_smallest_perfect_number_is_classified_correctly_test() {
  perfect_numbers.classify(6)
  |> should.equal(Ok(Perfect))
}

pub fn perfect_numbers_medium_perfect_number_is_classified_correctly_test() {
  perfect_numbers.classify(28)
  |> should.equal(Ok(Perfect))
}

pub fn perfect_numbers_large_perfect_number_is_classified_correctly_test() {
  perfect_numbers.classify(33_550_336)
  |> should.equal(Ok(Perfect))
}

pub fn abundant_numbers_smallest_abundant_number_is_classified_correctly_test() {
  perfect_numbers.classify(12)
  |> should.equal(Ok(Abundant))
}

pub fn abundant_numbers_medium_abundant_number_is_classified_correctly_test() {
  perfect_numbers.classify(30)
  |> should.equal(Ok(Abundant))
}

pub fn abundant_numbers_large_abundant_number_is_classified_correctly_test() {
  perfect_numbers.classify(33_550_335)
  |> should.equal(Ok(Abundant))
}

pub fn deficient_numbers_smallest_prime_deficient_number_is_classified_correctly_test() {
  perfect_numbers.classify(2)
  |> should.equal(Ok(Deficient))
}

pub fn deficient_numbers_smallest_non_prime_deficient_number_is_classified_correctly_test() {
  perfect_numbers.classify(4)
  |> should.equal(Ok(Deficient))
}

pub fn deficient_numbers_medium_deficient_number_is_classified_correctly_test() {
  perfect_numbers.classify(32)
  |> should.equal(Ok(Deficient))
}

pub fn deficient_numbers_large_deficient_number_is_classified_correctly_test() {
  perfect_numbers.classify(33_550_337)
  |> should.equal(Ok(Deficient))
}

pub fn deficient_numbers_edge_case_no_factors_other_than_itself_is_classified_correctly_test() {
  perfect_numbers.classify(1)
  |> should.equal(Ok(Deficient))
}

pub fn invalid_inputs_zero_is_rejected_as_it_is_not_a_positive_integer_test() {
  perfect_numbers.classify(0)
  |> should.equal(Error(NonPositiveInt))
}

pub fn invalid_inputs_negative_integer_is_rejected_as_it_is_not_a_positive_integer_test() {
  perfect_numbers.classify(-1)
  |> should.equal(Error(NonPositiveInt))
}
