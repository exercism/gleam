import exercism/should
import exercism/test_runner
import sum_of_multiples

pub fn main() {
  test_runner.main()
}

pub fn no_multiples_within_limit_test() {
  sum_of_multiples.sum(factors: [3, 5], limit: 1)
  |> should.equal(0)
}

pub fn one_factor_has_multiples_within_limit_test() {
  sum_of_multiples.sum(factors: [3, 5], limit: 4)
  |> should.equal(3)
}

pub fn more_than_one_multiple_within_limit_test() {
  sum_of_multiples.sum(factors: [3], limit: 7)
  |> should.equal(9)
}

pub fn more_than_one_factor_with_multiples_within_limit_test() {
  sum_of_multiples.sum(factors: [3, 5], limit: 10)
  |> should.equal(23)
}

pub fn each_multiple_is_only_counted_once_test() {
  sum_of_multiples.sum(factors: [3, 5], limit: 100)
  |> should.equal(2318)
}

pub fn a_much_larger_limit_test() {
  sum_of_multiples.sum(factors: [3, 5], limit: 1000)
  |> should.equal(233_168)
}

pub fn three_factors_test() {
  sum_of_multiples.sum(factors: [7, 13, 17], limit: 20)
  |> should.equal(51)
}

pub fn factors_not_relatively_prime_test() {
  sum_of_multiples.sum(factors: [4, 6], limit: 15)
  |> should.equal(30)
}

pub fn some_pairs_of_factors_relatively_prime_and_some_not_test() {
  sum_of_multiples.sum(factors: [5, 6, 8], limit: 150)
  |> should.equal(4419)
}

pub fn one_factor_is_a_multiple_of_another_test() {
  sum_of_multiples.sum(factors: [5, 25], limit: 51)
  |> should.equal(275)
}

pub fn much_larger_factors_test() {
  sum_of_multiples.sum(factors: [43, 47], limit: 10_000)
  |> should.equal(2_203_160)
}

pub fn all_numbers_are_multiples_of_1_test() {
  sum_of_multiples.sum(factors: [1], limit: 100)
  |> should.equal(4950)
}

pub fn no_factors_means_an_empty_sum_test() {
  sum_of_multiples.sum(factors: [], limit: 10_000)
  |> should.equal(0)
}

pub fn the_only_multiple_of_0_is_0_test() {
  sum_of_multiples.sum(factors: [0], limit: 1)
  |> should.equal(0)
}

pub fn the_factor_0_does_not_affect_the_sum_of_multiples_of_other_factors_test() {
  sum_of_multiples.sum(factors: [3, 0], limit: 4)
  |> should.equal(3)
}

pub fn solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3_test() {
  sum_of_multiples.sum(factors: [2, 3, 5, 7, 11], limit: 10_000)
  |> should.equal(39_614_537)
}
