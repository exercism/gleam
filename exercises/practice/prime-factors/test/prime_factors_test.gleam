import exercism/should
import exercism/test_runner
import prime_factors

pub fn main() {
  test_runner.main()
}

pub fn no_factors_test() {
  prime_factors.factors(1)
  |> should.equal([])
}

pub fn prime_number_test() {
  prime_factors.factors(2)
  |> should.equal([2])
}

pub fn another_prime_number_test() {
  prime_factors.factors(3)
  |> should.equal([3])
}

pub fn square_of_a_prime_test() {
  prime_factors.factors(9)
  |> should.equal([3, 3])
}

pub fn product_of_first_prime_test() {
  prime_factors.factors(4)
  |> should.equal([2, 2])
}

pub fn cube_of_a_prime_test() {
  prime_factors.factors(8)
  |> should.equal([2, 2, 2])
}

pub fn product_of_second_prime_test() {
  prime_factors.factors(27)
  |> should.equal([3, 3, 3])
}

pub fn product_of_third_prime_test() {
  prime_factors.factors(625)
  |> should.equal([5, 5, 5, 5])
}

pub fn product_of_first_and_second_prime_test() {
  prime_factors.factors(6)
  |> should.equal([2, 3])
}

pub fn product_of_primes_and_non_primes_test() {
  prime_factors.factors(12)
  |> should.equal([2, 2, 3])
}

pub fn product_of_primes_test() {
  prime_factors.factors(901_255)
  |> should.equal([5, 17, 23, 461])
}

pub fn factors_include_a_large_prime_test() {
  prime_factors.factors(93_819_012_551)
  |> should.equal([11, 9539, 894_119])
}
