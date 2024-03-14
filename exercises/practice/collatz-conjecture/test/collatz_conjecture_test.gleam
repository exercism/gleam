import collatz_conjecture.{NonPositiveNumber}
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn zero_steps_for_one_test() {
  collatz_conjecture.steps(1)
  |> should.equal(Ok(0))
}

pub fn divide_if_even_test() {
  collatz_conjecture.steps(16)
  |> should.equal(Ok(4))
}

pub fn even_and_odd_steps_test() {
  collatz_conjecture.steps(12)
  |> should.equal(Ok(9))
}

pub fn large_number_of_even_and_odd_steps_test() {
  collatz_conjecture.steps(1_000_000)
  |> should.equal(Ok(152))
}

pub fn zero_is_an_error_test() {
  collatz_conjecture.steps(0)
  |> should.equal(Error(NonPositiveNumber))
}

pub fn negative_value_is_an_error_test() {
  collatz_conjecture.steps(-15)
  |> should.equal(Error(NonPositiveNumber))
}
