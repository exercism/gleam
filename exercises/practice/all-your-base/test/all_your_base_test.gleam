import all_your_base.{InvalidBase, InvalidDigit}
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn single_bit_one_to_decimal_test() {
  all_your_base.rebase(digits: [1], input_base: 2, output_base: 10)
  |> should.equal(Ok([1]))
}

pub fn binary_to_single_decimal_test() {
  all_your_base.rebase(digits: [1, 0, 1], input_base: 2, output_base: 10)
  |> should.equal(Ok([5]))
}

pub fn single_decimal_to_binary_test() {
  all_your_base.rebase(digits: [5], input_base: 10, output_base: 2)
  |> should.equal(Ok([1, 0, 1]))
}

pub fn binary_to_multiple_decimal_test() {
  all_your_base.rebase(
    digits: [1, 0, 1, 0, 1, 0],
    input_base: 2,
    output_base: 10,
  )
  |> should.equal(Ok([4, 2]))
}

pub fn decimal_to_binary_test() {
  all_your_base.rebase(digits: [4, 2], input_base: 10, output_base: 2)
  |> should.equal(Ok([1, 0, 1, 0, 1, 0]))
}

pub fn trinary_to_hexadecimal_test() {
  all_your_base.rebase(digits: [1, 1, 2, 0], input_base: 3, output_base: 16)
  |> should.equal(Ok([2, 10]))
}

pub fn hexadecimal_to_trinary_test() {
  all_your_base.rebase(digits: [2, 10], input_base: 16, output_base: 3)
  |> should.equal(Ok([1, 1, 2, 0]))
}

pub fn fifteen_bit_integer_test() {
  all_your_base.rebase(digits: [3, 46, 60], input_base: 97, output_base: 73)
  |> should.equal(Ok([6, 10, 45]))
}

pub fn empty_list_test() {
  all_your_base.rebase(digits: [], input_base: 2, output_base: 10)
  |> should.equal(Ok([0]))
}

pub fn single_zero_test() {
  all_your_base.rebase(digits: [0], input_base: 10, output_base: 2)
  |> should.equal(Ok([0]))
}

pub fn multiple_zeros_test() {
  all_your_base.rebase(digits: [0, 0, 0], input_base: 10, output_base: 2)
  |> should.equal(Ok([0]))
}

pub fn leading_zeros_test() {
  all_your_base.rebase(digits: [0, 6, 0], input_base: 7, output_base: 10)
  |> should.equal(Ok([4, 2]))
}

pub fn input_base_is_one_test() {
  all_your_base.rebase(digits: [0], input_base: 1, output_base: 10)
  |> should.equal(Error(InvalidBase(1)))
}

pub fn input_base_is_zero_test() {
  all_your_base.rebase(digits: [], input_base: 0, output_base: 10)
  |> should.equal(Error(InvalidBase(0)))
}

pub fn input_base_is_negative_test() {
  all_your_base.rebase(digits: [1], input_base: -2, output_base: 10)
  |> should.equal(Error(InvalidBase(-2)))
}

pub fn negative_digit_test() {
  all_your_base.rebase(
    digits: [1, -1, 1, 0, 1, 0],
    input_base: 2,
    output_base: 10,
  )
  |> should.equal(Error(InvalidDigit(-1)))
}

pub fn invalid_positive_digit_test() {
  all_your_base.rebase(
    digits: [1, 2, 1, 0, 1, 0],
    input_base: 2,
    output_base: 10,
  )
  |> should.equal(Error(InvalidDigit(2)))
}

pub fn output_base_is_one_test() {
  all_your_base.rebase(
    digits: [1, 0, 1, 0, 1, 0],
    input_base: 2,
    output_base: 1,
  )
  |> should.equal(Error(InvalidBase(1)))
}

pub fn output_base_is_zero_test() {
  all_your_base.rebase(digits: [7], input_base: 10, output_base: 0)
  |> should.equal(Error(InvalidBase(0)))
}

pub fn output_base_is_negative_test() {
  all_your_base.rebase(digits: [1], input_base: 2, output_base: -7)
  |> should.equal(Error(InvalidBase(-7)))
}

pub fn both_bases_are_negative_test() {
  let output =
    all_your_base.rebase(digits: [1], input_base: -2, output_base: -7)
  let assert True =
    output == Error(InvalidBase(-2)) || output == Error(InvalidBase(-7))
}
