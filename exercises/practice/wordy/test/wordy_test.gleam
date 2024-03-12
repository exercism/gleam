import exercism/should
import exercism/test_runner
import wordy.{ImpossibleOperation, SyntaxError, UnknownOperation}

pub fn main() {
  test_runner.main()
}

pub fn just_a_number_test() {
  wordy.answer("What is 5?")
  |> should.equal(Ok(5))
}

pub fn addition_test() {
  wordy.answer("What is 1 plus 1?")
  |> should.equal(Ok(2))
}

pub fn more_addition_test() {
  wordy.answer("What is 53 plus 2?")
  |> should.equal(Ok(55))
}

pub fn addition_with_negative_numbers_test() {
  wordy.answer("What is -1 plus -10?")
  |> should.equal(Ok(-11))
}

pub fn large_addition_test() {
  wordy.answer("What is 123 plus 45678?")
  |> should.equal(Ok(45_801))
}

pub fn subtraction_test() {
  wordy.answer("What is 4 minus -12?")
  |> should.equal(Ok(16))
}

pub fn multiplication_test() {
  wordy.answer("What is -3 multiplied by 25?")
  |> should.equal(Ok(-75))
}

pub fn division_test() {
  wordy.answer("What is 33 divided by -3?")
  |> should.equal(Ok(-11))
}

pub fn multiple_additions_test() {
  wordy.answer("What is 1 plus 1 plus 1?")
  |> should.equal(Ok(3))
}

pub fn addition_and_subtraction_test() {
  wordy.answer("What is 1 plus 5 minus -2?")
  |> should.equal(Ok(8))
}

pub fn multiple_subtraction_test() {
  wordy.answer("What is 20 minus 4 minus 13?")
  |> should.equal(Ok(3))
}

pub fn subtraction_then_addition_test() {
  wordy.answer("What is 17 minus 6 plus 3?")
  |> should.equal(Ok(14))
}

pub fn multiple_multiplication_test() {
  wordy.answer("What is 2 multiplied by -2 multiplied by 3?")
  |> should.equal(Ok(-12))
}

pub fn addition_and_multiplication_test() {
  wordy.answer("What is -3 plus 7 multiplied by -2?")
  |> should.equal(Ok(-8))
}

pub fn multiple_division_test() {
  wordy.answer("What is -12 divided by 2 divided by -3?")
  |> should.equal(Ok(2))
}

pub fn unknown_operation_test() {
  wordy.answer("What is 52 cubed?")
  |> should.equal(Error(UnknownOperation))
}

pub fn non_math_question_test() {
  wordy.answer("Who is the President of the United States?")
  |> should.equal(Error(UnknownOperation))
}

pub fn reject_problem_missing_an_operand_test() {
  wordy.answer("What is 1 plus?")
  |> should.equal(Error(SyntaxError))
}

pub fn reject_problem_with_no_operands_or_operators_test() {
  wordy.answer("What is?")
  |> should.equal(Error(SyntaxError))
}

pub fn reject_two_operations_in_a_row_test() {
  wordy.answer("What is 1 plus plus 2?")
  |> should.equal(Error(SyntaxError))
}

pub fn reject_two_numbers_in_a_row_test() {
  wordy.answer("What is 1 plus 2 1?")
  |> should.equal(Error(SyntaxError))
}

pub fn reject_postfix_notation_test() {
  wordy.answer("What is 1 2 plus?")
  |> should.equal(Error(SyntaxError))
}

pub fn reject_prefix_notation_test() {
  wordy.answer("What is plus 1 2?")
  |> should.equal(Error(SyntaxError))
}

pub fn reject_division_by_zero_test() {
  wordy.answer("What is 1 divided by 0?")
  |> should.equal(Error(ImpossibleOperation))
}
