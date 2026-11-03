import exercism/should
import exercism/test_runner
import forth
import gleam/result

// The cases are split into multiple sections, all with the same structure.
// In all cases, the `expected` key is the resulting stack
// after executing the Forth program contained in the `input` key, unless an 'error' exists.

pub fn main() {
  test_runner.main()
}

fn error_with(o: Result(a, b), err: b) {
  should.equal(o, Error(err))
}

fn succeed_with(o: Result(a, b), res: a) {
  should.equal(o, Ok(res))
}

fn run_forth_for(prog: String, expected: String) {
  forth.new()
  |> forth.eval(prog)
  |> result.map(forth.format_stack)
  |> succeed_with(expected)
}

fn run_forth_and_error(prog: String, expected: forth.ForthError) {
  forth.new()
  |> forth.eval(prog)
  |> error_with(expected)
}

pub fn parsing_and_numbers_numbers_just_get_pushed_onto_the_stack_test() {
  run_forth_for("1 2 3 4 5", "1 2 3 4 5")
}

pub fn parsing_and_numbers_pushes_negative_numbers_onto_the_stack_test() {
  run_forth_for("-1 -2 -3 -4 -5", "-1 -2 -3 -4 -5")
}

pub fn addition_can_add_two_numbers_test() {
  run_forth_for("1 2 +", "3")
}

pub fn addition_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("+", forth.StackUnderflow)
}

pub fn addition_errors_if_there_is_only_one_value_on_the_stack_test() {
  run_forth_and_error("1 +", forth.StackUnderflow)
}

pub fn addition_more_than_two_values_on_the_stack_test() {
  run_forth_for("1 2 3 +", "1 5")
}

pub fn subtraction_can_subtract_two_numbers_test() {
  run_forth_for("3 4 -", "-1")
}

pub fn subtraction_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("-", forth.StackUnderflow)
}

pub fn subtraction_errors_if_there_is_only_one_value_on_the_stack_test() {
  run_forth_and_error("1 -", forth.StackUnderflow)
}

pub fn subtraction_more_than_two_values_on_the_stack_test() {
  run_forth_for("1 12 3 -", "1 9")
}

pub fn multiplication_can_multiply_two_numbers_test() {
  run_forth_for("2 4 *", "8")
}

pub fn multiplication_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("*", forth.StackUnderflow)
}

pub fn multiplication_errors_if_there_is_only_one_value_on_the_stack_test() {
  run_forth_and_error("1 *", forth.StackUnderflow)
}

pub fn multiplication_more_than_two_values_on_the_stack_test() {
  run_forth_for("1 2 3 *", "1 6")
}

pub fn division_can_divide_two_numbers_test() {
  run_forth_for("12 3 /", "4")
}

pub fn division_performs_integer_division_test() {
  run_forth_for("8 3 /", "2")
}

pub fn division_errors_if_dividing_by_zero_test() {
  run_forth_and_error("4 0 /", forth.DivisionByZero)
}

pub fn division_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("/", forth.StackUnderflow)
}

pub fn division_errors_if_there_is_only_one_value_on_the_stack_test() {
  run_forth_and_error("1 /", forth.StackUnderflow)
}

pub fn division_more_than_two_values_on_the_stack_test() {
  run_forth_for("1 12 3 /", "1 4")
}

pub fn combined_arithmetic_addition_and_subtraction_test() {
  run_forth_for("1 2 + 4 -", "-1")
}

pub fn combined_arithmetic_multiplication_and_division_test() {
  run_forth_for("2 4 * 3 /", "2")
}

pub fn combined_arithmetic_multiplication_and_addition_test() {
  run_forth_for("1 3 4 * +", "13")
}

pub fn combined_arithmetic_addition_and_multiplication_test() {
  run_forth_for("1 3 4 + *", "7")
}

pub fn dup_copies_a_value_on_the_stack_test() {
  run_forth_for("1 dup", "1 1")
}

pub fn dup_copies_the_top_value_on_the_stack_test() {
  run_forth_for("1 2 dup", "1 2 2")
}

pub fn dup_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("dup", forth.StackUnderflow)
}

pub fn drop_removes_the_top_value_on_the_stack_if_it_is_the_only_one_test() {
  run_forth_for("1 drop", "")
}

pub fn drop_removes_the_top_value_on_the_stack_if_it_is_not_the_only_one_test() {
  run_forth_for("1 2 drop", "1")
}

pub fn drop_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("drop", forth.StackUnderflow)
}

pub fn swap_swaps_the_top_two_values_on_the_stack_if_they_are_the_only_ones_test() {
  run_forth_for("1 2 swap", "2 1")
}

pub fn swap_swaps_the_top_two_values_on_the_stack_if_they_are_not_the_only_ones_test() {
  run_forth_for("1 2 3 swap", "1 3 2")
}

pub fn swap_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("swap", forth.StackUnderflow)
}

pub fn swap_errors_if_there_is_only_one_value_on_the_stack_test() {
  run_forth_and_error("1 swap", forth.StackUnderflow)
}

pub fn over_copies_the_second_element_if_there_are_only_two_test() {
  run_forth_for("1 2 over", "1 2 1")
}

pub fn over_copies_the_second_element_if_there_are_more_than_two_test() {
  run_forth_for("1 2 3 over", "1 2 3 2")
}

pub fn over_errors_if_there_is_nothing_on_the_stack_test() {
  run_forth_and_error("over", forth.StackUnderflow)
}

pub fn over_errors_if_there_is_only_one_value_on_the_stack_test() {
  run_forth_and_error("1 over", forth.StackUnderflow)
}

pub fn user_defined_words_can_consist_of_built_in_words_test() {
  forth.new()
  |> forth.eval(": dup-twice dup dup ;")
  |> result.try(forth.eval(_, "1 dup-twice"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1")
}

pub fn user_defined_words_execute_in_the_right_order_test() {
  forth.new()
  |> forth.eval(": countup 1 2 3 ;")
  |> result.try(forth.eval(_, "countup"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 2 3")
}

pub fn user_defined_words_can_override_other_user_defined_words_test() {
  forth.new()
  |> forth.eval(": foo dup ;")
  |> result.try(forth.eval(_, ": foo dup dup ;"))
  |> result.try(forth.eval(_, "1 foo"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1")
}

pub fn user_defined_words_can_override_built_in_words_test() {
  forth.new()
  |> forth.eval(": swap dup ;")
  |> result.try(forth.eval(_, "1 swap"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1")
}

pub fn user_defined_words_can_override_built_in_operators_test() {
  forth.new()
  |> forth.eval(": + * ;")
  |> result.try(forth.eval(_, "3 4 +"))
  |> result.map(forth.format_stack)
  |> succeed_with("12")
}

pub fn user_defined_words_can_use_different_words_with_the_same_name_test() {
  forth.new()
  |> forth.eval(": foo 5 ;")
  |> result.try(forth.eval(_, ": bar foo ;"))
  |> result.try(forth.eval(_, ": foo 6 ;"))
  |> result.try(forth.eval(_, "bar foo"))
  |> result.map(forth.format_stack)
  |> succeed_with("5 6")
}

pub fn user_defined_words_cannot_redefine_non_negative_numbers_test() {
  run_forth_and_error(": 1 2 ;", forth.InvalidWord)
}

pub fn user_defined_words_cannot_redefine_negative_numbers_test() {
  run_forth_and_error(": -1 2 ;", forth.InvalidWord)
}

pub fn user_defined_words_errors_if_executing_a_non_existent_word_test() {
  run_forth_and_error("foo", forth.UnknownWord)
}

pub fn case_insensitivity_dup_is_case_insensitive_test() {
  run_forth_for("1 DUP Dup dup", "1 1 1 1")
}

pub fn case_insensitivity_drop_is_case_insensitive_test() {
  run_forth_for("1 2 3 4 DROP Drop drop", "1")
}

pub fn case_insensitivity_swap_is_case_insensitive_test() {
  run_forth_for("1 2 SWAP 3 Swap 4 swap", "2 3 4 1")
}

pub fn case_insensitivity_over_is_case_insensitive_test() {
  run_forth_for("1 2 OVER Over over", "1 2 1 2 1")
}

pub fn case_insensitivity_user_defined_words_are_case_insensitive_test() {
  forth.new()
  |> forth.eval(": foo dup ;")
  |> result.try(forth.eval(_, "1 FOO Foo foo"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1 1")
}

pub fn case_insensitivity_definitions_are_case_insensitive_test() {
  forth.new()
  |> forth.eval(": SWAP DUP Dup dup ;")
  |> result.try(forth.eval(_, "1 swap"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1 1")
}
