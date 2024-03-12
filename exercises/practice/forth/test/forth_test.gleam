import exercism/should
import exercism/test_runner
import forth
import gleam/result

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

pub fn no_input_no_stack_test() {
  forth.new()
  |> forth.format_stack
  |> should.equal("")
}

pub fn numbers_just_get_pushed_onto_the_stack_test() {
  run_forth_for("1 2 3 4 5", "1 2 3 4 5")
}

pub fn pushes_negative_numbers_onto_the_stack_test() {
  run_forth_for("-1 -2 -3 -4 -5", "-1 -2 -3 -4 -5")
}

pub fn can_add_two_numbers_test() {
  run_forth_for("1 2 +", "3")
}

pub fn errors_if_there_is_nothing_on_the_stack_test() {
  forth.new()
  |> forth.eval("+")
  |> error_with(forth.StackUnderflow)
}

pub fn errors_if_there_is_only_one_value_on_the_stack_test() {
  forth.new()
  |> forth.eval("1 +")
  |> error_with(forth.StackUnderflow)
}

pub fn can_subtract_two_numbers_test() {
  run_forth_for("3 4 -", "-1")
}

pub fn subtraction_errors_if_there_is_nothing_on_the_stack_test() {
  forth.new()
  |> forth.eval("-")
  |> error_with(forth.StackUnderflow)
}

pub fn subtraction_errors_if_there_is_only_one_value_on_the_stack_test() {
  forth.new()
  |> forth.eval("1 -")
  |> error_with(forth.StackUnderflow)
}

pub fn can_multiply_two_numbers_test() {
  run_forth_for("2 4 *", "8")
}

pub fn multiplication_errors_if_there_is_nothing_on_the_stack_test() {
  forth.new()
  |> forth.eval("*")
  |> error_with(forth.StackUnderflow)
}

pub fn multiplication_errors_if_there_is_only_one_value_on_the_stack_test() {
  forth.new()
  |> forth.eval("1 *")
  |> error_with(forth.StackUnderflow)
}

pub fn can_divide_two_numbers_test() {
  run_forth_for("12 3 /", "4")
}

pub fn division_performs_integer_division_test() {
  run_forth_for("8 3 /", "2")
}

pub fn division_errors_if_there_is_nothing_on_the_stack_test() {
  forth.new()
  |> forth.eval("/")
  |> error_with(forth.StackUnderflow)
}

pub fn division_errors_if_there_is_only_one_value_on_the_stack_test() {
  forth.new()
  |> forth.eval("1 /")
  |> error_with(forth.StackUnderflow)
}

pub fn basic_arithmetic_test() {
  run_forth_for("1 2 + 4 -", "-1")
}

pub fn integer_division_test() {
  run_forth_for("2 4 * 3 /", "2")
}

pub fn division_by_zero_test() {
  forth.new()
  |> forth.eval("4 0 /")
  |> error_with(forth.DivisionByZero)
}

pub fn stack_dup1_test() {
  run_forth_for("1 DUP", "1 1")
}

pub fn stack_dup2_test() {
  run_forth_for("1 2 DUP", "1 2 2")
}

pub fn stack_dup_fail_test() {
  forth.new()
  |> forth.eval("DUP")
  |> error_with(forth.StackUnderflow)
}

pub fn stack_drop1_test() {
  run_forth_for("1 drop", "")
}

pub fn stack_drop2_test() {
  run_forth_for("1 2 drop", "1")
}

pub fn stack_drop_fail_test() {
  forth.new()
  |> forth.eval("drop")
  |> error_with(forth.StackUnderflow)
}

pub fn stack_swap1_test() {
  run_forth_for("1 2 swap", "2 1")
}

pub fn stack_swap2_test() {
  run_forth_for("1 2 3 swap", "1 3 2")
}

pub fn stack_swap_fail1_test() {
  forth.new()
  |> forth.eval("swap")
  |> error_with(forth.StackUnderflow)
}

pub fn stack_swap_fail2_test() {
  forth.new()
  |> forth.eval("1 swap")
  |> error_with(forth.StackUnderflow)
}

pub fn stack_over_1_test() {
  run_forth_for("1 2 over", "1 2 1")
}

pub fn stack_over_2_test() {
  run_forth_for("1 2 3 over", "1 2 3 2")
}

pub fn stack_over_fail_1_test() {
  forth.new()
  |> forth.eval("1 over")
  |> error_with(forth.StackUnderflow)
}

pub fn stack_over_fail_2_test() {
  forth.new()
  |> forth.eval("over")
  |> error_with(forth.StackUnderflow)
}

pub fn define_new_word_test() {
  forth.new()
  |> forth.eval(": dup-twice dup dup ;")
  |> result.then(forth.eval(_, "1 dup-twice"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1")
}

pub fn redefine_existing_word_test() {
  forth.new()
  |> forth.eval(": foo dup ;")
  |> result.then(forth.eval(_, ": foo dup dup ;"))
  |> result.then(forth.eval(_, "1 foo"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1")
}

pub fn redefining_an_existing_builtin_word_test() {
  forth.new()
  |> forth.eval(": swap dup ;")
  |> result.then(forth.eval(_, "1 swap"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1")
}

pub fn defining_words_with_odd_characters_test() {
  forth.new()
  |> forth.eval(": € 220371 ;")
  |> result.then(forth.eval(_, "€"))
  |> result.map(forth.format_stack)
  |> succeed_with("220371")
}

pub fn defining_a_number_test() {
  forth.new()
  |> forth.eval(": 1 2 ;")
  |> error_with(forth.InvalidWord)
}

pub fn calling_a_nonexistent_word_test() {
  forth.new()
  |> forth.eval("1 foo")
  |> error_with(forth.UnknownWord)
}

pub fn user_defined_words_execute_in_the_right_order_test() {
  forth.new()
  |> forth.eval(": countup 1 2 3 ;")
  |> result.then(forth.eval(_, "countup"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 2 3")
}

pub fn user_defined_words_can_override_builtin_operators_test() {
  forth.new()
  |> forth.eval(": + * ;")
  |> result.then(forth.eval(_, "3 4 +"))
  |> result.map(forth.format_stack)
  |> succeed_with("12")
}

pub fn user_defined_words_can_use_different_words_with_the_same_name_test() {
  forth.new()
  |> forth.eval(": foo 5 ;")
  |> result.then(forth.eval(_, ": bar foo ;"))
  |> result.then(forth.eval(_, ": foo 6 ;"))
  |> result.then(forth.eval(_, "bar foo"))
  |> result.map(forth.format_stack)
  |> succeed_with("5 6")
}

pub fn user_defined_words_can_define_word_that_uses_word_with_the_same_name_test() {
  forth.new()
  |> forth.eval(": foo 10 ;")
  |> result.then(forth.eval(_, ": foo foo 1 + ;"))
  |> result.then(forth.eval(_, "foo"))
  |> result.map(forth.format_stack)
  |> succeed_with("11")
}

pub fn user_defined_words_cannot_redefine_non_negative_numbers_test() {
  forth.new()
  |> forth.eval(": 1 2 ;")
  |> error_with(forth.InvalidWord)
}

pub fn user_defined_words_cannot_redefine_negative_numbers_test() {
  forth.new()
  |> forth.eval(": -1 2 ;")
  |> error_with(forth.InvalidWord)
}

pub fn user_defined_words_errors_if_executing_non_existent_word_test() {
  forth.new()
  |> forth.eval("foo")
  |> error_with(forth.UnknownWord)
}

pub fn dup_is_case_insensitive_test() {
  run_forth_for("1 DUP Dup dup", "1 1 1 1")
}

pub fn drop_is_case_insensitive_test() {
  run_forth_for("1 2 3 4 DROP Drop drop", "1")
}

pub fn swap_is_case_insensitive_test() {
  run_forth_for("1 2 SWAP 3 Swap 4 swap", "2 3 4 1")
}

pub fn over_is_case_insensitive_test() {
  run_forth_for("1 2 OVER Over over", "1 2 1 2 1")
}

pub fn user_defined_words_are_case_insensitive_test() {
  forth.new()
  |> forth.eval(": foo dup ;")
  |> result.then(forth.eval(_, "1 FOO Foo foo"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1 1")
}

pub fn definitions_are_case_insensitive_test() {
  forth.new()
  |> forth.eval(": SWAP DUP Dup dup ;")
  |> result.then(forth.eval(_, "1 swap"))
  |> result.map(forth.format_stack)
  |> succeed_with("1 1 1 1")
}
