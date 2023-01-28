import forth
import gleam/result
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
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
