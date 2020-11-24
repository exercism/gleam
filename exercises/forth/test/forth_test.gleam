import forth
import gleam/should
import gleam/result
import gleam/list
import gleam/io

fn error_with(o: result.Result(a, b), err: b) -> should.Expectation {
  should.equal(Error(err), o)
}

fn succeed_with(o: result.Result(a, b), res: a) -> should.Expectation {
  should.equal(Ok(res), o)
}

fn pending() -> should.Expectation {
  should.be_true(True)
}

fn run_forth_for(prog: String, expected: String) -> should.Expectation {
  forth.new()
  |> forth.eval(prog)
  |> result.map(forth.format_stack)
  |> succeed_with(expected)
}

pub fn no_input_no_stack_test() {
  pending() // forth.new() |> forth.format_stack |> should.equal("")
}

pub fn numbers_just_get_pushed_onto_the_stack_test() {
  pending() // run_forth_for("1 2 3 4 5", "1 2 3 4 5")
}

pub fn non_word_chars_are_separators_test() {
  // Note the Ogham Space Mark ( ), this is a spacing character.
  pending() // run_forth_for("1\x002\x013\n4\r5 6\t7", "1 2 3 4 5 6 7")
}

pub fn basic_arithmetic_test() {
  pending() // run_forth_for("1 2 + 4 -", "-1")
}

pub fn integer_division_test() {
  pending() // run_forth_for("2 4 * 3 /", "2")
}

pub fn division_by_zero_test() {
  pending() // forth.new() |> forth.eval("4 2 2 - /") |> error_with(forth.DivisionByZero)
}

pub fn stack_dup1_test() {
  pending() // run_forth_for("1 DUP", "1 1")
}

pub fn stack_dup2_test() {
  pending() // run_forth_for("1 2 DUP", "1 2 2")
}

pub fn stack_dup_fail_test() {
  pending() // forth.new() |> forth.eval("DUP") |> error_with(forth.StackUnderflow)
}

pub fn stack_drop1_test() {
  pending() // run_forth_for("1 drop", "")
}

pub fn stack_drop2_test() {
  pending() // run_forth_for("1 2 drop", "1")
}

pub fn stack_drop_fail_test() {
  pending() // forth.new() |> forth.eval("drop") |> error_with(forth.StackUnderflow)
}

pub fn stack_swap1_test() {
  pending() // run_forth_for("1 2 swap", "2 1")
}

pub fn stack_swap2_test() {
  pending() // run_forth_for("1 2 3 swap", "1 3 2")
}

pub fn stack_swap_fail1_test() {
  pending() // forth.new() |> forth.eval("swap") |> error_with(forth.StackUnderflow)
}

pub fn stack_swap_fail2_test() {
  pending() // forth.new() |> forth.eval("1 swap") |> error_with(forth.StackUnderflow)
}

pub fn stack_over_test1() {
  pending() // run_forth_for("1 2 over", "1 2 1")
}

pub fn stack_over_test2() {
  pending() // run_forth_for("1 2 3 over", "1 2 3 2")
}

pub fn stack_over_fail_test1() {
  pending() // forth.new() |> forth.eval("1 over") |> error_with(forth.StackUnderflow)
}

pub fn stack_over_fail_test2() {
  pending() // forth.new() |> forth.eval("over") |> error_with(forth.StackUnderflow)
}

pub fn define_new_word_test() {
  pending()
  // forth.new()
  //   |> forth.eval(": dup-twice dup dup ;")
  //   |> result.then(forth.eval(_, "1 dup-twice"))
  //   |> result.map(forth.format_stack)
  //   |> succeed_with("1 1 1")
}

pub fn redefine_existing_word_test() {
  pending()
//   forth.new()
//     |> forth.eval(": foo dup ;")
//     |> result.then(forth.eval(": foo dup dup ;"))
//     |> result.then(forth.eval("1 foo"))
//     |> result.map(forth.format_stack())
//     |> succeed_with("1 1 1")
}

pub fn redefining_an_existing_builtin_word() {
  pending()
    // forth.new()
    //   |> forth.eval(": swap dup ;")
    //   |> result.then(forth.eval("1 swap"))
    //   |> result.map(forth.format_stack())
    //   |> succeed_with("1 1")
}

pub fn defining_words_with_odd_characters() {
  pending()
  // run_forth_for(": € 220371 ; €", "220371")
}

pub fn defining_a_number() {
  pending()
  // forth.new() |> forth.eval(": 1 2 ;") |> error_with(forth.InvalidWord)
}

pub fn calling_a_nonexistent_word() {
  pending()
  // forth.new() |> forth.eval("1 foo") |> error_with(forth.UnknownWord)
}
