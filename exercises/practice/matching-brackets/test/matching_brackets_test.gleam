import exercism/test_runner
import matching_brackets

pub fn main() {
  test_runner.main()
}

pub fn paired_square_brackets_test() {
  let assert True = matching_brackets.is_paired("[]")
}

pub fn empty_string_test() {
  let assert True = matching_brackets.is_paired("")
}

pub fn unpaired_brackets_test() {
  let assert False = matching_brackets.is_paired("[[")
}

pub fn wrong_ordered_brackets_test() {
  let assert False = matching_brackets.is_paired("}{")
}

pub fn wrong_closing_bracket_test() {
  let assert False = matching_brackets.is_paired("{]")
}

pub fn paired_with_whitespace_test() {
  let assert True = matching_brackets.is_paired("{ }")
}

pub fn partially_paired_brackets_test() {
  let assert False = matching_brackets.is_paired("{[])")
}

pub fn simple_nested_brackets_test() {
  let assert True = matching_brackets.is_paired("{[]}")
}

pub fn several_paired_brackets_test() {
  let assert True = matching_brackets.is_paired("{}[]")
}

pub fn paired_and_nested_brackets_test() {
  let assert True = matching_brackets.is_paired("([{}({}[])])")
}

pub fn unopened_closing_brackets_test() {
  let assert False = matching_brackets.is_paired("{[)][]}")
}

pub fn unpaired_and_nested_brackets_test() {
  let assert False = matching_brackets.is_paired("([{])")
}

pub fn paired_and_wrong_nested_brackets_test() {
  let assert False = matching_brackets.is_paired("[({]})")
}

pub fn paired_and_wrong_nested_brackets_but_innermost_are_correct_test() {
  let assert False = matching_brackets.is_paired("[({}])")
}

pub fn paired_and_incomplete_brackets_test() {
  let assert False = matching_brackets.is_paired("{}[")
}

pub fn too_many_closing_brackets_test() {
  let assert False = matching_brackets.is_paired("[]]")
}

pub fn early_unexpected_brackets_test() {
  let assert False = matching_brackets.is_paired(")()")
}

pub fn early_mismatched_brackets_test() {
  let assert False = matching_brackets.is_paired("{)()")
}

pub fn math_expression_test() {
  let assert True =
    matching_brackets.is_paired("(((185 + 223.85) * 15) - 543)/2")
}

pub fn complex_latex_expression_test() {
  let assert True =
    matching_brackets.is_paired(
      "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)",
    )
}
