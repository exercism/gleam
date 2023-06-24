import exercism/test_runner
import isbn_verifier

pub fn main() {
  test_runner.main()
}

pub fn valid_isbn_test() {
  let assert True = isbn_verifier.is_valid("3-598-21508-8")
}

pub fn invalid_isbn_check_digit_test() {
  let assert False = isbn_verifier.is_valid("3-598-21508-9")
}

pub fn valid_isbn_with_a_check_digit_of_10_test() {
  let assert True = isbn_verifier.is_valid("3-598-21507-X")
}

pub fn check_digit_is_a_character_other_than_x_test() {
  let assert False = isbn_verifier.is_valid("3-598-21507-A")
}

pub fn invalid_check_digit_in_isbn_is_not_treated_as_zero_test() {
  let assert False = isbn_verifier.is_valid("4-598-21507-B")
}

pub fn invalid_character_in_isbn_is_not_treated_as_zero_test() {
  let assert False = isbn_verifier.is_valid("3-598-P1581-X")
}

pub fn x_is_only_valid_as_a_check_digit_test() {
  let assert False = isbn_verifier.is_valid("3-598-2X507-9")
}

pub fn valid_isbn_without_separating_dashes_test() {
  let assert True = isbn_verifier.is_valid("3598215088")
}

pub fn isbn_without_separating_dashes_and_x_as_check_digit_test() {
  let assert True = isbn_verifier.is_valid("359821507X")
}

pub fn isbn_without_check_digit_and_dashes_test() {
  let assert False = isbn_verifier.is_valid("359821507")
}

pub fn too_long_isbn_and_no_dashes_test() {
  let assert False = isbn_verifier.is_valid("3598215078X")
}

pub fn too_short_isbn_test() {
  let assert False = isbn_verifier.is_valid("00")
}

pub fn isbn_without_check_digit_test() {
  let assert False = isbn_verifier.is_valid("3-598-21507")
}

pub fn check_digit_of_x_should_not_be_used_for_0_test() {
  let assert False = isbn_verifier.is_valid("3-598-21515-X")
}

pub fn empty_isbn_test() {
  let assert False = isbn_verifier.is_valid("")
}

pub fn input_is_9_characters_test() {
  let assert False = isbn_verifier.is_valid("134456729")
}

pub fn invalid_characters_are_not_ignored_after_checking_length_test() {
  let assert False = isbn_verifier.is_valid("3132P34035")
}

pub fn invalid_characters_are_not_ignored_before_checking_length_test() {
  let assert False = isbn_verifier.is_valid("3598P215088")
}

pub fn input_is_too_long_but_contains_a_valid_isbn_test() {
  let assert False = isbn_verifier.is_valid("98245726788")
}
