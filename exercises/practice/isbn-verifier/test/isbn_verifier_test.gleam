import exercism/should
import exercism/test_runner
import isbn_verifier

// An expected value of true indicates a valid ISBN-10, 
// whereas false means the ISBN-10 is invalid.

pub fn main() {
  test_runner.main()
}

pub fn valid_isbn_test() {
  isbn_verifier.is_valid("3-598-21508-8")
  |> should.equal(True)
}

pub fn invalid_isbn_check_digit_test() {
  isbn_verifier.is_valid("3-598-21508-9")
  |> should.equal(False)
}

pub fn valid_isbn_with_a_check_digit_of_10_test() {
  isbn_verifier.is_valid("3-598-21507-X")
  |> should.equal(True)
}

pub fn check_digit_is_a_character_other_than_x_test() {
  isbn_verifier.is_valid("3-598-21507-A")
  |> should.equal(False)
}

pub fn invalid_check_digit_in_isbn_is_not_treated_as_zero_test() {
  isbn_verifier.is_valid("4-598-21507-B")
  |> should.equal(False)
}

pub fn invalid_character_in_isbn_is_not_treated_as_zero_test() {
  isbn_verifier.is_valid("3-598-P1581-X")
  |> should.equal(False)
}

pub fn x_is_only_valid_as_a_check_digit_test() {
  isbn_verifier.is_valid("3-598-2X507-9")
  |> should.equal(False)
}

pub fn only_one_check_digit_is_allowed_test() {
  isbn_verifier.is_valid("3-598-21508-96")
  |> should.equal(False)
}

pub fn x_is_not_substituted_by_the_value_10_test() {
  isbn_verifier.is_valid("3-598-2X507-5")
  |> should.equal(False)
}

pub fn valid_isbn_without_separating_dashes_test() {
  isbn_verifier.is_valid("3598215088")
  |> should.equal(True)
}

pub fn isbn_without_separating_dashes_and_x_as_check_digit_test() {
  isbn_verifier.is_valid("359821507X")
  |> should.equal(True)
}

pub fn isbn_without_check_digit_and_dashes_test() {
  isbn_verifier.is_valid("359821507")
  |> should.equal(False)
}

pub fn too_long_isbn_and_no_dashes_test() {
  isbn_verifier.is_valid("3598215078X")
  |> should.equal(False)
}

pub fn too_short_isbn_test() {
  isbn_verifier.is_valid("00")
  |> should.equal(False)
}

pub fn isbn_without_check_digit_test() {
  isbn_verifier.is_valid("3-598-21507")
  |> should.equal(False)
}

pub fn check_digit_of_x_should_not_be_used_for_0_test() {
  isbn_verifier.is_valid("3-598-21515-X")
  |> should.equal(False)
}

pub fn empty_isbn_test() {
  isbn_verifier.is_valid("")
  |> should.equal(False)
}

pub fn input_is_9_characters_test() {
  isbn_verifier.is_valid("134456729")
  |> should.equal(False)
}

pub fn invalid_characters_are_not_ignored_after_checking_length_test() {
  isbn_verifier.is_valid("3132P34035")
  |> should.equal(False)
}

// Catch invalid characters in an otherwise valid isbn
pub fn invalid_characters_are_not_ignored_before_checking_length_test() {
  isbn_verifier.is_valid("3598P215088")
  |> should.equal(False)
}

pub fn input_is_too_long_but_contains_a_valid_isbn_test() {
  isbn_verifier.is_valid("98245726788")
  |> should.equal(False)
}
