import exercism/test_runner
import isogram

pub fn main() {
  test_runner.main()
}

pub fn empty_string_test() {
  let assert True = isogram.is_isogram(phrase: "")
}

pub fn isogram_with_only_lower_case_characters_test() {
  let assert True = isogram.is_isogram(phrase: "isogram")
}

pub fn word_with_one_duplicated_character_test() {
  let assert False = isogram.is_isogram(phrase: "eleven")
}

pub fn word_with_one_duplicated_character_from_the_end_of_the_alphabet_test() {
  let assert False = isogram.is_isogram(phrase: "zzyzx")
}

pub fn longest_reported_english_isogram_test() {
  let assert True = isogram.is_isogram(phrase: "subdermatoglyphic")
}

pub fn word_with_duplicated_character_in_mixed_case_test() {
  let assert False = isogram.is_isogram(phrase: "Alphabet")
}

pub fn word_with_duplicated_character_in_mixed_case_lowercase_first_test() {
  let assert False = isogram.is_isogram(phrase: "alphAbet")
}

pub fn hypothetical_isogrammic_word_with_hyphen_test() {
  let assert True = isogram.is_isogram(phrase: "thumbscrew-japingly")
}

pub fn hypothetical_word_with_duplicated_character_following_hyphen_test() {
  let assert False = isogram.is_isogram(phrase: "thumbscrew-jappingly")
}

pub fn isogram_with_duplicated_hyphen_test() {
  let assert True = isogram.is_isogram(phrase: "six-year-old")
}

pub fn made_up_name_that_is_an_isogram_test() {
  let assert True = isogram.is_isogram(phrase: "Emily Jung Schwartzkopf")
}

pub fn duplicated_character_in_the_middle_test() {
  let assert False = isogram.is_isogram(phrase: "accentor")
}

pub fn same_first_and_last_characters_test() {
  let assert False = isogram.is_isogram(phrase: "angola")
}

pub fn word_with_duplicated_character_and_with_two_hyphens_test() {
  let assert False = isogram.is_isogram(phrase: "up-to-date")
}
