import gleeunit
import gleeunit/should
import isogram

pub fn main() {
  gleeunit.main()
}

pub fn empty_string_test() {
  isogram.is_isogram(phrase: "")
  |> should.be_true
}

pub fn isogram_with_only_lower_case_characters_test() {
  isogram.is_isogram(phrase: "isogram")
  |> should.be_true
}

pub fn word_with_one_duplicated_character_test() {
  isogram.is_isogram(phrase: "eleven")
  |> should.be_false
}

pub fn word_with_one_duplicated_character_from_the_end_of_the_alphabet_test() {
  isogram.is_isogram(phrase: "zzyzx")
  |> should.be_false
}

pub fn longest_reported_english_isogram_test() {
  isogram.is_isogram(phrase: "subdermatoglyphic")
  |> should.be_true
}

pub fn word_with_duplicated_character_in_mixed_case_test() {
  isogram.is_isogram(phrase: "Alphabet")
  |> should.be_false
}

pub fn word_with_duplicated_character_in_mixed_case__lowercase_first_test() {
  isogram.is_isogram(phrase: "alphAbet")
  |> should.be_false
}

pub fn hypothetical_isogrammic_word_with_hyphen_test() {
  isogram.is_isogram(phrase: "thumbscrew-japingly")
  |> should.be_true
}

pub fn hypothetical_word_with_duplicated_character_following_hyphen_test() {
  isogram.is_isogram(phrase: "thumbscrew-jappingly")
  |> should.be_false
}

pub fn isogram_with_duplicated_hyphen_test() {
  isogram.is_isogram(phrase: "six-year-old")
  |> should.be_true
}

pub fn made_up_name_that_is_an_isogram_test() {
  isogram.is_isogram(phrase: "Emily Jung Schwartzkopf")
  |> should.be_true
}

pub fn duplicated_character_in_the_middle_test() {
  isogram.is_isogram(phrase: "accentor")
  |> should.be_false
}

pub fn same_first_and_last_characters_test() {
  isogram.is_isogram(phrase: "angola")
  |> should.be_false
}

pub fn word_with_duplicated_character_and_with_two_hyphens_test() {
  isogram.is_isogram(phrase: "up-to-date")
  |> should.be_false
}
