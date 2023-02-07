import gleeunit
import gleeunit/should
import scrabble_score

pub fn main() {
  gleeunit.main()
}

pub fn lowercase_letter_test() {
  scrabble_score.score("a")
  |> should.equal(1)
}

pub fn uppercase_letter_test() {
  scrabble_score.score("A")
  |> should.equal(1)
}

pub fn valuable_letter_test() {
  scrabble_score.score("f")
  |> should.equal(4)
}

pub fn short_word_test() {
  scrabble_score.score("at")
  |> should.equal(2)
}

pub fn short__valuable_word_test() {
  scrabble_score.score("zoo")
  |> should.equal(12)
}

pub fn medium_word_test() {
  scrabble_score.score("street")
  |> should.equal(6)
}

pub fn medium__valuable_word_test() {
  scrabble_score.score("quirky")
  |> should.equal(22)
}

pub fn long__mixed_case_word_test() {
  scrabble_score.score("OxyphenButazone")
  |> should.equal(41)
}

pub fn english_like_word_test() {
  scrabble_score.score("pinata")
  |> should.equal(8)
}

pub fn empty_input_test() {
  scrabble_score.score("")
  |> should.equal(0)
}

pub fn entire_alphabet_available_test() {
  scrabble_score.score("abcdefghijklmnopqrstuvwxyz")
  |> should.equal(87)
}
