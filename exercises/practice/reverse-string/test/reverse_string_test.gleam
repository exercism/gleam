import gleeunit
import gleeunit/should
import reverse_string

// If property based testing tools are available, a good property to test is reversing a string twice: reverse(reverse(string)) == string

pub fn main() {
  gleeunit.main()
}

pub fn an_empty_string_test() {
  reverse_string.reverse(value: "")
  |> should.equal("")
}

pub fn a_word_test() {
  reverse_string.reverse(value: "robot")
  |> should.equal("tobor")
}

pub fn a_capitalized_word_test() {
  reverse_string.reverse(value: "Ramen")
  |> should.equal("nemaR")
}

pub fn a_sentence_with_punctuation_test() {
  reverse_string.reverse(value: "I'm hungry!")
  |> should.equal("!yrgnuh m'I")
}

pub fn a_palindrome_test() {
  reverse_string.reverse(value: "racecar")
  |> should.equal("racecar")
}

pub fn an_even_sized_word_test() {
  reverse_string.reverse(value: "drawer")
  |> should.equal("reward")
}
