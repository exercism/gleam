import exercism/should
import exercism/test_runner
import reverse_string

pub fn main() {
  test_runner.main()
}

pub fn an_empty_string_test() {
  reverse_string.reverse("")
  |> should.equal("")
}

pub fn a_word_test() {
  reverse_string.reverse("robot")
  |> should.equal("tobor")
}

pub fn a_capitalized_word_test() {
  reverse_string.reverse("Ramen")
  |> should.equal("nemaR")
}

pub fn a_sentence_with_punctuation_test() {
  reverse_string.reverse("I'm hungry!")
  |> should.equal("!yrgnuh m'I")
}

pub fn a_palindrome_test() {
  reverse_string.reverse("racecar")
  |> should.equal("racecar")
}

pub fn an_even_sized_word_test() {
  reverse_string.reverse("drawer")
  |> should.equal("reward")
}
