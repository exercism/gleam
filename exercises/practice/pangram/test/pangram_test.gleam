import exercism/should
import exercism/test_runner
import pangram

// A pangram is a sentence using every letter of the alphabet at least once.
// Output should be a boolean denoting if the string is a pangram or not.

pub fn main() {
  test_runner.main()
}

pub fn empty_sentence_test() {
  pangram.is_pangram("")
  |> should.equal(False)
}

pub fn perfect_lower_case_test() {
  pangram.is_pangram("abcdefghijklmnopqrstuvwxyz")
  |> should.equal(True)
}

pub fn only_lower_case_test() {
  pangram.is_pangram("the quick brown fox jumps over the lazy dog")
  |> should.equal(True)
}

pub fn missing_the_letter_x__test() {
  pangram.is_pangram(
    "a quick movement of the enemy will jeopardize five gunboats",
  )
  |> should.equal(False)
}

pub fn missing_the_letter_h__test() {
  pangram.is_pangram("five boxing wizards jump quickly at it")
  |> should.equal(False)
}

pub fn with_underscores_test() {
  pangram.is_pangram("the_quick_brown_fox_jumps_over_the_lazy_dog")
  |> should.equal(True)
}

pub fn with_numbers_test() {
  pangram.is_pangram("the 1 quick brown fox jumps over the 2 lazy dogs")
  |> should.equal(True)
}

pub fn missing_letters_replaced_by_numbers_test() {
  pangram.is_pangram("7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog")
  |> should.equal(False)
}

pub fn mixed_case_and_punctuation_test() {
  pangram.is_pangram("\"Five quacking Zephyrs jolt my wax bed.\"")
  |> should.equal(True)
}

pub fn a_m_and_a_m_are_26_different_characters_but_not_a_pangram_test() {
  pangram.is_pangram("abcdefghijklm ABCDEFGHIJKLM")
  |> should.equal(False)
}
