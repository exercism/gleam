import exercism/test_runner
import pangram

// A pangram is a sentence using every letter of the alphabet at least once.
// Output should be a boolean denoting if the string is a pangram or not.

pub fn main() {
  test_runner.main()
}

pub fn empty_sentence_test() {
  let assert False = pangram.is_pangram("")
}

pub fn perfect_lower_case_test() {
  let assert True = pangram.is_pangram("abcdefghijklmnopqrstuvwxyz")
}

pub fn only_lower_case_test() {
  let assert True = pangram.is_pangram("the quick brown fox jumps over the lazy dog")
}

pub fn missing_the_letter_x__test() {
  let assert False = pangram.is_pangram(
    "a quick movement of the enemy will jeopardize five gunboats",
  )
}

pub fn missing_the_letter_h__test() {
  let assert False = pangram.is_pangram("five boxing wizards jump quickly at it")
}

pub fn with_underscores_test() {
  let assert True = pangram.is_pangram("the_quick_brown_fox_jumps_over_the_lazy_dog")
}

pub fn with_numbers_test() {
  let assert True = pangram.is_pangram("the 1 quick brown fox jumps over the 2 lazy dogs")
}

pub fn missing_letters_replaced_by_numbers_test() {
  let assert False = pangram.is_pangram("7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog")
}

pub fn mixed_case_and_punctuation_test() {
  let assert True = pangram.is_pangram("\"Five quacking Zephyrs jolt my wax bed.\"")
}

pub fn a_m_and_a_m_are_26_different_characters_but_not_a_pangram_test() {
  let assert False = pangram.is_pangram("abcdefghijklm ABCDEFGHIJKLM")
}
