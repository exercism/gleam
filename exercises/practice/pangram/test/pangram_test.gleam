import exercism/test_runner
import pangram.{is_pangram}

pub fn main() {
  test_runner.main()
}

pub fn empty_sentence_test() {
  let sentence = ""
  let assert False = is_pangram(sentence)
}

pub fn perfect_lower_case_test() {
  let sentence = "abcdefghijklmnopqrstuvwxyz"
  let assert True = is_pangram(sentence)
}

pub fn only_lower_case_test() {
  let sentence = "the quick brown fox jumps over the lazy dog"
  let assert True = is_pangram(sentence)
}

pub fn missing_the_letter_x_test() {
  let sentence = "a quick movement of the enemy will jeopardize five gunboats"
  let assert False = is_pangram(sentence)
}

pub fn missing_the_letter_h_test() {
  let sentence = "five boxing wizards jump quickly at it"
  let assert False = is_pangram(sentence)
}

pub fn with_underscores_test() {
  let sentence = "the_quick_brown_fox_jumps_over_the_lazy_dog"
  let assert True = is_pangram(sentence)
}

pub fn with_numbers_test() {
  let sentence = "the 1 quick brown fox jumps over the 2 lazy dogs"
  let assert True = is_pangram(sentence)
}

pub fn missing_letters_replaced_by_numbers_test() {
  let sentence = "7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"
  let assert False = is_pangram(sentence)
}

pub fn mixed_case_and_punctuation_test() {
  let sentence = "Five quacking Zephyrs jolt my wax bed."
  let assert True = is_pangram(sentence)
}

pub fn case_insensitive_test() {
  let sentence = "the quick brown fox jumps over with lazy FX"
  let assert False = is_pangram(sentence)
}

pub fn a_m_and_upper_a_m_are_26_different_characters_but_not_a_pangram_test() {
  let sentence = "abcdefghijklm ABCDEFGHIJKLM"
  let assert False = is_pangram(sentence)
}
