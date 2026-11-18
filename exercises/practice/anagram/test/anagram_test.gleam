import anagram
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn no_matches_test() {
  anagram.find_anagrams("diaper", ["hello", "world", "zombies", "pants"])
  |> should.equal([])
}

pub fn detects_two_anagrams_test() {
  anagram.find_anagrams("solemn", ["lemons", "cherry", "melons"])
  |> should.equal(["lemons", "melons"])
}

pub fn does_not_detect_anagram_subsets_test() {
  anagram.find_anagrams("good", ["dog", "goody"])
  |> should.equal([])
}

pub fn detects_anagram_test() {
  anagram.find_anagrams("listen", ["enlists", "google", "inlets", "banana"])
  |> should.equal(["inlets"])
}

pub fn detects_three_anagrams_test() {
  anagram.find_anagrams("allergy", [
    "gallery", "ballerina", "regally", "clergy", "largely", "leading",
  ])
  |> should.equal(["gallery", "regally", "largely"])
}

pub fn detects_multiple_anagrams_with_different_case_test() {
  anagram.find_anagrams("nose", ["Eons", "ONES"])
  |> should.equal(["Eons", "ONES"])
}

pub fn does_not_detect_non_anagrams_with_identical_checksum_test() {
  anagram.find_anagrams("mass", ["last"])
  |> should.equal([])
}

pub fn detects_anagrams_case_insensitively_test() {
  anagram.find_anagrams("Orchestra", ["cashregister", "Carthorse", "radishes"])
  |> should.equal(["Carthorse"])
}

pub fn detects_anagrams_using_case_insensitive_subject_test() {
  anagram.find_anagrams("Orchestra", ["cashregister", "carthorse", "radishes"])
  |> should.equal(["carthorse"])
}

pub fn detects_anagrams_using_case_insensitive_possible_matches_test() {
  anagram.find_anagrams("orchestra", ["cashregister", "Carthorse", "radishes"])
  |> should.equal(["Carthorse"])
}

pub fn does_not_detect_an_anagram_if_the_original_word_is_repeated_test() {
  anagram.find_anagrams("go", ["goGoGO"])
  |> should.equal([])
}

pub fn anagrams_must_use_all_letters_exactly_once_test() {
  anagram.find_anagrams("tapper", ["patter"])
  |> should.equal([])
}

pub fn words_are_not_anagrams_of_themselves_test() {
  anagram.find_anagrams("BANANA", ["BANANA"])
  |> should.equal([])
}

pub fn words_are_not_anagrams_of_themselves_even_if_letter_case_is_partially_different_test() {
  anagram.find_anagrams("BANANA", ["Banana"])
  |> should.equal([])
}

pub fn words_are_not_anagrams_of_themselves_even_if_letter_case_is_completely_different_test() {
  anagram.find_anagrams("BANANA", ["banana"])
  |> should.equal([])
}

pub fn words_other_than_themselves_can_be_anagrams_test() {
  anagram.find_anagrams("LISTEN", ["LISTEN", "Silent"])
  |> should.equal(["Silent"])
}

pub fn handles_case_of_greek_letters_test() {
  anagram.find_anagrams("ΑΒΓ", ["ΒΓΑ", "ΒΓΔ", "γβα", "αβγ"])
  |> should.equal(["ΒΓΑ", "γβα"])
}

// a⬂ => 61 E2 AC 82
// €a => E2 82 AC 61
pub fn different_characters_may_have_the_same_bytes_test() {
  anagram.find_anagrams("a⬂", ["€a"])
  |> should.equal([])
}
