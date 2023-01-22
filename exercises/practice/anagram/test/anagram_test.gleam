import gleeunit
import gleeunit/should
import anagram.{find_anagrams}

pub fn main() {
  gleeunit.main()
}

pub fn no_matches_test() {
  find_anagrams("diaper", ["hello", "world", "zombies", "pants"])
  |> should.equal([])
}

pub fn does_not_detect_anagram_subsets_test() {
  find_anagrams("good", ["dog", "goody"])
  |> should.equal([])
}

pub fn detects_anagram_test() {
  find_anagrams("listen", ["enlists", "google", "inlets", "banana"])
  |> should.equal(["inlets"])
}

pub fn detects_three_anagrams_test() {
  find_anagrams(
    "allergy",
    ["gallery", "ballerina", "regally", "clergy", "largely", "leading"],
  )
  |> should.equal(["gallery", "regally", "largely"])
}

pub fn detects_anagrams_case_insensitively_test() {
  find_anagrams("Orchestra", ["cashregister", "Carthorse", "radishes"])
  |> should.equal(["Carthorse"])
}
