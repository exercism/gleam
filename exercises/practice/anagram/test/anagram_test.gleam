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

pub fn simple_test() {
  find_anagrams("ant", ["tan", "stand", "at"])
  |> should.equal(["tan"])
}

pub fn no_false_positive_test() {
  find_anagrams("galea", ["eagle"])
  |> should.equal([])
}

pub fn no_superset_or_subset_test() {
  find_anagrams("good", ["goody", "god"])
  |> should.equal([])
}

pub fn single_test() {
  find_anagrams("listen", ["enlists", "google", "inlets", "banana"])
  |> should.equal(["inlets"])
}

pub fn multiple_test() {
  find_anagrams(
    "allergy",
    ["gallery", "ballerina", "regally", "clergy", "largely", "leading"],
  )
  |> should.equal(["gallery", "regally", "largely"])
}

pub fn case_insensitive_test() {
  find_anagrams("Orchestra", ["cashregister", "Carthorse", "radishes"])
  |> should.equal(["Carthorse"])
}

pub fn no_self_detection_test() {
  find_anagrams("banana", ["Banana"])
  |> should.equal([])
}
