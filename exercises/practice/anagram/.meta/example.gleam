import gleam/list
import gleam/string
import gleam/string_builder

fn normalize(word: String) {
  word
  |> string.lowercase
  |> string.to_graphemes
  |> list.sort(string.compare)
  |> string_builder.from_strings
  |> string_builder.to_string
}

fn is_anagram(normalized_word: String, candidate: String) {
  normalized_word == normalize(candidate)
}

pub fn find_anagrams(word: String, candidates: List(String)) -> List(String) {
  let normalized_word = normalize(word)

  candidates
  |> list.filter(fn(candidate) {
    string.lowercase(word) != string.lowercase(candidate)
  })
  |> list.filter(is_anagram(normalized_word, _))
}
