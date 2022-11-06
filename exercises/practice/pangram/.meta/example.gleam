import gleam/string
import gleam/set

const alphabet = "abcdefghijklmnopqrstuvwxyz"

pub fn is_pangram(sentence: String) -> Bool {
  let alphabet = set.from_list(string.to_graphemes(alphabet))

  let sentence =
    sentence
    |> string.lowercase
    |> string.to_graphemes
    |> set.from_list
    |> set.intersection(alphabet)

  sentence == alphabet
}
