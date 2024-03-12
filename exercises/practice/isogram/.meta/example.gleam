import gleam/list
import gleam/string

pub fn is_isogram(phrase phrase: String) -> Bool {
  let graphemes =
    phrase
    |> string.lowercase
    |> string.replace(" ", "")
    |> string.replace("-", "")
    |> string.to_graphemes

  let number_of_unique_graphemes =
    graphemes
    |> list.unique
    |> list.length

  list.length(graphemes) == number_of_unique_graphemes
}
