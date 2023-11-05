import gleam/string

pub fn reverse_accumulate(
  graphemes_list: List(String),
  result: String,
) -> String {
  case graphemes_list {
    [] -> result
    [token, ..rest] -> reverse_accumulate(rest, token <> result)
  }
}

pub fn reverse(value: String) -> String {
  value
  |> string.to_graphemes
  |> reverse_accumulate("")
}
