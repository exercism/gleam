import gleam/string.{to_graphemes}

pub fn reverse_accumulate(
  graphemes_list: List(String),
  result: String,
) -> String {
  case graphemes_list {
    [] -> result
    [token, ..rest] -> reverse_accumulate(rest, token <> result)
  }
}

pub fn reverse(value value: String) -> String {
  value
  |> to_graphemes
  |> reverse_accumulate("")
}
