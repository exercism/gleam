import gleam/string
import gleam/list

pub fn is_paired(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.fold(from: [], with: match)
  |> list.is_empty
}

fn match(stack: List(String), char: String) -> List(String) {
  case #(char, stack) {
    #("}", ["{", ..rest]) | #("]", ["[", ..rest]) | #(")", ["(", ..rest]) ->
      rest
    #("}", _) | #("]", _) | #(")", _) -> [char, ..stack]
    #("{", _) | #("[", _) | #("(", _) -> [char, ..stack]
    _ -> stack
  }
}
