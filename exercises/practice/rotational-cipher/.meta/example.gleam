import gleam/string
import gleam/list
import gleam/dict.{type Dict}
import gleam/result

fn shift_map(shift_key: Int, text: String) -> Dict(String, String) {
  let graphemes = string.to_graphemes(text)
  let shifted =
    list.append(
      list.drop(graphemes, shift_key),
      list.take(graphemes, shift_key),
    )
  list.zip(graphemes, shifted)
  |> dict.from_list
}

fn shift_letters(shift_key: Int) -> Dict(String, String) {
  dict.merge(
    shift_map(shift_key, "abcdefghijklmnopqrstuvwxyz"),
    shift_map(shift_key, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
  )
}

pub fn rotate(shift_key: Int, text: String) -> String {
  let letter_map = shift_letters(shift_key)

  text
  |> string.to_graphemes()
  |> list.map(fn(letter) {
    dict.get(letter_map, letter)
    |> result.unwrap(letter)
  })
  |> string.concat
}
