import gleam/result.{Option}
import gleam/string

pub fn two_fer(name: Option(String)) -> String {
  "One for "
  |> string.append(result.unwrap(name, "you"))
  |> string.append(", one for me")
}
