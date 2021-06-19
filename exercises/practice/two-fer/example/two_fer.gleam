import gleam/option.{Option}
import gleam/string

pub fn two_fer(name: Option(String)) -> String {
  "One for "
  |> string.append(option.unwrap(name, "you"))
  |> string.append(", one for me")
}
