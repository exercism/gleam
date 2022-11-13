import gleam/option.{Option}
import gleam/string

pub fn two_fer(name: Option(String)) -> String {
  "One for " <> option.unwrap(name, "you") <> ", one for me"
}
