import gleam/option.{Option}

pub fn two_fer(name: Option(String)) -> String {
  "One for " <> option.unwrap(name, "you") <> ", one for me."
}
