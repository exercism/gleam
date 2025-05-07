import gleam/option.{Some}
import gleam/regexp

pub fn is_valid_line(line: String) -> Bool {
  let assert Ok(re) = regexp.from_string("^\\[(DEBUG|INFO|WARNING|ERROR)\\]")
  regexp.check(re, line)
}

pub fn split_line(line: String) -> List(String) {
  let assert Ok(re) = regexp.from_string("\\<[\\*=\\-~]*\\>")
  regexp.split(re, line)
}

pub fn tag_with_user_name(line: String) -> String {
  let assert Ok(re) = regexp.from_string("User\\s+(\\S+)")
  case regexp.scan(re, line) {
    [regexp.Match(_, [Some(name)])] -> "[USER] " <> name <> " " <> line
    _ -> line
  }
}
