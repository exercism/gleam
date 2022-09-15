import gleam/string

pub fn hey(remark: String) -> String {
  let remark = string.trim(remark)
  let is_empty = string.is_empty(remark)
  let is_question = string.ends_with(remark, "?")
  let is_yelling =
    string.uppercase(remark) == remark && string.lowercase(remark) != remark

  case is_empty, is_yelling, is_question {
    True, _, _ -> "Fine. Be that way!"
    _, True, True -> "Calm down, I know what I'm doing!"
    _, True, _ -> "Whoa, chill out!"
    _, _, True -> "Sure."
    _, _, _ -> "Whatever."
  }
}
