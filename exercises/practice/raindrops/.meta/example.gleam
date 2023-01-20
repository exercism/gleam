import gleam/int

pub fn convert(number: Int) -> String {
  let pling = case number % 3 {
    0 -> "Pling"
    _ -> ""
  }
  let plang = case number % 5 {
    0 -> "Plang"
    _ -> ""
  }
  let plong = case number % 7 {
    0 -> "Plong"
    _ -> ""
  }
  case pling <> plang <> plong {
    "" -> int.to_string(number)
    result -> result
  }
}
