import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn valid(value: String) -> Bool {
  value
  |> parse_digits()
  |> result.then(validate_length)
  |> result.map(is_valid_number)
  |> result.unwrap(False)
}

fn parse_digits(value: String) -> Result(List(Int), Nil) {
  value
  |> string.replace(" ", "")
  |> string.to_graphemes()
  |> list.try_map(int.parse)
}

fn validate_length(digits: List(Int)) -> Result(List(Int), Nil) {
  case list.length(digits) > 1 {
    True -> Ok(digits)
    False -> Error(Nil)
  }
}

fn is_valid_number(digits: List(Int)) -> Bool {
  digits
  |> list.reverse()
  |> list.sized_chunk(into: 2)
  |> list.flat_map(with: double_second_digit)
  |> int.sum()
  |> is_evenly_divisible(by: 10)
}

fn double_second_digit(digits: List(Int)) -> List(Int) {
  case digits {
    [first, second] -> {
      let second = case second * 2 {
        double if double > 9 -> double - 9
        double -> double
      }

      [first, second]
    }

    _ -> digits
  }
}

fn is_evenly_divisible(digit: Int, by factor: Int) -> Bool {
  digit % factor == 0
}
