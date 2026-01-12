import gleam/float
import gleam/int
import gleam/list

fn armstrong_number_loop(list: List(Int), npower: Float) -> Int {
  case list {
    [] -> 0
    [item, ..rest] -> {
      let assert Ok(total) = int.power(item, npower)
      float.round(total) + armstrong_number_loop(rest, npower)
    }
  }
}

pub fn is_armstrong_number(number: Int) -> Bool {
  case int.digits(number, 10) {
    Ok(digits) -> {
      let npower = list.length(digits) |> int.to_float
      let result = armstrong_number_loop(digits, npower)
      result == number
    }
    _ -> False
  }
}
