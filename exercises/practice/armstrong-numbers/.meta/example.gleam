import gleam/int.{digits}
import gleam/list.{fold, length}

fn repeat_mult(number: Int, power: Int) -> Int {
  case power {
    1 -> number
    pow -> number * repeat_mult(number, pow - 1)
  }
}

pub fn is_armstrong_number(number: Int) -> Bool {
  case digits(number, 10) {
    Ok(digits_list) -> {
      let num_digits = length(digits_list)
      let res =
        fold(digits_list, 0, fn(acc, x) { acc + repeat_mult(x, num_digits) })
      res == number
    }
    _ -> False
  }
}
