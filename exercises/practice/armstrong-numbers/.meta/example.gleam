import gleam/int
import gleam/list

fn repeat_mult(number: Int, power: Int) -> Int {
  case power {
    1 -> number
    pow -> number * repeat_mult(number, pow - 1)
  }
}

pub fn is_armstrong_number(number: Int) -> Bool {
  case int.digits(number, 10) {
    Ok(digits) -> {
      let num_digits = list.length(digits)
      let res =
        list.fold(digits, 0, fn(acc, x) { acc + repeat_mult(x, num_digits) })
      res == number
    }
    _ -> False
  }
}
