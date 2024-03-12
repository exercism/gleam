import gleam/list.{Continue, Stop}

pub type Error {
  InvalidBase(Int)
  InvalidDigit(Int)
}

pub fn rebase(
  digits digits: List(Int),
  input_base input_base: Int,
  output_base output_base: Int,
) -> Result(List(Int), Error) {
  case input_base, output_base {
    input, output if input >= 2 && output >= 2 ->
      case get_base_number(digits, input) {
        Ok(0) -> Ok([0])
        Ok(value) -> Ok(get_base_digits(value, output, []))
        Error(error_detail) -> Error(error_detail)
      }
    input, _ if input < 2 -> Error(InvalidBase(input))
    _, output if output < 2 -> Error(InvalidBase(output))
  }
}

pub fn get_base_number(digits: List(Int), base: Int) -> Result(Int, Error) {
  list.fold_until(digits, Ok(0), fn(accumulator, digit) {
    case digit >= 0 && digit < base, accumulator {
      True, Ok(current_total) -> Continue(Ok(current_total * base + digit))
      _, _ -> Stop(Error(InvalidDigit(digit)))
    }
  })
}

fn get_base_digits(value: Int, base: Int, result_list: List(Int)) -> List(Int) {
  case value {
    0 -> result_list
    _ -> get_base_digits(value / base, base, [value % base, ..result_list])
  }
}
