import gleam/list
import gleam/result

pub type Error {
  InvalidBase(Int)
  InvalidDigitSequence(Int)
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

fn get_base_number(digits: List(Int), base: Int) -> Result(Int, Error) {
  list.fold(
    digits,
    Ok(0),
    fn(accumulator, digit) {
      case digit, accumulator {
        value, Ok(current) if value >= 0 && value < base ->
          Ok(current * base + value)
        value, present_val ->
          result.then(present_val, fn(_) { Error(InvalidDigitSequence(value)) })
      }
    },
  )
}

fn get_base_digits(value: Int, base: Int, result_list: List(Int)) -> List(Int) {
  case value {
    0 -> result_list
    _ -> get_base_digits(value / base, base, [value % base, ..result_list])
  }
}
