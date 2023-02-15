import gleam/list
import gleam/result

pub type Error {
  InvalidBase
  InvalidDigit
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
        _ -> Error(InvalidDigit)
      }
    _, _ -> Error(InvalidBase)
  }
}

fn get_base_number(digits: List(Int), base: Int) -> Result(Int, Error) {
  digits
  |> list.fold(
    Ok(0),
    fn(acc, cur) {
      case cur, acc {
        _, Error(InvalidDigit) -> Error(InvalidDigit)
        value, acc if value >= 0 && value < base ->
          result.then(acc, fn(acc_val) { Ok(acc_val * base + value) })
        _, _ -> Error(InvalidDigit)
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
