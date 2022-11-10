import gleam/list
import gleam/string
import gleam/string_builder

pub fn convert(number: Int) {
  number
  |> list.repeat("I", _)
  |> string_builder.from_strings
  |> string_builder.to_string
  |> string.replace("IIIII", "V")
  |> string.replace("VV", "X")
  |> string.replace("XXXXX", "L")
  |> string.replace("LL", "C")
  |> string.replace("CCCCC", "D")
  |> string.replace("DD", "M")
  |> string.replace("DCCCC", "CM")
  |> string.replace("CCCC", "CD")
  |> string.replace("LXXXX", "XC")
  |> string.replace("XXXX", "XL")
  |> string.replace("VIIII", "IX")
  |> string.replace("IIII", "IV")
}
