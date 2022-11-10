import gleam/list
import gleam/string_builder

pub fn convert(number: Int) {
  number
  |> list.repeat("I", _)
  |> string_builder.from_strings
  |> string_builder.replace("IIIII", "V")
  |> string_builder.replace("VV", "X")
  |> string_builder.replace("XXXXX", "L")
  |> string_builder.replace("LL", "C")
  |> string_builder.replace("CCCCC", "D")
  |> string_builder.replace("DD", "M")
  |> string_builder.replace("DCCCC", "CM")
  |> string_builder.replace("CCCC", "CD")
  |> string_builder.replace("LXXXX", "XC")
  |> string_builder.replace("XXXX", "XL")
  |> string_builder.replace("VIIII", "IX")
  |> string_builder.replace("IIII", "IV")
  |> string_builder.to_string
}
