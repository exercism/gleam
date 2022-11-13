import gleam/list
import gleam/string

pub fn convert(number: Int) {
  number
  |> list.repeat("I", _)
  |> string.concat
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
