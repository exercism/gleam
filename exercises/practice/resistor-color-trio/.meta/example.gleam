import gleam/int
import gleam/string

pub type Resistance {
  Resistance(unit: String, value: Int)
}

fn get_label(color: String) -> Int {
  case color {
    "black" -> 0
    "brown" -> 1
    "red" -> 2
    "orange" -> 3
    "yellow" -> 4
    "green" -> 5
    "blue" -> 6
    "violet" -> 7
    "grey" -> 8
    "white" -> 9
    _ -> -1
  }
}

pub fn label(colors: List(String)) -> Result(Resistance, Nil) {
  let assert [first_color, second_color, multiplier, ..] = colors

  let first_color_val = get_label(first_color)
  let second_color_val = get_label(second_color)
  let multiplier_val = get_label(multiplier)

  case first_color_val, second_color_val, multiplier_val {
    -1, _, _ -> Error(Nil)
    _, -1, _ -> Error(Nil)
    _, _, -1 -> Error(Nil)
    _, _, _ -> {
      let value_string =
        int.to_string(first_color_val)
        <> int.to_string(second_color_val)
        <> string.repeat("0", multiplier_val)

      let assert Ok(value) = int.parse(value_string)

      let total_zeros = case second_color_val {
        0 -> multiplier_val + 1
        _ -> multiplier_val
      }

      case total_zeros {
        9 -> Ok(Resistance(unit: "gigaohms", value: value / 1_000_000_000))
        6 | 7 | 8 -> Ok(Resistance(unit: "megaohms", value: value / 1_000_000))
        3 | 4 | 5 -> Ok(Resistance(unit: "kiloohms", value: value / 1000))
        0 | 1 | 2 -> Ok(Resistance(unit: "ohms", value: value))
        _ -> Error(Nil)
      }
    }
  }
}
