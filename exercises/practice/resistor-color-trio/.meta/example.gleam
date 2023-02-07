import gleam/map.{from_list, get}
import gleam/int.{parse, to_string}
import gleam/string.{repeat}
import gleam/list.{at}

pub type CustomRecordType {
  CustomRecordType(unit: String, value: Int)
}

pub fn label(colors: List(String)) -> CustomRecordType {
  let colors_map =
    from_list([
      #("black", 0),
      #("brown", 1),
      #("red", 2),
      #("orange", 3),
      #("yellow", 4),
      #("green", 5),
      #("blue", 6),
      #("violet", 7),
      #("grey", 8),
      #("white", 9),
    ])

  assert Ok(first_color) = at(colors, 0)
  assert Ok(second_color) = at(colors, 1)
  assert Ok(multiplier) = at(colors, 2)

  assert Ok(first_color_val) = get(colors_map, first_color)
  assert Ok(second_color_val) = get(colors_map, second_color)
  assert Ok(multiplier_val) = get(colors_map, multiplier)

  let value_string =
    to_string(first_color_val) <> to_string(second_color_val) <> repeat(
      "0",
      multiplier_val,
    )

  assert Ok(value) = parse(value_string)

  let total_zeros = case second_color_val {
    0 -> multiplier_val + 1
    _ -> multiplier_val
  }

  case total_zeros {
    9 -> CustomRecordType(unit: "gigaohms", value: value / 1_000_000_000)
    6 | 7 | 8 -> CustomRecordType(unit: "megaohms", value: value / 1_000_000)
    3 | 4 | 5 -> CustomRecordType(unit: "kiloohms", value: value / 1000)
    0 | 1 | 2 -> CustomRecordType(unit: "ohms", value: value)
  }
}
