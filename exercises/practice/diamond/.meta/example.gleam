import gleam/list
import gleam/string
import gleam/result
import gleam/bit_string

pub fn build(letter: String) -> String {
  let limit = letter_to_int(letter)
  limit
  |> range
  |> list.map(build_row(_, limit))
  |> string.join("\n")
}

fn build_row(i: Int, size: Int) -> String {
  let letter = int_to_letter(i)

  case i {
    0 -> {
      let outside = string.repeat(" ", size)
      outside <> letter <> outside
    }

    _ -> {
      let outside = string.repeat(" ", size - i)
      let inside = string.repeat(" ", i * 2 - 1)
      outside <> letter <> inside <> letter <> outside
    }
  }
}

fn range(size: Int) -> List(Int) {
  case size {
    0 -> [0]
    _ ->
      list.range(0, size)
      |> list.append(list.range(size - 1, 0))
  }
}

fn letter_to_int(letter: String) -> Int {
  case <<letter:utf8>> {
    <<letter>> -> letter - 65
    _ -> 0
  }
}

fn int_to_letter(i: Int) -> String {
  let i = i + 65
  <<i>>
  |> bit_string.to_string
  |> result.unwrap("A")
}
