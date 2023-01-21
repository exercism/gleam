import gleam/string
import gleam/list

pub fn slices(input: String, size: Int) -> Result(List(String), Error) {
  use <- require(input != "", EmptySeries)
  use <- require(size != 0, SliceLengthZero)
  use <- require(size >= 0, SliceLengthNegative)

  let characters = string.to_graphemes(input)
  let length = list.length(characters)
  use <- require(size <= length, SliceLengthTooLarge)

  characters
  |> list.window(size)
  |> list.map(string.concat)
  |> Ok
}

fn require(
  condition: Bool,
  error: Error,
  next: fn() -> Result(t, Error),
) -> Result(t, Error) {
  case condition {
    True -> next()
    False -> Error(error)
  }
}

pub type Error {
  SliceLengthNegative
  SliceLengthTooLarge
  SliceLengthZero
  EmptySeries
}
