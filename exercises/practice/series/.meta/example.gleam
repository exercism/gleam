import gleam/bool
import gleam/list
import gleam/string

pub fn slices(input: String, size: Int) -> Result(List(String), Error) {
  use <- bool.guard(input == "", Error(EmptySeries))
  use <- bool.guard(size == 0, Error(SliceLengthZero))
  use <- bool.guard(size < 0, Error(SliceLengthNegative))

  let characters = string.to_graphemes(input)
  let length = list.length(characters)
  use <- bool.guard(size > length, Error(SliceLengthTooLarge))

  characters
  |> list.window(size)
  |> list.map(string.concat)
  |> Ok
}

pub type Error {
  SliceLengthNegative
  SliceLengthTooLarge
  SliceLengthZero
  EmptySeries
}
