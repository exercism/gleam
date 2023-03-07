import gleam/list
import gleam/string

pub type Output {
  Unknown
  Digit(Int)
  List(List(Output))
}

pub type Error {
  InvalidLineNumber
  InvalidRowNumber
}

pub fn convert(input: String) -> Result(Output, Error) {
  let lines =
    input
    |> string.split("\n")
    |> list.drop(1)

  case lines, list.length(lines) % 4 == 0 {
    [], _ | _, False -> Error(InvalidLineNumber)
    [line, ..], True ->
      case line, string.length(line) % 3 == 0 {
        "", _ | _, False -> Error(InvalidRowNumber)
        _, True -> Ok(safe_convert(lines))
      }
  }
}

fn safe_convert(lines: List(String)) -> Output {
  let outputs =
    lines
    |> list.sized_chunk(4)
    |> list.map(convert_line)

  case outputs {
    [line] -> line
    _ -> List(outputs)
  }
}

fn convert_line(lines: List(String)) -> Output {
  let outputs =
    lines
    |> list.map(fn(line) {
      line
      |> string.to_graphemes
      |> list.sized_chunk(3)
      |> list.map(string.concat)
    })
    |> list.transpose
    |> list.map(read_digit)

  case outputs {
    [digit] -> digit
    _ -> List(outputs)
  }
}

fn read_digit(lines: List(String)) -> Output {
  case "\n" <> string.join(lines, "\n") {
    "
 _ 
| |
|_|
   " -> Digit(0)
    "
   
  |
  |
   " -> Digit(1)
    "
 _ 
 _|
|_ 
   " -> Digit(2)
    "
 _ 
 _|
 _|
   " -> Digit(3)
    "
   
|_|
  |
   " -> Digit(4)
    "
 _ 
|_ 
 _|
   " -> Digit(5)
    "
 _ 
|_ 
|_|
   " -> Digit(6)
    "
 _ 
  |
  |
   " -> Digit(7)
    "
 _ 
|_|
|_|
   " -> Digit(8)
    "
 _ 
|_|
 _|
   " -> Digit(9)
    _ -> Unknown
  }
}
