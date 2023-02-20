import gleam/list
import gleam/string

pub fn recite(start_verse start_verse: Int, end_verse end_verse: Int) -> String {
  list.range(from: 1, to: end_verse)
  |> list.scan(
    from: [],
    with: fn(previous, number) { [verse(number), ..previous] },
  )
  |> list.map(fn(parts) { string.join(["This is", ..parts], " the ") })
  |> list.drop(start_verse - 1)
  |> string.join("\n")
}

fn verse(number: Int) -> String {
  case number {
    1 -> "house that Jack built."
    2 -> "malt that lay in"
    3 -> "rat that ate"
    4 -> "cat that killed"
    5 -> "dog that worried"
    6 -> "cow with the crumpled horn that tossed"
    7 -> "maiden all forlorn that milked"
    8 -> "man all tattered and torn that kissed"
    9 -> "priest all shaven and shorn that married"
    10 -> "rooster that crowed in the morn that woke"
    11 -> "farmer sowing his corn that kept"
    12 -> "horse and the hound and the horn that belonged to"
  }
}
