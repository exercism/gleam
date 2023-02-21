import gleam/string
import gleam/list
import gleam/pair

pub type Student {
  Alice
  Bob
  Charlie
  David
  Eve
  Fred
  Ginny
  Harriet
  Ileana
  Joseph
  Kincaid
  Larry
}

pub type Plant {
  Radishes
  Clover
  Violets
  Grass
}

pub fn plants(diagram: String, student: Student) -> List(Plant) {
  diagram
  |> string.split("\n")
  |> list.flat_map(fn(cups) {
    cups
    |> string.to_graphemes
    |> list.sized_chunk(into: 2)
    |> list.zip(students)
    |> list.filter(fn(pair) { pair.second(pair) == student })
    |> list.flat_map(pair.first)
    |> list.map(to_plant)
  })
}

fn to_plant(letter: String) -> Plant {
  case letter {
    "R" -> Radishes
    "C" -> Clover
    "V" -> Violets
    "G" -> Grass
  }
}

const students = [
  Alice,
  Bob,
  Charlie,
  David,
  Eve,
  Fred,
  Ginny,
  Harriet,
  Ileana,
  Joseph,
  Kincaid,
  Larry,
]
