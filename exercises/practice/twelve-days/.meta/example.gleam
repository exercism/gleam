import gleam/list
import gleam/pair
import gleam/string

pub fn verse(number: Int) -> String {
  let #(day, gift) = day_gift_pair_for_verse(number)

  let gifts = case number {
    1 -> gift
    _ ->
      list.range(from: number, to: 2)
      |> list.map(day_gift_pair_for_verse)
      |> list.map(pair.second)
      |> string.join(", ")
      |> string.append(", and a Partridge in a Pear Tree")
  }

  "On the "
  <> day
  <> " day of Christmas my true love gave to me: "
  <> gifts
  <> "."
}

pub fn lyrics(from starting_verse: Int, to ending_verse: Int) -> String {
  starting_verse
  |> list.range(ending_verse)
  |> list.map(verse)
  |> string.join("\n")
}

fn day_gift_pair_for_verse(number) {
  case number {
    1 -> #("first", "a Partridge in a Pear Tree")
    2 -> #("second", "two Turtle Doves")
    3 -> #("third", "three French Hens")
    4 -> #("fourth", "four Calling Birds")
    5 -> #("fifth", "five Gold Rings")
    6 -> #("sixth", "six Geese-a-Laying")
    7 -> #("seventh", "seven Swans-a-Swimming")
    8 -> #("eighth", "eight Maids-a-Milking")
    9 -> #("ninth", "nine Ladies Dancing")
    10 -> #("tenth", "ten Lords-a-Leaping")
    11 -> #("eleventh", "eleven Pipers Piping")
    _12 -> #("twelfth", "twelve Drummers Drumming")
  }
}
