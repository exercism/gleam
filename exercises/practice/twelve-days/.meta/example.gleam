import gleam/list
import gleam/pair
import gleam/string

const gifts_map = [
  #("first", "a Partridge in a Pear Tree"),
  #("second", "two Turtle Doves"),
  #("third", "three French Hens"),
  #("fourth", "four Calling Birds"),
  #("fifth", "five Gold Rings"),
  #("sixth", "six Geese-a-Laying"),
  #("seventh", "seven Swans-a-Swimming"),
  #("eighth", "eight Maids-a-Milking"),
  #("ninth", "nine Ladies Dancing"),
  #("tenth", "ten Lords-a-Leaping"),
  #("eleventh", "eleven Pipers Piping"),
  #("twelfth", "twelve Drummers Drumming"),
]

pub fn verse(number: Int) -> String {
  let [first_gift, ..gifts] = gifts_map

  let #(day, gifts) = case number {
    1 -> first_gift

    _ -> {
      let [this_verse_gift, ..rest] =
        gifts
        |> list.take(number - 1)
        |> list.reverse()

      pair.map_second(
        this_verse_gift,
        fn(gift) {
          [gift, ..list.map(rest, pair.second)]
          |> string.join(", ")
          |> string.append(", and " <> pair.second(first_gift))
        },
      )
    }
  }

  "On the " <> day <> " day of Christmas my true love gave to me: " <> gifts <> "."
}

pub fn lyrics(from starting_verse: Int, to ending_verse: Int) -> String {
  starting_verse
  |> list.range(ending_verse)
  |> list.map(verse)
  |> string.join("\n")
}
