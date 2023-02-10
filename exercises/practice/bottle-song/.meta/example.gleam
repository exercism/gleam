import gleam/string
import gleam/list

pub fn recite(
  start_bottles start_bottles: Int,
  take_down take_down: Int,
) -> String {
  let phrases = [
    "no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "ten",
  ]

  list.range(start_bottles, start_bottles - take_down + 1)
  |> list.fold(
    "",
    fn(acc, amount) {
      let current_bottles = pluralise_bottle(amount)
      let remaining_bottles = pluralise_bottle(amount - 1)

      assert Ok(available) = list.at(phrases, amount)
      assert Ok(remaining) = list.at(phrases, amount - 1)

      let current_phrase =
        string.capitalise(available) <> " green " <> current_bottles <> " hanging on the wall,\n" <> string.capitalise(
          available,
        ) <> " green " <> current_bottles <> " hanging on the wall,\n" <> "And if one green bottle should accidentally fall,\n" <> "There'll be " <> remaining <> " green " <> remaining_bottles <> " hanging on the wall.\n" <> "\n"

      acc <> current_phrase
    },
  )
  |> string.trim
}

fn pluralise_bottle(quantity: Int) -> String {
  case quantity {
    1 -> "bottle"
    _ -> "bottles"
  }
}
