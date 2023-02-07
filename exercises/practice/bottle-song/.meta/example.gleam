import gleam/string.{capitalise}
import gleam/list.{append, at}

pub fn pluralise_bottle(qty: Int) -> String {
  case qty {
    1 -> "bottle"
    _ -> "bottles"
  }
}

pub fn recite_helper(
  start_bottles: Int,
  take_down: Int,
  result: List(String),
) -> List(String) {
  case take_down {
    0 -> result
    _ -> {
      let phrases = [
        "no", "one", "two", "three", "four", "five", "six", "seven", "eight",
        "nine", "ten",
      ]
      assert Ok(available) = at(phrases, start_bottles)

      assert Ok(remaining) = at(phrases, start_bottles - 1)

      let current_bottles = pluralise_bottle(start_bottles)
      let remaining_bottles = pluralise_bottle(start_bottles - 1)

      let current_phrase: List(String) = [
        capitalise(available) <> " green " <> current_bottles <> " hanging on the wall,",
        capitalise(available) <> " green " <> current_bottles <> " hanging on the wall,",
        "And if one green bottle should accidentally fall,",
        "There'll be " <> remaining <> " green " <> remaining_bottles <> " hanging on the wall.",
      ]

      case take_down == 1 {
        False ->
          recite_helper(
            start_bottles - 1,
            take_down - 1,
            append(result, append(current_phrase, [""])),
          )
        _ ->
          recite_helper(
            start_bottles - 1,
            take_down - 1,
            append(result, current_phrase),
          )
      }
    }
  }
}

pub fn recite(
  start_bottles start_bottles: Int,
  take_down take_down: Int,
) -> List(String) {
  recite_helper(start_bottles, take_down, [])
}
