import gleam/string
import gleam/list

pub fn recite(
  start_bottles start_bottles: Int,
  take_down take_down: Int,
) -> String {
  list.range(start_bottles, start_bottles - take_down + 1)
  |> list.map(fn(amount) {
    let current_bottles = pluralise_bottle(amount)
    let remaining_bottles = pluralise_bottle(amount - 1)

    assert Ok(available) = get_phrase(amount)
    assert Ok(remaining) = get_phrase(amount - 1)

    string.concat([
      string.capitalise(available),
      " green ",
      current_bottles,
      " hanging on the wall,\n",
      string.capitalise(available),
      " green ",
      current_bottles,
      " hanging on the wall,\n",
      "And if one green bottle should accidentally fall,\n",
      "There'll be ",
      remaining,
      " green ",
      remaining_bottles,
      " hanging on the wall.",
    ])
  })
  |> string.join("\n\n")
  |> string.trim
}

fn get_phrase(amount: Int) -> Result(String, Nil) {
  case amount {
    0 -> Ok("no")
    1 -> Ok("one")
    2 -> Ok("two")
    3 -> Ok("three")
    4 -> Ok("four")
    5 -> Ok("five")
    6 -> Ok("six")
    7 -> Ok("seven")
    8 -> Ok("eight")
    9 -> Ok("nine")
    10 -> Ok("ten")
    _ -> Error(Nil)
  }
}

fn pluralise_bottle(quantity: Int) -> String {
  case quantity {
    1 -> "bottle"
    _ -> "bottles"
  }
}
