pub fn reply(guess: Int) -> String {
  case guess {
    _ if guess < 41 -> "Too low"
    41 -> "So close"
    42 -> "Correct"
    43 -> "So close"
    _ -> "Too high"
  }
}
