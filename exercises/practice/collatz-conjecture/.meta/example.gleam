pub fn get_collatz(number: Int) -> Int {
  case number {
    1 -> 0
    val ->
      case val % 2 == 0 {
        True -> 1 + get_collatz(val / 2)
        False -> 1 + get_collatz(3 * val + 1)
      }
  }
}

pub fn steps(number: Int) -> Result(Int, String) {
  case number <= 0 {
    True -> Error("Only positive numbers are allowed")
    False -> Ok(get_collatz(number))
  }
}
