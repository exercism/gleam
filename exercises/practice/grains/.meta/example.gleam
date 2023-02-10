pub type InvalidSquare {
  InvalidSquare
}

fn power(exponent: Int, result: Int) -> Int {
  case exponent {
    0 -> result
    val -> power(val - 1, 2 * result)
  }
}

pub fn square(square: Int) -> Result(Int, InvalidSquare) {
  case square >= 1 && square <= 64 {
    True -> Ok(power(square - 1, 1))
    False -> Error(InvalidSquare)
  }
}

pub fn total() -> Int {
  power(64, 1) - 1
}
