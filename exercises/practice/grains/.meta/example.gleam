pub type Error {
  InvalidSquare
}

fn square_up(exponent: Int, result: Int) -> Int {
  case exponent {
    0 -> result
    val -> square_up(val - 1, 2 * result)
  }
}

pub fn square(square: Int) -> Result(Int, Error) {
  case square >= 1 && square <= 64 {
    True -> Ok(square_up(square - 1, 1))
    False -> Error(InvalidSquare)
  }
}

pub fn total() -> Int {
  square_up(64, 1) - 1
}
