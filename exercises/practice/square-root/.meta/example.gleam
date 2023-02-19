import gleam/int

pub fn square_root(radicand: Int) -> Int {
  do_square_root(radicand, radicand)
}

fn do_square_root(radicand: Int, guess: Int) -> Int {
  let diff = int.absolute_value(guess * guess - radicand)
  let new_guess = { radicand / guess + guess } / 2
  let new_diff = int.absolute_value(new_guess * new_guess - radicand)

  case new_diff >= diff {
    True -> guess
    False -> do_square_root(radicand, new_guess)
  }
}
