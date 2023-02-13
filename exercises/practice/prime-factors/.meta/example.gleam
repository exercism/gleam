import gleam/list
import gleam/int

pub fn factors(value: Int) -> List(Int) {
  find_factors(value, 2, [], [])
}

fn find_factors(
  value: Int,
  guess: Int,
  primes: List(Int),
  factors: List(Int),
) -> List(Int) {
  case guess * guess > value {
    True if value == 1 -> list.sort(factors, int.compare)
    True -> list.sort([value, ..factors], int.compare)
    False -> {
      let next_guess = case guess % 6 {
        1 -> guess + 4
        2 -> 3
        3 -> 5
        5 -> guess + 2
      }

      case list.all(primes, fn(prime) { guess % prime != 0 }) {
        False -> find_factors(value, next_guess, primes, factors)
        True -> {
          let #(value, factors) = reduce(value, guess, factors)
          let primes = [guess, ..primes]
          find_factors(value, next_guess, primes, factors)
        }
      }
    }
  }
}

fn reduce(value: Int, prime: Int, factors: List(Int)) -> #(Int, List(Int)) {
  case value % prime {
    0 -> reduce(value / prime, prime, [prime, ..factors])
    _ -> #(value, factors)
  }
}
