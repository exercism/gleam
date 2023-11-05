import gleam/int
import gleam/float
import gleam/iterator.{type Iterator}

pub fn prime(number: Int) -> Result(Int, Nil) {
  case number > 0 {
    True -> iterator.at(primes(), number - 1)
    False -> Error(Nil)
  }
}

fn primes() -> Iterator(Int) {
  iterator.single(2)
  |> iterator.append(iterator.single(3))
  |> iterator.append(
    iterator.interleave(
      iterator.iterate(from: 5, with: fn(x) { x + 6 }),
      iterator.iterate(from: 7, with: fn(x) { x + 6 }),
    )
    |> iterator.filter(is_prime),
  )
}

fn is_prime(number: Int) -> Bool {
  let assert Ok(root_float) = int.square_root(number)
  let root = float.round(float.floor(root_float))

  number == 2 || number == 3 || number == 5 || iterator.range(5, root)
  |> iterator.all(fn(x) { number % x != 0 })
}
