import gleam/float
import gleam/int
import gleam/yielder.{type Yielder}

pub fn prime(number: Int) -> Result(Int, Nil) {
  case number > 0 {
    True -> yielder.at(primes(), number - 1)
    False -> Error(Nil)
  }
}

fn primes() -> Yielder(Int) {
  yielder.single(2)
  |> yielder.append(yielder.single(3))
  |> yielder.append(
    yielder.interleave(
      yielder.iterate(from: 5, with: fn(x) { x + 6 }),
      yielder.iterate(from: 7, with: fn(x) { x + 6 }),
    )
    |> yielder.filter(is_prime),
  )
}

fn is_prime(number: Int) -> Bool {
  let assert Ok(root_float) = int.square_root(number)
  let root = float.round(float.floor(root_float))

  number == 2
  || number == 3
  || number == 5
  || yielder.range(5, root)
  |> yielder.all(fn(x) { number % x != 0 })
}
