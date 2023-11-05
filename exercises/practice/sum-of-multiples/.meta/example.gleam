import gleam/set.{type Set}
import gleam/list
import gleam/int

pub fn sum(factors factors: List(Int), limit limit: Int) -> Int {
  factors
  |> list.fold(
    from: set.new(),
    with: fn(multiples, factor) { get_multiples(multiples, factor, limit) },
  )
  |> set.to_list
  |> int.sum
}

fn get_multiples(multiples: Set(Int), factor: Int, limit: Int) -> Set(Int) {
  case limit <= factor {
    True -> multiples
    False ->
      list.range(1, { limit - 1 } / factor)
      |> list.map(int.multiply(_, factor))
      |> list.fold(from: multiples, with: set.insert)
  }
}
