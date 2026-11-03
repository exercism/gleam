import gleam/int
import gleam/list

pub type Triplet {
  Triplet(first: Int, second: Int, thrid: Int)
}

pub fn triplets_with_sum(sum: Int) -> List(Triplet) {
  let start = [Triplet(3, 4, 5)]
  generate_triples(start, sum, [])
  |> list.map(order_sides)
  |> list.sort(fn(a, b) { int.compare(a.first, b.first) })
}

fn generate_triples(
  candidates: List(Triplet),
  sum: Int,
  valid: List(Triplet),
) -> List(Triplet) {
  case candidates {
    [] -> valid
    [triplet, ..rest] -> {
      let n = sum_of_sides(triplet)

      let valid = case sum % n == 0 {
        True -> [multiply_sides(triplet, sum / n), ..valid]
        False -> valid
      }

      let candidates = case n >= sum {
        True -> rest
        False -> list.append(next_triples(triplet), rest)
      }

      generate_triples(candidates, sum, valid)
    }
  }
}

fn sum_of_sides(triplet: Triplet) -> Int {
  let Triplet(a, b, c) = triplet
  a + b + c
}

fn multiply_sides(triplet: Triplet, factor: Int) -> Triplet {
  let Triplet(a, b, c) = triplet
  Triplet(factor * a, factor * b, factor * c)
}

// https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples
fn next_triples(triplet: Triplet) -> List(Triplet) {
  let Triplet(a, b, c) = triplet
  [
    Triplet(a - 2 * b + 2 * c, 2 * a - b + 2 * c, 2 * a - 2 * b + 3 * c),
    Triplet(a + 2 * b + 2 * c, 2 * a + b + 2 * c, 2 * a + 2 * b + 3 * c),
    Triplet(0 - a + 2 * b + 2 * c, -2 * a + b + 2 * c, -2 * a + 2 * b + 3 * c),
  ]
}

fn order_sides(triplet: Triplet) -> Triplet {
  let Triplet(a, b, c) = triplet
  let assert [a, b, c] = list.sort([a, b, c], int.compare)
  Triplet(a, b, c)
}
