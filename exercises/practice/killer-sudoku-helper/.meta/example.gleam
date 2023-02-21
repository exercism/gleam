import gleam/int
import gleam/list

pub fn combinations(
  size size: Int,
  sum sum: Int,
  exclude exclude: List(Int),
) -> List(List(Int)) {
  list.range(1, 9)
  |> list.filter(fn(digit) { !list.contains(exclude, digit) })
  |> list.combinations(size)
  |> list.filter(fn(combination) { int.sum(combination) == sum })
}
