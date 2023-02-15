import gleam/int

pub type Item {
  Item(value: Int, weight: Int)
}

pub fn maximum_value(items: List(Item), maximum_weight: Int) -> Int {
  case items {
    [] -> 0
    [Item(_, weight), ..tail] if weight > maximum_weight ->
      maximum_value(tail, maximum_weight)
    [Item(value, weight), ..tail] ->
      int.max(
        maximum_value(tail, maximum_weight),
        value + maximum_value(tail, maximum_weight - weight),
      )
  }
}
