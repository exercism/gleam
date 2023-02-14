import gleam/int

pub type Item {
  Item(value: Int, weight: Int)
}

pub fn maximum_value(items: List(Item), maximum_weight: Int) -> Int {
  do_maximum_value(items, maximum_weight)
}

fn do_maximum_value(items: List(Item), remaining_weight: Int) -> Int {
  case items {
    [] -> 0
    [Item(_, weight), ..tail] if weight > remaining_weight ->
      do_maximum_value(tail, remaining_weight)
    [Item(value, weight), ..tail] ->
      int.max(
        do_maximum_value(tail, remaining_weight),
        value + do_maximum_value(tail, remaining_weight - weight),
      )
  }
}
