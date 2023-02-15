import gleeunit
import gleeunit/should
import knapsack.{Item}

pub fn main() {
  gleeunit.main()
}

pub fn no_items_test() {
  knapsack.maximum_value([], 100)
  |> should.equal(0)
}

pub fn one_item__too_heavy_test() {
  knapsack.maximum_value([Item(value: 1, weight: 100)], 10)
  |> should.equal(0)
}

pub fn five_items__cannot_be_greedy_by_weight__test() {
  knapsack.maximum_value(
    [
      Item(value: 5, weight: 2),
      Item(value: 5, weight: 2),
      Item(value: 5, weight: 2),
      Item(value: 5, weight: 2),
      Item(value: 21, weight: 10),
    ],
    10,
  )
  |> should.equal(21)
}

pub fn five_items__cannot_be_greedy_by_value__test() {
  knapsack.maximum_value(
    [
      Item(value: 20, weight: 2),
      Item(value: 20, weight: 2),
      Item(value: 20, weight: 2),
      Item(value: 20, weight: 2),
      Item(value: 50, weight: 10),
    ],
    10,
  )
  |> should.equal(80)
}

pub fn example_knapsack_test() {
  knapsack.maximum_value(
    [
      Item(value: 10, weight: 5),
      Item(value: 40, weight: 4),
      Item(value: 30, weight: 6),
      Item(value: 50, weight: 4),
    ],
    10,
  )
  |> should.equal(90)
}

pub fn eight_items_test() {
  knapsack.maximum_value(
    [
      Item(value: 350, weight: 25),
      Item(value: 400, weight: 35),
      Item(value: 450, weight: 45),
      Item(value: 20, weight: 5),
      Item(value: 70, weight: 25),
      Item(value: 8, weight: 3),
      Item(value: 5, weight: 2),
      Item(value: 5, weight: 2),
    ],
    104,
  )
  |> should.equal(900)
}

pub fn fifteen_items_test() {
  knapsack.maximum_value(
    [
      Item(value: 135, weight: 70),
      Item(value: 139, weight: 73),
      Item(value: 149, weight: 77),
      Item(value: 150, weight: 80),
      Item(value: 156, weight: 82),
      Item(value: 163, weight: 87),
      Item(value: 173, weight: 90),
      Item(value: 184, weight: 94),
      Item(value: 192, weight: 98),
      Item(value: 201, weight: 106),
      Item(value: 210, weight: 110),
      Item(value: 214, weight: 113),
      Item(value: 221, weight: 115),
      Item(value: 229, weight: 118),
      Item(value: 240, weight: 120),
    ],
    750,
  )
  |> should.equal(1458)
}
