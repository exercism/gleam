import boutique_inventory.{Item}
import exercism/should
import exercism/test_runner
import gleam/yielder

pub fn main() {
  test_runner.main()
}

pub fn item_names_test() {
  [
    Item("Red Brown Dress", 65, 3),
    Item("Red Short Skirt", 50, 5),
    Item("Black Short Skirt", 29, 1),
  ]
  |> yielder.from_list
  |> boutique_inventory.item_names
  |> yielder.to_list
  |> should.equal(["Red Brown Dress", "Red Short Skirt", "Black Short Skirt"])
}

pub fn item_names_does_not_consume_yielder_test() {
  yielder.repeatedly(fn() {
    panic as "The yielder should not be consumed by item_names"
  })
  |> boutique_inventory.item_names
  |> yielder.take(0)
  |> yielder.to_list
  |> should.equal([])
}

pub fn cheap_test() {
  [
    Item("Red Brown Dress", 65, 6),
    Item("Black Short Skirt", 29, 8),
    Item("Red Short Skirt", 50, 4),
    Item("Pink Crop Top", 19, 13),
  ]
  |> yielder.from_list
  |> boutique_inventory.cheap
  |> yielder.to_list
  |> should.equal([
    Item("Black Short Skirt", 29, 8),
    Item("Pink Crop Top", 19, 13),
  ])
}

pub fn cheap_does_not_consume_yielder_test() {
  yielder.repeatedly(fn() {
    panic as "The yielder should not be consumed by cheap"
  })
  |> boutique_inventory.cheap
  |> yielder.take(0)
  |> yielder.to_list
  |> should.equal([])
}

pub fn out_of_stock_test() {
  [
    Item("Red Brown Dress", 65, 0),
    Item("Black Short Skirt", 29, 8),
    Item("Red Short Skirt", 50, 4),
    Item("Pink Crop Top", 19, 0),
  ]
  |> yielder.from_list
  |> boutique_inventory.out_of_stock
  |> yielder.to_list
  |> should.equal([Item("Red Brown Dress", 65, 0), Item("Pink Crop Top", 19, 0)])
}

pub fn out_of_stock_does_not_consume_yielder_test() {
  yielder.repeatedly(fn() {
    panic as "The yielder should not be consumed by out_of_stock"
  })
  |> boutique_inventory.out_of_stock
  |> yielder.take(0)
  |> yielder.to_list
  |> should.equal([])
}

pub fn total_stock_test() {
  [
    Item("Red Brown Dress", 65, 0),
    Item("Black Short Skirt", 29, 8),
    Item("Red Short Skirt", 50, 4),
    Item("Pink Crop Top", 19, 16),
  ]
  |> yielder.from_list
  |> boutique_inventory.total_stock()
  |> should.equal(28)
}
