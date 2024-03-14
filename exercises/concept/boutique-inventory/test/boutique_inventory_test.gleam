import boutique_inventory.{Item}
import exercism/should
import exercism/test_runner
import gleam/iterator

pub fn main() {
  test_runner.main()
}

pub fn item_names_test() {
  [
    Item("Red Brown Dress", 65, 3),
    Item("Red Short Skirt", 50, 5),
    Item("Black Short Skirt", 29, 1),
  ]
  |> iterator.from_list
  |> boutique_inventory.item_names
  |> iterator.to_list
  |> should.equal(["Red Brown Dress", "Red Short Skirt", "Black Short Skirt"])
}

pub fn item_names_does_not_consume_iterator_test() {
  iterator.repeatedly(fn() {
    panic as "The iterator should not be consumed by item_names"
  })
  |> boutique_inventory.item_names
  |> iterator.take(0)
  |> iterator.to_list
  |> should.equal([])
}

pub fn cheap_test() {
  [
    Item("Red Brown Dress", 65, 6),
    Item("Black Short Skirt", 29, 8),
    Item("Red Short Skirt", 50, 4),
    Item("Pink Crop Top", 19, 13),
  ]
  |> iterator.from_list
  |> boutique_inventory.cheap
  |> iterator.to_list
  |> should.equal([
    Item("Black Short Skirt", 29, 8),
    Item("Pink Crop Top", 19, 13),
  ])
}

pub fn cheap_does_not_consume_iterator_test() {
  iterator.repeatedly(fn() {
    panic as "The iterator should not be consumed by cheap"
  })
  |> boutique_inventory.cheap
  |> iterator.take(0)
  |> iterator.to_list
  |> should.equal([])
}

pub fn out_of_stock_terst() {
  [
    Item("Red Brown Dress", 65, 0),
    Item("Black Short Skirt", 29, 8),
    Item("Red Short Skirt", 50, 4),
    Item("Pink Crop Top", 19, 0),
  ]
  |> iterator.from_list
  |> boutique_inventory.out_of_stock
  |> iterator.to_list
  |> should.equal([Item("Red Brown Dress", 65, 0), Item("Pink Crop Top", 19, 0)])
}

pub fn out_of_stock_does_not_consume_iterator_test() {
  iterator.repeatedly(fn() {
    panic as "The iterator should not be consumed by out_of_stock"
  })
  |> boutique_inventory.out_of_stock
  |> iterator.take(0)
  |> iterator.to_list
  |> should.equal([])
}

pub fn total_stock_test() {
  [
    Item("Red Brown Dress", 65, 0),
    Item("Black Short Skirt", 29, 8),
    Item("Red Short Skirt", 50, 4),
    Item("Pink Crop Top", 19, 16),
  ]
  |> iterator.from_list
  |> boutique_inventory.total_stock
  |> should.equal(28)
}
