import gleam/iterator.{type Iterator}

pub type Item {
  Item(name: String, price: Int, quantity: Int)
}

pub fn item_names(items: Iterator(Item)) -> Iterator(String) {
  iterator.map(items, fn(item) { item.name })
}

pub fn cheap(items: Iterator(Item)) -> Iterator(Item) {
  iterator.filter(items, fn(item) { item.price < 30 })
}

pub fn out_of_stock(items: Iterator(Item)) -> Iterator(Item) {
  iterator.filter(items, fn(item) { item.quantity == 0 })
}

pub fn total_stock(items: Iterator(Item)) -> Int {
  iterator.fold(items, 0, fn(total, item) { total + item.quantity })
}
