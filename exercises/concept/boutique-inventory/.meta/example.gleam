import gleam/iterator.{Iterator}

pub type Item {
  Item(name: String, price: Int, quantity: Int)
}

pub fn item_names(items: Iterator(Item)) -> Iterator(String) {
  iterator.map(items, fn(item) { item.name })
}

pub fn cheap(items: Iterator(Item)) -> Iterator(Item) {
  todo
}

pub fn out_of_stock(items: Iterator(Item)) -> Iterator(Item) {
  todo
}

pub fn total_stock(items: Iterator(Item)) -> Int {
  todo
}
