import gleam/yielder.{type Yielder}

pub type Item {
  Item(name: String, price: Int, quantity: Int)
}

pub fn item_names(items: Yielder(Item)) -> Yielder(String) {
  todo
}

pub fn cheap(items: Yielder(Item)) -> Yielder(Item) {
  todo
}

pub fn out_of_stock(items: Yielder(Item)) -> Yielder(Item) {
  todo
}

pub fn total_stock(items: Yielder(Item)) -> Int {
  todo
}
