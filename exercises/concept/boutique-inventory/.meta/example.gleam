import gleam/yielder.{type Yielder}

pub type Item {
  Item(name: String, price: Int, quantity: Int)
}

pub fn item_names(items: Yielder(Item)) -> Yielder(String) {
  yielder.map(items, fn(item) { item.name })
}

pub fn cheap(items: Yielder(Item)) -> Yielder(Item) {
  yielder.filter(items, fn(item) { item.price < 30 })
}

pub fn out_of_stock(items: Yielder(Item)) -> Yielder(Item) {
  yielder.filter(items, fn(item) { item.quantity == 0 })
}

pub fn total_stock(items: Yielder(Item)) -> Int {
  yielder.fold(items, 0, fn(total, item) { total + item.quantity })
}
