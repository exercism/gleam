import gleam/set.{type Set}

pub fn new_collection(card: String) -> Set(String) {
  todo
}

pub fn add_card(collection: Set(String), card: String) -> #(Bool, Set(String)) {
  todo
}

pub fn trade_card(
  my_card: String,
  their_card: String,
  collection: Set(String),
) -> #(Bool, Set(String)) {
  todo
}

pub fn boring_cards(collections: List(Set(String))) -> List(String) {
  todo
}

pub fn total_cards(collections: List(Set(String))) -> Int {
  todo
}

pub fn shiny_cards(collection: Set(String)) -> Set(String) {
  todo
}
