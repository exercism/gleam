import gleam/set.{Set}
import gleam/string
import gleam/list

pub fn new_collection(card: String) -> Set(String) {
  set.from_list([card])
}

pub fn add_card(collection: Set(String), card: String) -> #(Bool, Set(String)) {
  let new = set.contains(collection, card)
  #(new, set.insert(collection, card))
}

pub fn trade_card(
  my_card: String,
  their_card: String,
  collection: Set(String),
) -> #(Bool, Set(String)) {
  let possible = set.contains(collection, my_card)
  let worthwhile = !set.contains(collection, their_card)
  let collection =
    collection
    |> set.delete(my_card)
    |> set.insert(their_card)
  #(possible && worthwhile, collection)
}

pub fn boring_cards(collections: List(Set(String))) -> List(String) {
  let all = list.fold(collections, set.new(), set.union)
  collections
  |> list.fold(all, set.intersection)
  |> set.to_list
}

pub fn total_cards(collections: List(Set(String))) -> Int {
  collections
  |> list.fold(set.new(), set.union)
  |> set.size
}

pub fn shiny_cards(collection: Set(String)) -> Set(String) {
  collection
  |> set.filter(string.starts_with(_, "Shiny "))
}
