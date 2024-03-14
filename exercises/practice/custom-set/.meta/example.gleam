import gleam/bool
import gleam/dict.{type Dict}
import gleam/list

pub opaque type Set(t) {
  Set(map: Dict(t, Bool))
}

pub fn new(members: List(t)) -> Set(t) {
  let map =
    members
    |> list.map(fn(member) { #(member, True) })
    |> dict.from_list()

  Set(map)
}

pub fn is_empty(set: Set(t)) -> Bool {
  dict.size(set.map) == 0
}

pub fn contains(in set: Set(t), this member: t) -> Bool {
  dict.has_key(set.map, member)
}

pub fn is_subset(first: Set(t), of second: Set(t)) -> Bool {
  first.map
  |> dict.keys()
  |> list.all(fn(member) { dict.has_key(second.map, member) })
}

pub fn disjoint(first: Set(t), second: Set(t)) -> Bool {
  first.map
  |> dict.keys()
  |> list.any(fn(member) { dict.has_key(second.map, member) })
  |> bool.negate()
}

pub fn is_equal(first: Set(t), to second: Set(t)) -> Bool {
  first == second
}

pub fn add(to set: Set(t), this member: t) -> Set(t) {
  Set(map: dict.insert(set.map, member, True))
}

pub fn intersection(of first: Set(t), and second: Set(t)) -> Set(t) {
  Set(map: dict.take(from: first.map, keeping: dict.keys(second.map)))
}

pub fn difference(between first: Set(t), and second: Set(t)) -> Set(t) {
  Set(map: dict.drop(from: first.map, drop: dict.keys(second.map)))
}

pub fn union(of first: Set(t), and second: Set(t)) -> Set(t) {
  Set(map: dict.merge(into: first.map, from: second.map))
}
