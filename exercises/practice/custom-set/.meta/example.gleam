import gleam/bool
import gleam/list
import gleam/map.{type Map}

pub opaque type Set(t) {
  Set(map: Map(t, Bool))
}

pub fn new(members: List(t)) -> Set(t) {
  let map =
    members
    |> list.map(fn(member) { #(member, True) })
    |> map.from_list()

  Set(map)
}

pub fn is_empty(set: Set(t)) -> Bool {
  map.size(set.map) == 0
}

pub fn contains(in set: Set(t), this member: t) -> Bool {
  map.has_key(set.map, member)
}

pub fn is_subset(first: Set(t), of second: Set(t)) -> Bool {
  first.map
  |> map.keys()
  |> list.all(fn(member) { map.has_key(second.map, member) })
}

pub fn disjoint(first: Set(t), second: Set(t)) -> Bool {
  first.map
  |> map.keys()
  |> list.any(fn(member) { map.has_key(second.map, member) })
  |> bool.negate()
}

pub fn is_equal(first: Set(t), to second: Set(t)) -> Bool {
  first == second
}

pub fn add(to set: Set(t), this member: t) -> Set(t) {
  Set(map: map.insert(set.map, member, True))
}

pub fn intersection(of first: Set(t), and second: Set(t)) -> Set(t) {
  Set(map: map.take(from: first.map, keeping: map.keys(second.map)))
}

pub fn difference(between first: Set(t), and second: Set(t)) -> Set(t) {
  Set(map: map.drop(from: first.map, drop: map.keys(second.map)))
}

pub fn union(of first: Set(t), and second: Set(t)) -> Set(t) {
  Set(map: map.merge(into: first.map, from: second.map))
}
