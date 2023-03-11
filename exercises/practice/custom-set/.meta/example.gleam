import gleam/bool
import gleam/list
import gleam/map.{Map}

pub opaque type CustomSet(t) {
  CustomSet(map: Map(t, Bool))
}

pub fn new(members: List(t)) -> CustomSet(t) {
  let map =
    members
    |> list.map(fn(member) { #(member, True) })
    |> map.from_list()

  CustomSet(map)
}

pub fn is_empty(custom_set: CustomSet(t)) -> Bool {
  map.size(custom_set.map) == 0
}

pub fn contains(in custom_set: CustomSet(t), this member: t) -> Bool {
  map.has_key(custom_set.map, member)
}

pub fn is_subset(first: CustomSet(t), of second: CustomSet(t)) -> Bool {
  first.map
  |> map.keys()
  |> list.all(fn(member) { map.has_key(second.map, member) })
}

pub fn disjoint(first: CustomSet(t), second: CustomSet(t)) -> Bool {
  first.map
  |> map.keys()
  |> list.any(fn(member) { map.has_key(second.map, member) })
  |> bool.negate()
}

pub fn is_equal(first: CustomSet(t), to second: CustomSet(t)) -> Bool {
  first == second
}

pub fn add(to custom_set: CustomSet(t), this member: t) -> CustomSet(t) {
  CustomSet(map: map.insert(custom_set.map, member, True))
}

pub fn intersection(
  of first: CustomSet(t),
  and second: CustomSet(t),
) -> CustomSet(t) {
  CustomSet(map: map.take(from: first.map, keeping: map.keys(second.map)))
}

pub fn difference(
  between first: CustomSet(t),
  and second: CustomSet(t),
) -> CustomSet(t) {
  CustomSet(map: map.drop(from: first.map, drop: map.keys(second.map)))
}

pub fn union(of first: CustomSet(t), and second: CustomSet(t)) -> CustomSet(t) {
  CustomSet(map: map.merge(into: first.map, from: second.map))
}
