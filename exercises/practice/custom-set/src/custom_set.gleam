pub opaque type CustomSet(t) {
  CustomSet
}

pub fn new(members: List(t)) -> CustomSet(t) {
  todo
}

pub fn is_empty(custom_set: CustomSet(t)) -> Bool {
  todo
}

pub fn contains(in custom_set: CustomSet(t), this member: t) -> Bool {
  todo
}

pub fn is_subset(first: CustomSet(t), of second: CustomSet(t)) -> Bool {
  todo
}

pub fn disjoint(first: CustomSet(t), second: CustomSet(t)) -> Bool {
  todo
}

pub fn is_equal(first: CustomSet(t), to second: CustomSet(t)) -> Bool {
  todo
}

pub fn add(to custom_set: CustomSet(t), this member: t) -> CustomSet(t) {
  todo
}

pub fn intersection(
  of first: CustomSet(t),
  and second: CustomSet(t),
) -> CustomSet(t) {
  todo
}

pub fn difference(
  between first: CustomSet(t),
  and second: CustomSet(t),
) -> CustomSet(t) {
  todo
}

pub fn union(of first: CustomSet(t), and second: CustomSet(t)) -> CustomSet(t) {
  todo
}
