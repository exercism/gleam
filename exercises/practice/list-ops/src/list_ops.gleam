pub fn append(first first: List(a), second second: List(a)) -> List(a) {
  todo
}

pub fn concat(lists: List(List(a))) -> List(a) {
  todo
}

pub fn filter(list: List(a), function: fn(a) -> Bool) -> List(a) {
  todo
}

pub fn length(list: List(a)) -> Int {
  todo
}

pub fn map(list: List(a), function: fn(a) -> b) -> List(b) {
  todo
}

pub fn foldl(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  todo
}

pub fn foldr(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  todo
}

pub fn reverse(list: List(a)) -> List(a) {
  todo
}
