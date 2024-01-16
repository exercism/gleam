fn prepend(list: List(a), element: a) -> List(a) {
  case list {
    [] -> [element]
    _ -> [element, ..list]
  }
}

pub fn append(first first: List(a), second second: List(a)) -> List(a) {
  foldr(over: first, from: second, with: prepend)
}

pub fn concat(lists: List(List(a))) -> List(a) {
  foldl(over: lists, from: [], with: append)
}

pub fn filter(list: List(a), function: fn(a) -> Bool) -> List(a) {
  foldr(over: list, from: [], with: fn(list, a) {
    case function(a) {
      True -> prepend(list, a)
      False -> list
    }
  })
}

pub fn length(list: List(a)) -> Int {
  foldr(over: list, from: 0, with: fn(length, _) { length + 1 })
}

pub fn map(list: List(a), function: fn(a) -> b) -> List(b) {
  foldr(over: list, from: [], with: fn(list, a) { prepend(list, function(a)) })
}

pub fn foldl(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  case list {
    [] -> initial
    [head, ..tail] ->
      foldl(over: tail, from: function(initial, head), with: function)
  }
}

pub fn foldr(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  case list {
    [] -> initial
    [head, ..tail] ->
      function(foldr(over: tail, from: initial, with: function), head)
  }
}

pub fn reverse(list: List(a)) -> List(a) {
  foldl(over: list, from: [], with: prepend)
}
