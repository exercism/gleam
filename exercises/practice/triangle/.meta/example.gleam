import gleam/float
import gleam/list

pub fn equilateral(a: Float, b: Float, c: Float) -> Bool {
  let is_valid_triangle = is_valid_triangle(a, b, c)

  case list.unique([a, b, c]) {
    [_] if is_valid_triangle -> True
    _ -> False
  }
}

pub fn isosceles(a: Float, b: Float, c: Float) -> Bool {
  let is_valid_triangle = is_valid_triangle(a, b, c)

  case list.unique([a, b, c]) {
    [_, _] if is_valid_triangle -> True
    [_] if is_valid_triangle -> True
    _ -> False
  }
}

pub fn scalene(a: Float, b: Float, c: Float) -> Bool {
  let is_valid_triangle = is_valid_triangle(a, b, c)

  case list.unique([a, b, c]) {
    [_, _, _] if is_valid_triangle -> True
    _ -> False
  }
}

fn is_valid_triangle(a: Float, b: Float, c: Float) -> Bool {
  assert [a, b, c] = list.sort([a, b, c], float.compare)

  a >. 0.0 && b >. 0.0 && c >. 0.0 && a +. b >=. c
}
