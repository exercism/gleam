import gleam/int
import gleam/list

pub fn equilateral(a: Int, b: Int, c: Int) -> Bool {
  let is_valid_triangle = is_valid_triangle(a, b, c)

  case list.unique([a, b, c]) {
    [_] if is_valid_triangle -> True
    _ -> False
  }
}

pub fn isosceles(a: Int, b: Int, c: Int) -> Bool {
  let is_valid_triangle = is_valid_triangle(a, b, c)

  case list.unique([a, b, c]) {
    [_, _] if is_valid_triangle -> True
    [_] if is_valid_triangle -> True
    _ -> False
  }
}

pub fn scalene(a: Int, b: Int, c: Int) -> Bool {
  let is_valid_triangle = is_valid_triangle(a, b, c)

  case list.unique([a, b, c]) {
    [_, _, _] if is_valid_triangle -> True
    _ -> False
  }
}

fn is_valid_triangle(a: Int, b: Int, c: Int) -> Bool {
  let [a, b, c] = list.sort([a, b, c], int.compare)

  a > 0 && b > 0 && c > 0 && a + b >= c
}
