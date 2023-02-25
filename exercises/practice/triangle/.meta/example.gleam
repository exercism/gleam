import gleam/int
import gleam/list

pub fn equilateral(sides: List(Int)) -> Bool {
  let is_valid_triangle = is_valid_triangle(sides)

  case list.unique(sides) {
    [_] if is_valid_triangle -> True
    _ -> False
  }
}

pub fn isosceles(sides: List(Int)) -> Bool {
  let is_valid_triangle = is_valid_triangle(sides)

  case list.unique(sides) {
    [_, _] if is_valid_triangle -> True
    [_] if is_valid_triangle -> True
    _ -> False
  }
}

pub fn scalene(sides: List(Int)) -> Bool {
  let is_valid_triangle = is_valid_triangle(sides)

  case list.unique(sides) {
    [_, _, _] if is_valid_triangle -> True
    _ -> False
  }
}

fn is_valid_triangle(sides: List(Int)) -> Bool {
  let [a, b, c] = list.sort(sides, int.compare)

  a > 0 && b > 0 && c > 0 && a + b >= c
}
