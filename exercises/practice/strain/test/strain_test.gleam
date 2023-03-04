import gleeunit
import gleam/int
import gleam/list
import gleam/string
import strain

pub fn main() {
  gleeunit.main()
}

pub fn empty_keep_test() {
  let assert [] = strain.keep([], fn(_) { True })
}

pub fn keeps_everything_test() {
  let assert [1, 2, 3] = strain.keep([1, 2, 3], fn(e) { e < 10 })
}

pub fn keeps_odd_numbers_test() {
  let assert [1, 3] = strain.keep([1, 2, 3], int.is_odd)
}

pub fn keeps_even_numbers_test() {
  let assert [2, 4] = strain.keep([1, 2, 3, 4, 5], int.is_even)
}

pub fn keeps_strings_starting_with_z_test() {
  let assert ["zebra", "zombies", "zelot"] =
    strain.keep(
      ["apple", "zebra", "banana", "zombies", "cherimoya", "zelot"],
      fn(word) { string.starts_with(word, "z") },
    )
}

pub fn keeps_lists_containing_the_number_5_test() {
  let list = [
    [1, 2, 3],
    [5, 5, 5],
    [5, 1, 2],
    [2, 1, 2],
    [1, 5, 2],
    [2, 2, 1],
    [1, 2, 5],
  ]

  let assert [[5, 5, 5], [5, 1, 2], [1, 5, 2], [1, 2, 5]] =
    strain.keep(list, fn(row) { list.any(row, fn(n) { n == 5 }) })
}

pub fn empty_discard_test() {
  let assert [] = strain.discard([], fn(_) { True })
}

pub fn discards_nothing_test() {
  let assert [1, 2, 3] = strain.discard([1, 2, 3], fn(e) { e > 10 })
}

pub fn discards_odd_numbers_test() {
  let assert [2] = strain.discard([1, 2, 3], int.is_odd)
}

pub fn discards_even_numbers_test() {
  let assert [1, 3, 5] = strain.discard([1, 2, 3, 4, 5], int.is_even)
}

pub fn discards_strings_starting_with_z_test() {
  let assert ["apple", "banana", "cherimoya"] =
    strain.discard(
      ["apple", "zebra", "banana", "zombies", "cherimoya", "zelot"],
      fn(word) { string.starts_with(word, "z") },
    )
}

pub fn discards_lists_containing_the_number_5_test() {
  let list = [
    [1, 2, 3],
    [5, 5, 5],
    [5, 1, 2],
    [2, 1, 2],
    [1, 5, 2],
    [2, 2, 1],
    [1, 2, 5],
  ]

  let assert [[1, 2, 3], [2, 1, 2], [2, 2, 1]] =
    strain.discard(list, fn(row) { list.any(row, fn(n) { n == 5 }) })
}
