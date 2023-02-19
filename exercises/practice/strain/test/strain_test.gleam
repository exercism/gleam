import gleeunit
import gleeunit/should
import gleam/int
import gleam/list
import gleam/string
import strain

pub fn main() {
  gleeunit.main()
}

pub fn empty_keep_test() {
  []
  |> strain.keep(fn(_) { True })
  |> should.equal([])
}

pub fn keeps_everything_test() {
  [1, 2, 3]
  |> strain.keep(fn(e) { e < 10 })
  |> should.equal([1, 2, 3])
}

pub fn keeps_odd_numbers_test() {
  [1, 2, 3]
  |> strain.keep(int.is_odd)
  |> should.equal([1, 3])
}

pub fn keeps_even_numbers_test() {
  [1, 2, 3, 4, 5]
  |> strain.keep(int.is_even)
  |> should.equal([2, 4])
}

pub fn keeps_strings_starting_with_z_test() {
  ["apple", "zebra", "banana", "zombies", "cherimoya", "zelot"]
  |> strain.keep(fn(word) { string.starts_with(word, "z") })
  |> should.equal(["zebra", "zombies", "zelot"])
}

pub fn keeps_lists_containing_the_number_5_test() {
  [[1, 2, 3], [5, 5, 5], [5, 1, 2], [2, 1, 2], [1, 5, 2], [2, 2, 1], [1, 2, 5]]
  |> strain.keep(fn(row) { list.any(row, fn(n) { n == 5 }) })
  |> should.equal([[5, 5, 5], [5, 1, 2], [1, 5, 2], [1, 2, 5]])
}

pub fn empty_discard_test() {
  []
  |> strain.discard(fn(_) { True })
  |> should.equal([])
}

pub fn discards_nothing_test() {
  [1, 2, 3]
  |> strain.discard(fn(e) { e > 10 })
  |> should.equal([1, 2, 3])
}

pub fn discards_odd_numbers_test() {
  [1, 2, 3]
  |> strain.discard(int.is_odd)
  |> should.equal([2])
}

pub fn discards_even_numbers_test() {
  [1, 2, 3, 4, 5]
  |> strain.discard(int.is_even)
  |> should.equal([1, 3, 5])
}

pub fn discards_strings_starting_with_z_test() {
  ["apple", "zebra", "banana", "zombies", "cherimoya", "zelot"]
  |> strain.discard(fn(word) { string.starts_with(word, "z") })
  |> should.equal(["apple", "banana", "cherimoya"])
}

pub fn discards_lists_containing_the_number_5_test() {
  [[1, 2, 3], [5, 5, 5], [5, 1, 2], [2, 1, 2], [1, 5, 2], [2, 2, 1], [1, 2, 5]]
  |> strain.discard(fn(row) { list.any(row, fn(n) { n == 5 }) })
  |> should.equal([[1, 2, 3], [2, 1, 2], [2, 2, 1]])
}
