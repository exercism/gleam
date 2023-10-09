import exercism/test_runner
import gleam/int
import gleam/list
import gleam/string
import strain

pub fn main() {
  test_runner.main()
}

pub fn keep_on_empty_list_returns_empty_list_test() {
  let assert [] = strain.keep([], fn(_) { True })
}

pub fn keeps_everything_test() {
  let assert [1, 2, 3] = strain.keep([1, 2, 3], fn(_) { True })
}

pub fn keeps_nothing_test() {
  let assert [] = strain.keep([1, 2, 3], fn(_) { False })
}

pub fn keeps_first_and_last_test() {
  let assert [1, 3] = strain.keep([1, 2, 3], int.is_odd)
}

pub fn keeps_neither_first_nor_last_test() {
  let assert [2] = strain.keep([1, 2, 3], int.is_even)
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

pub fn discard_on_empty_list_returns_empty_list_test() {
  let assert [] = strain.discard([], fn(_) { True })
}

pub fn discards_everything_test() {
  let assert [] = strain.discard([1, 2, 3], fn(_) { True })
}

pub fn discards_nothing_test() {
  let assert [1, 2, 3] = strain.discard([1, 2, 3], fn(_) { False })
}

pub fn discards_first_and_last_test() {
  let assert [2] = strain.discard([1, 2, 3], int.is_odd)
}

pub fn discards_neither_first_nor_last_test() {
  let assert [1, 3] = strain.discard([1, 2, 3], int.is_even)
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
