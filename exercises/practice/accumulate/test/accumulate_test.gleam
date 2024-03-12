import accumulate
import exercism/should
import exercism/test_runner
import gleam/string

pub fn main() {
  test_runner.main()
}

pub fn empty_accumulation_test() {
  accumulate.accumulate([], fn(x) { x * x })
  |> should.equal([])
}

pub fn accumulate_squares_test() {
  accumulate.accumulate([1, 2, 3], fn(x) { x * x })
  |> should.equal([1, 4, 9])
}

pub fn accumulate_upcases_test() {
  accumulate.accumulate(["hello", "world"], string.uppercase)
  |> should.equal(["HELLO", "WORLD"])
}

pub fn accumulate_reversed_strings_test() {
  accumulate.accumulate(["the", "quick", "brown", "fox", "etc"], string.reverse)
  |> should.equal(["eht", "kciuq", "nworb", "xof", "cte"])
}

pub fn accumulate_recursively_test() {
  accumulate.accumulate(["a", "b", "c"], fn(x) -> List(String) {
    accumulate.accumulate(["1", "2", "3"], fn(y) -> String {
      string.append(x, y)
    })
  })
  |> should.equal([["a1", "a2", "a3"], ["b1", "b2", "b3"], ["c1", "c2", "c3"]])
}
