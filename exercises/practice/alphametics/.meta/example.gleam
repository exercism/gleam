import gleam/map.{Map}
import gleam/string
import gleam/list
import gleam/int
import gleam/option
import gleam/set.{Set}

pub fn solve(puzzle: String) -> Result(Map(String, Int), Nil) {
  let #(coefficients, leading_digits) = parse_puzzle(puzzle)
  let letters = map.keys(coefficients)
  let values =
    list.range(0, 9)
    |> set.from_list
  let guess = map.new()
  solve_puzzle(letters, values, guess, coefficients, leading_digits)
}

fn parse_puzzle(puzzle: String) -> #(Map(String, Int), Set(String)) {
  assert [left, right] = string.split(puzzle, " == ")
  let terms = string.split(left, " + ")

  let coefficients =
    terms
    |> list.fold(
      from: map.new(),
      with: fn(coefficients, term) { add_term(coefficients, term, int.add) },
    )
    |> add_term(right, int.subtract)

  let leading_digits =
    [right, ..terms]
    |> list.map(string.slice(_, at_index: 0, length: 1))
    |> set.from_list

  #(coefficients, leading_digits)
}

fn add_term(
  coefficients: Map(String, Int),
  term: String,
  operator: fn(Int, Int) -> Int,
) -> Map(String, Int) {
  term
  |> string.to_graphemes
  |> list.reverse
  |> list.index_fold(
    from: coefficients,
    with: fn(coefficients, letter, index) {
      map.update(
        coefficients,
        letter,
        fn(maybe_coeff) {
          operator(option.unwrap(maybe_coeff, 0), pow(10, index))
        },
      )
    },
  )
}

fn pow(base: Int, exp: Int) -> Int {
  case exp {
    _ if exp <= 0 -> 1
    _ -> base * pow(base, exp - 1)
  }
}

fn solve_puzzle(
  letters_to_assign: List(String),
  possible_values: Set(Int),
  current_guess: Map(String, Int),
  coefficients: Map(String, Int),
  leading_digits: Set(String),
) -> Result(Map(String, Int), Nil) {
  case letters_to_assign {
    [] -> is_solution(current_guess, coefficients)
    [letter, ..rest] ->
      possible_values
      |> maybe_remove_zero(letter, leading_digits)
      |> set.to_list
      |> list.fold_until(
        from: Error(Nil),
        with: fn(_, value) {
          let possible_values = set.delete(possible_values, value)
          let current_guess = map.insert(current_guess, letter, value)
          case
            solve_puzzle(
              rest,
              possible_values,
              current_guess,
              coefficients,
              leading_digits,
            )
          {
            Ok(solution) -> list.Stop(Ok(solution))
            Error(Nil) -> list.Continue(Error(Nil))
          }
        },
      )
  }
}

fn maybe_remove_zero(
  values: Set(Int),
  letter: String,
  leading_digits: Set(String),
) -> Set(Int) {
  case set.contains(leading_digits, letter) {
    True -> set.delete(values, 0)
    False -> values
  }
}

fn is_solution(
  current_guess: Map(String, Int),
  coefficients: Map(String, Int),
) -> Result(Map(String, Int), Nil) {
  let sum =
    map.fold(
      over: current_guess,
      from: 0,
      with: fn(sum, letter, value) {
        assert Ok(coeff) = map.get(coefficients, letter)
        sum + coeff * value
      },
    )
  case sum == 0 {
    True -> Ok(current_guess)
    False -> Error(Nil)
  }
}
