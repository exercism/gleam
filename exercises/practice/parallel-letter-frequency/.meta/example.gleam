import gleam/function
import gleam/string
import gleam/regex
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/task.{Task}
import gleam/map.{Map}

pub fn calculate_frequencies(texts: List(String)) -> Map(String, Int) {
  texts
  |> list.map(calculate_frequency)
  |> list.map(task.await(_, 100))
  |> list.fold(from: map.new(), with: merge_frequencies)
}

fn calculate_frequency(text: String) -> Task(Map(String, Int)) {
  task.async(fn() {
    text
    |> letters()
    |> list.group(function.identity)
    |> map.map_values(fn(_, occurences) { list.length(occurences) })
  })
}

fn letters(text: String) -> List(String) {
  assert Ok(re) = regex.from_string("\\p{L}")

  text
  |> string.lowercase()
  |> regex.scan(re, _)
  |> list.map(fn(m) { m.content })
}

fn merge_frequencies(
  a: Map(String, Int),
  b: Map(String, Int),
) -> Map(String, Int) {
  map.fold(
    over: b,
    from: a,
    with: fn(frequencies, letter, frequency) {
      map.update(
        in: frequencies,
        update: letter,
        with: fn(current_opt) {
          case current_opt {
            Some(current) -> current + frequency
            None -> frequency
          }
        },
      )
    },
  )
}
