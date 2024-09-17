import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/task
import gleam/regex
import gleam/string

fn increment_count(existing: Option(Int)) -> Int {
  case existing {
    None -> 1
    Some(value) -> value + 1
  }
}

fn count_graphemes(text: String, acc: Dict(String, Int)) -> Dict(String, Int) {
  case string.pop_grapheme(text) {
    Ok(#(next, tail)) -> {
      let assert Ok(re) = regex.from_string("[0-9\\s,\\p{P}]")
      case regex.check(re, next) {
        True -> count_graphemes(tail, acc)
        False ->
          count_graphemes(
            tail,
            dict.update(acc, string.lowercase(next), increment_count),
          )
      }
    }
    Error(Nil) -> acc
  }
}

fn add_counts(acc: Dict(String, Int), elems: Dict(String, Int)) {
  dict.fold(over: elems, from: acc, with: fn(acc, grapheme, count) {
    dict.update(acc, grapheme, fn(ex) {
      case ex {
        None -> count
        Some(value) -> value + count
      }
    })
  })
}

pub fn calculate_frequencies(texts: List(String)) -> Dict(String, Int) {
  list.map(texts, fn(text) {
    task.async(fn() { count_graphemes(text, dict.new()) })
  })
  |> list.map(fn(task) { task.await(task, 10_000) })
  |> list.fold(from: dict.new(), with: add_counts)
}
