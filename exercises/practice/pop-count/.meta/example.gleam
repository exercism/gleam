import gleam/int
import gleam/result

pub fn egg_count(number: Int) -> Int {
  recurse(number, 0)
}

fn recurse(number: Int, eggs: Int) -> Int {
  case number {
    0 -> eggs
    _ -> {
      let new_number = number / 2
      case int.is_odd(number) {
        True -> recurse(new_number, eggs + 1)
        _ -> recurse(new_number, eggs)
      }
    }
  }
}
