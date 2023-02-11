import gleam/int
import gleam/list
import gleam/order.{Eq, Gt, Lt}

pub type Classification {
  Perfect
  Abundant
  Deficient
}

pub type Error {
  NonPositiveInt
}

pub fn classify(number: Int) -> Result(Classification, Error) {
  case number {
    _ if number <= 0 -> Error(NonPositiveInt)
    1 -> Ok(Deficient)
    _ -> {
      let sum =
        number
        |> factors
        |> int.sum

      case int.compare(sum, number) {
        Gt -> Ok(Abundant)
        Eq -> Ok(Perfect)
        Lt -> Ok(Deficient)
      }
    }
  }
}

fn factors(number: Int) -> List(Int) {
  list.range(1, number - 1)
  |> list.filter(fn(n) { number % n == 0 })
}
