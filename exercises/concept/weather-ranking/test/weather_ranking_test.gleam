import exercism/should
import exercism/test_runner
import gleam/order
import weather_ranking.{Celsius, City, Fahrenheit}

pub fn main() {
  test_runner.main()
}

pub fn fahrenheit_to_celsius_freezing_test() {
  32.0
  |> weather_ranking.fahrenheit_to_celsius
  |> should_approximately_equal(0.0)
}

pub fn fahrenheit_to_celsius_body_equal_test() {
  98.6
  |> weather_ranking.fahrenheit_to_celsius
  |> should_approximately_equal(37.0)
}

pub fn compare_temperature_both_celsius_test() {
  weather_ranking.compare_temperature(Celsius(27.3), Celsius(31.8))
  |> should.equal(order.Lt)
}

pub fn compare_temperature_both_fahrenheit_test() {
  weather_ranking.compare_temperature(Fahrenheit(88.1), Fahrenheit(83.2))
  |> should.equal(order.Gt)
}

pub fn compare_temperature_equal_test() {
  weather_ranking.compare_temperature(Celsius(31.1), Celsius(31.1))
  |> should.equal(order.Eq)
}

pub fn sort_cities_by_temperature_test() {
  [
    City("Barcelona", Celsius(31.8)),
    City("Delhi", Celsius(41.0)),
    City("Manchester", Celsius(27.3)),
    City("Provo", Fahrenheit(88.1)),
  ]
  |> weather_ranking.sort_cities_by_temperature
  |> should.equal([
    City("Manchester", Celsius(27.3)),
    City("Provo", Fahrenheit(88.1)),
    City("Barcelona", Celsius(31.8)),
    City("Delhi", Celsius(41.0)),
  ])
}

fn should_approximately_equal(value: Float, expected: Float) -> Float {
  case value <. expected +. 0.01 && value >. expected -. 0.01 {
    True -> value
    False -> should.equal(value, expected)
  }
}
