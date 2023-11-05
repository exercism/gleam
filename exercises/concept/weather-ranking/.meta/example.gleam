import gleam/order.{type Order}
import gleam/float
import gleam/list

pub type City {
  City(name: String, temperature: Temperature)
}

pub type Temperature {
  Celsius(Float)
  Fahrenheit(Float)
}

pub fn fahrenheit_to_celsius(f: Float) -> Float {
  { f -. 32.0 } /. 1.8
}

fn convert(temperature: Temperature) -> Float {
  case temperature {
    Celsius(c) -> c
    Fahrenheit(f) -> fahrenheit_to_celsius(f)
  }
}

pub fn compare_temperature(left: Temperature, right: Temperature) -> Order {
  float.compare(convert(left), convert(right))
}

pub fn sort_cities_by_temperature(cities: List(City)) -> List(City) {
  cities
  |> list.sort(fn(city_a, city_b) {
    compare_temperature(city_a.temperature, city_b.temperature)
  })
}
