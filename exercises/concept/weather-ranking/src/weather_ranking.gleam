import gleam/order.{type Order}

pub type City {
  City(name: String, temperature: Temperature)
}

pub type Temperature {
  Celsius(Float)
  Fahrenheit(Float)
}

pub fn fahrenheit_to_celsius(f: Float) -> Float {
  todo
}

pub fn compare_temperature(left: Temperature, right: Temperature) -> Order {
  todo
}

pub fn sort_cities_by_temperature(cities: List(City)) -> List(City) {
  todo
}
