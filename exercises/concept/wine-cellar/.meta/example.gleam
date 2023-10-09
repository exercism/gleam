import gleam/list

pub fn wines_of_color(wines: List(Wine), color color: Color) -> List(Wine) {
  wines
  |> list.filter(fn(wine) { wine.color == color })
}

pub fn wines_from_country(
  wines: List(Wine),
  country country: String,
) -> List(Wine) {
  wines
  |> list.filter(fn(wine) { wine.country == country })
}

pub fn filter(
  wines: List(Wine),
  color color: Color,
  country country: String,
) -> List(Wine) {
  wines
  |> wines_of_color(color)
  |> wines_from_country(country)
}

pub type Wine {
  Wine(name: String, year: Int, country: String, color: Color)
}

pub type Color {
  Red
  Rose
  White
}
