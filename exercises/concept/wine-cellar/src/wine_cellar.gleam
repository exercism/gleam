pub fn wines_of_color(wines: List(Wine), color: Color) -> List(Wine) {
  todo
}

pub fn wines_from_country(wines: List(Wine), country: String) -> List(Wine) {
  todo
}

// Please define the required labelled arguments for this function
pub fn filter(wines: List(Wine)) -> List(Wine) {
  todo
}

pub type Wine {
  Wine(name: String, year: Int, country: String, color: Color)
}

pub type Color {
  Red
  Rose
  White
}
