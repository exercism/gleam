pub type Planet {
  Mercury
  Venus
  Earth
  Mars
  Jupiter
  Saturn
  Uranus
  Neptune
}

pub fn age(planet: Planet, seconds: Float) -> Float {
  seconds /. orbital_period_in_earth_years(planet) /. seconds_in_earth_year
}

fn orbital_period_in_earth_years(planet: Planet) -> Float {
  case planet {
    Mercury -> 0.2408467
    Venus -> 0.61519726
    Earth -> 1.0
    Mars -> 1.8808158
    Jupiter -> 11.862615
    Saturn -> 29.447498
    Uranus -> 84.016846
    Neptune -> 164.79132
  }
}

const seconds_in_earth_year = 31_557_600.0
