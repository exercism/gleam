import exercism/test_runner
import gleam/float
import space_age.{Earth, Jupiter, Mars, Mercury, Neptune, Saturn, Uranus, Venus}

pub fn main() {
  test_runner.main()
}

pub fn age_on_earth_test() {
  let assert True =
    space_age.age(Earth, 1_000_000_000.0)
    |> float.loosely_equals(with: 31.69, tolerating: 0.01)
}

pub fn age_on_mercury_test() {
  let assert True =
    space_age.age(Mercury, 2_134_835_688.0)
    |> float.loosely_equals(with: 280.88, tolerating: 0.01)
}

pub fn age_on_venus_test() {
  let assert True =
    space_age.age(Venus, 189_839_836.0)
    |> float.loosely_equals(with: 9.78, tolerating: 0.01)
}

pub fn age_on_mars_test() {
  let assert True =
    space_age.age(Mars, 2_129_871_239.0)
    |> float.loosely_equals(with: 35.88, tolerating: 0.01)
}

pub fn age_on_jupiter_test() {
  let assert True =
    space_age.age(Jupiter, 901_876_382.0)
    |> float.loosely_equals(with: 2.41, tolerating: 0.01)
}

pub fn age_on_saturn_test() {
  let assert True =
    space_age.age(Saturn, 2_000_000_000.0)
    |> float.loosely_equals(with: 2.15, tolerating: 0.01)
}

pub fn age_on_uranus_test() {
  let assert True =
    space_age.age(Uranus, 1_210_123_456.0)
    |> float.loosely_equals(with: 0.46, tolerating: 0.01)
}

pub fn age_on_neptune_test() {
  let assert True =
    space_age.age(Neptune, 1_821_023_456.0)
    |> float.loosely_equals(with: 0.35, tolerating: 0.01)
}
