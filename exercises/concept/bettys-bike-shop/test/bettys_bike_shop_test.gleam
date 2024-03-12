import bettys_bike_shop
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn pence_to_pounds_599_is_5_99_pounds_test() {
  bettys_bike_shop.pence_to_pounds(599)
  |> should.equal(5.99)
}

pub fn pence_to_pounds_33_is_0_33_pounds_test() {
  bettys_bike_shop.pence_to_pounds(33)
  |> should.equal(0.33)
}

pub fn pounds_to_string_5_99_test() {
  bettys_bike_shop.pounds_to_string(5.99)
  |> should.equal("£5.99")
}

pub fn pounds_to_string_0_33_test() {
  bettys_bike_shop.pounds_to_string(0.33)
  |> should.equal("£0.33")
}
