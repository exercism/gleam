import freelancer_rates
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn daily_rate_test() {
  freelancer_rates.daily_rate(60)
  |> should.equal(480.0)
}

pub fn apply_discount_test() {
  freelancer_rates.apply_discount(150, 10.0)
  |> should.equal(135.0)
}

pub fn monthly_rate_test() {
  freelancer_rates.monthly_rate(77, 10.5)
  |> should.equal(12_130)
}

pub fn days_in_budget_test() {
  freelancer_rates.days_in_budget(20_000, 80, 11.0)
  |> should.equal(35.0)
}
