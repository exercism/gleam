import gleam/float

pub fn daily_rate(hourly_rate: Int) -> Float {
  hourly_rate * 8.0
}

pub fn apply_discount(before_discount: Int, discount: Float) -> Float {
  before_discount - (before_discount * (discount / 100))
}

pub fn monthly_rate(hourly_rate: Int, discount: Float) -> Float {
  let before_discount = daily_rate(hourly_rate) * 22
    let after_discount = apply_discount(before_discount, discount)
    float.truncate(float.ceiling(after_discount))
}

pub fn days_in_budget(budget: Int, hourly_rate: Int, discount: Float) -> Float {
     float.floor( budget / apply_discount(daily_rate(hourly_rate), discount), 1)
}