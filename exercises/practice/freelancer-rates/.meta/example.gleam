import gleam/float
import gleam/int

pub fn daily_rate(hourly_rate: Int) -> Float {
  int.to_float(hourly_rate) *. 8.0
}

pub fn apply_discount(before_discount: Int, discount: Float) -> Float {
  let float_discount = int.to_float(before_discount)
  float_discount -. float_discount *. { discount /. 100.0 }
}

pub fn monthly_rate(hourly_rate: Int, discount: Float) -> Int {
  let before_discount = float.round(daily_rate(hourly_rate) *. 22.0)
  let after_discount = apply_discount(before_discount, discount)
  float.truncate(float.ceiling(after_discount))
}

pub fn days_in_budget(budget: Int, hourly_rate: Int, discount: Float) -> Float {
  float.floor(
    int.to_float(budget)
    /. apply_discount(float.round(daily_rate(hourly_rate)), discount),
  )
}
