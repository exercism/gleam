import leap
import gleam/expect

pub fn year_2015_test() {
  leap.is_leap_year(2015)
  |> expect.false
}

pub fn year_1996_test() {
  leap.is_leap_year(1996)
  |> expect.true
}

pub fn year_2100_test() {
  leap.is_leap_year(2100)
  |> expect.false
}

pub fn year_2000_test() {
  leap.is_leap_year(2000)
  |> expect.true
}

pub fn year_1800_test() {
  leap.is_leap_year(1800)
  |> expect.false
}
