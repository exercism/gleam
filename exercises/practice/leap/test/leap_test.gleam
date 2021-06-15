import leap
import gleam/should

pub fn year_2015_test() {
  leap.is_leap_year(2015)
  |> should.be_false
}

pub fn year_1996_test() {
  leap.is_leap_year(1996)
  |> should.be_true
}

pub fn year_2100_test() {
  leap.is_leap_year(2100)
  |> should.be_false
}

pub fn year_2000_test() {
  leap.is_leap_year(2000)
  |> should.be_true
}

pub fn year_1800_test() {
  leap.is_leap_year(1800)
  |> should.be_false
}
