import darts
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn missed_target_test() {
  darts.score(x: -9.0, y: 9.0)
  |> should.equal(0)
}

pub fn on_the_outer_circle_test() {
  darts.score(x: 0.0, y: 10.0)
  |> should.equal(1)
}

pub fn on_the_middle_circle_test() {
  darts.score(x: -5.0, y: 0.0)
  |> should.equal(5)
}

pub fn on_the_inner_circle_test() {
  darts.score(x: 0.0, y: -1.0)
  |> should.equal(10)
}

pub fn exactly_on_centre_test() {
  darts.score(x: 0.0, y: 0.0)
  |> should.equal(10)
}

pub fn near_the_centre_test() {
  darts.score(x: -0.1, y: -0.1)
  |> should.equal(10)
}

pub fn just_within_the_inner_circle_test() {
  darts.score(x: 0.7, y: 0.7)
  |> should.equal(10)
}

pub fn just_outside_the_inner_circle_test() {
  darts.score(x: 0.8, y: -0.8)
  |> should.equal(5)
}

pub fn just_within_the_middle_circle_test() {
  darts.score(x: -3.5, y: 3.5)
  |> should.equal(5)
}

pub fn just_outside_the_middle_circle_test() {
  darts.score(x: -3.6, y: -3.6)
  |> should.equal(1)
}

pub fn just_within_the_outer_circle_test() {
  darts.score(x: -7.0, y: 7.0)
  |> should.equal(1)
}

pub fn just_outside_the_outer_circle_test() {
  darts.score(x: 7.1, y: -7.1)
  |> should.equal(0)
}

pub fn asymmetric_position_between_the_inner_and_middle_circles_test() {
  darts.score(x: 0.5, y: -4.0)
  |> should.equal(5)
}
