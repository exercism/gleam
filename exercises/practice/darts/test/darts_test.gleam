import darts
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn missed_target_test() {
  darts.score(-9.0, 9.0)
  |> should.equal(0)
}

pub fn on_the_outer_circle_test() {
  darts.score(0.0, 10.0)
  |> should.equal(1)
}

pub fn on_the_middle_circle_test() {
  darts.score(-5.0, 0.0)
  |> should.equal(5)
}

pub fn on_the_inner_circle_test() {
  darts.score(0.0, -1.0)
  |> should.equal(10)
}

pub fn exactly_on_centre_test() {
  darts.score(0.0, 0.0)
  |> should.equal(10)
}

pub fn near_the_centre_test() {
  darts.score(-0.1, -0.1)
  |> should.equal(10)
}

pub fn just_within_the_inner_circle_test() {
  darts.score(0.7, 0.7)
  |> should.equal(10)
}

pub fn just_outside_the_inner_circle_test() {
  darts.score(0.8, -0.8)
  |> should.equal(5)
}

pub fn just_within_the_middle_circle_test() {
  darts.score(-3.5, 3.5)
  |> should.equal(5)
}

pub fn just_outside_the_middle_circle_test() {
  darts.score(-3.6, -3.6)
  |> should.equal(1)
}

pub fn just_within_the_outer_circle_test() {
  darts.score(-7.0, 7.0)
  |> should.equal(1)
}

pub fn just_outside_the_outer_circle_test() {
  darts.score(7.1, -7.1)
  |> should.equal(0)
}

pub fn asymmetric_position_between_the_inner_and_middle_circles_test() {
  darts.score(0.5, -4.0)
  |> should.equal(5)
}
