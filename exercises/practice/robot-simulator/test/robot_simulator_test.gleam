import gleeunit
import gleeunit/should
import robot_simulator.{East, North, Position, Robot, South, West}

pub fn main() {
  gleeunit.main()
}

pub fn create_robot_at_origin_facing_north_test() {
  robot_simulator.create(direction: North, position: Position(x: 0, y: 0))
  |> should.equal(Robot(direction: North, position: Position(x: 0, y: 0)))
}

pub fn create_robot_at_negative_position_facing_south_test() {
  robot_simulator.create(direction: South, position: Position(x: -1, y: -1))
  |> should.equal(Robot(direction: South, position: Position(x: -1, y: -1)))
}

pub fn rotating_clockwise_changes_north_to_east_test() {
  robot_simulator.move(
    direction: North,
    instructions: "R",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: East, position: Position(x: 0, y: 0)))
}

pub fn rotating_clockwise_changes_east_to_south_test() {
  robot_simulator.move(
    direction: East,
    instructions: "R",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: South, position: Position(x: 0, y: 0)))
}

pub fn rotating_clockwise_changes_south_to_west_test() {
  robot_simulator.move(
    direction: South,
    instructions: "R",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: West, position: Position(x: 0, y: 0)))
}

pub fn rotating_clockwise_changes_west_to_north_test() {
  robot_simulator.move(
    direction: West,
    instructions: "R",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: North, position: Position(x: 0, y: 0)))
}

pub fn rotating_counter_clockwise_changes_north_to_west_test() {
  robot_simulator.move(
    direction: North,
    instructions: "L",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: West, position: Position(x: 0, y: 0)))
}

pub fn rotating_counter_clockwise_changes_west_to_south_test() {
  robot_simulator.move(
    direction: West,
    instructions: "L",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: South, position: Position(x: 0, y: 0)))
}

pub fn rotating_counter_clockwise_changes_south_to_east_test() {
  robot_simulator.move(
    direction: South,
    instructions: "L",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: East, position: Position(x: 0, y: 0)))
}

pub fn rotating_counter_clockwise_changes_east_to_north_test() {
  robot_simulator.move(
    direction: East,
    instructions: "L",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: North, position: Position(x: 0, y: 0)))
}

pub fn moving_forward_one_facing_north_increments_y_test() {
  robot_simulator.move(
    direction: North,
    instructions: "A",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: North, position: Position(x: 0, y: 1)))
}

pub fn moving_forward_one_facing_south_decrements_y_test() {
  robot_simulator.move(
    direction: South,
    instructions: "A",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: South, position: Position(x: 0, y: -1)))
}

pub fn moving_forward_one_facing_east_increments_x_test() {
  robot_simulator.move(
    direction: East,
    instructions: "A",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: East, position: Position(x: 1, y: 0)))
}

pub fn moving_forward_one_facing_west_decrements_x_test() {
  robot_simulator.move(
    direction: West,
    instructions: "A",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: West, position: Position(x: -1, y: 0)))
}

pub fn follow_series_of_instructions_moving_east_and_north_from_readme_test() {
  robot_simulator.move(
    direction: North,
    instructions: "RAALAL",
    position: Position(x: 7, y: 3),
  )
  |> should.equal(Robot(direction: West, position: Position(x: 9, y: 4)))
}

pub fn follow_series_of_instructions_moving_west_and_north_test() {
  robot_simulator.move(
    direction: North,
    instructions: "LAAARALA",
    position: Position(x: 0, y: 0),
  )
  |> should.equal(Robot(direction: West, position: Position(x: -4, y: 1)))
}

pub fn follow_series_of_instructions_moving_west_and_south_test() {
  robot_simulator.move(
    direction: East,
    instructions: "RRAAAAALA",
    position: Position(x: 2, y: -7),
  )
  |> should.equal(Robot(direction: South, position: Position(x: -3, y: -8)))
}

pub fn follow_series_of_instructions_moving_east_and_north_test() {
  robot_simulator.move(
    direction: South,
    instructions: "LAAARRRALLLL",
    position: Position(x: 8, y: 4),
  )
  |> should.equal(Robot(direction: North, position: Position(x: 11, y: 5)))
}
