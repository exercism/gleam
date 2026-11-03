import exercism/should
import exercism/test_runner
import robot_simulator

// Some tests have two expectations: one for the position, one for the direction
// Optionally, you can also test
//  - An invalid direction throws an error
//  - An invalid instruction throws an error
//  - Default starting position and direction if none are provided

pub fn main() {
  test_runner.main()
}

pub fn create_robot_at_origin_facing_north_test() {
  robot_simulator.create("north", CustomRecordType(x: 0, y: 0))
  |> should.equal(CustomRecordType(
    direction: "north",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn create_robot_at_negative_position_facing_south_test() {
  robot_simulator.create("south", CustomRecordType(x: -1, y: -1))
  |> should.equal(CustomRecordType(
    direction: "south",
    position: CustomRecordType(x: -1, y: -1),
  ))
}

pub fn rotating_clockwise_changes_north_to_east_test() {
  robot_simulator.move(
    direction: "north",
    instructions: "R",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "east",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn rotating_clockwise_changes_east_to_south_test() {
  robot_simulator.move(
    direction: "east",
    instructions: "R",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "south",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn rotating_clockwise_changes_south_to_west_test() {
  robot_simulator.move(
    direction: "south",
    instructions: "R",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "west",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn rotating_clockwise_changes_west_to_north_test() {
  robot_simulator.move(
    direction: "west",
    instructions: "R",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "north",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn rotating_counter_clockwise_changes_north_to_west_test() {
  robot_simulator.move(
    direction: "north",
    instructions: "L",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "west",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn rotating_counter_clockwise_changes_west_to_south_test() {
  robot_simulator.move(
    direction: "west",
    instructions: "L",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "south",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn rotating_counter_clockwise_changes_south_to_east_test() {
  robot_simulator.move(
    direction: "south",
    instructions: "L",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "east",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn rotating_counter_clockwise_changes_east_to_north_test() {
  robot_simulator.move(
    direction: "east",
    instructions: "L",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "north",
    position: CustomRecordType(x: 0, y: 0),
  ))
}

pub fn moving_forward_one_facing_north_increments_y_test() {
  robot_simulator.move(
    direction: "north",
    instructions: "A",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "north",
    position: CustomRecordType(x: 0, y: 1),
  ))
}

pub fn moving_forward_one_facing_south_decrements_y_test() {
  robot_simulator.move(
    direction: "south",
    instructions: "A",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "south",
    position: CustomRecordType(x: 0, y: -1),
  ))
}

pub fn moving_forward_one_facing_east_increments_x_test() {
  robot_simulator.move(
    direction: "east",
    instructions: "A",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "east",
    position: CustomRecordType(x: 1, y: 0),
  ))
}

pub fn moving_forward_one_facing_west_decrements_x_test() {
  robot_simulator.move(
    direction: "west",
    instructions: "A",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "west",
    position: CustomRecordType(x: -1, y: 0),
  ))
}

// The robot can follow a series of instructions and end up with the correct position and direction.
// Where R = Turn Right, L = Turn Left and A = Advance

pub fn follow_series_of_instructions_moving_east_and_north_from_readme_test() {
  robot_simulator.move(
    direction: "north",
    instructions: "RAALAL",
    position: CustomRecordType(x: 7, y: 3),
  )
  |> should.equal(CustomRecordType(
    direction: "west",
    position: CustomRecordType(x: 9, y: 4),
  ))
}

pub fn follow_series_of_instructions_moving_west_and_north_test() {
  robot_simulator.move(
    direction: "north",
    instructions: "LAAARALA",
    position: CustomRecordType(x: 0, y: 0),
  )
  |> should.equal(CustomRecordType(
    direction: "west",
    position: CustomRecordType(x: -4, y: 1),
  ))
}

pub fn follow_series_of_instructions_moving_west_and_south_test() {
  robot_simulator.move(
    direction: "east",
    instructions: "RRAAAAALA",
    position: CustomRecordType(x: 2, y: -7),
  )
  |> should.equal(CustomRecordType(
    direction: "south",
    position: CustomRecordType(x: -3, y: -8),
  ))
}

pub fn follow_series_of_instructions_moving_east_and_north_test() {
  robot_simulator.move(
    direction: "south",
    instructions: "LAAARRRALLLL",
    position: CustomRecordType(x: 8, y: 4),
  )
  |> should.equal(CustomRecordType(
    direction: "north",
    position: CustomRecordType(x: 11, y: 5),
  ))
}
