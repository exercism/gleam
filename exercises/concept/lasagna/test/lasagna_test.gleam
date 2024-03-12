import exercism/should
import exercism/test_runner
import lasagna

pub fn main() {
  test_runner.main()
}

pub fn expected_minutes_in_oven_test() {
  lasagna.expected_minutes_in_oven()
  |> should.equal(40)
}

pub fn remaining_minutes_in_oven_test() {
  lasagna.remaining_minutes_in_oven(25)
  |> should.equal(15)
}

pub fn preparation_time_in_minutes_for_one_layer_test() {
  lasagna.preparation_time_in_minutes(1)
  |> should.equal(2)
}

pub fn preparation_time_in_minutes_for_multiple_layers_test() {
  lasagna.preparation_time_in_minutes(4)
  |> should.equal(8)
}

pub fn total_time_in_minutes_for_one_layer_test() {
  lasagna.total_time_in_minutes(1, 30)
  |> should.equal(32)
}

pub fn total_time_in_minutes_for_multiple_layers_test() {
  lasagna.total_time_in_minutes(4, 8)
  |> should.equal(16)
}

pub fn notification_message_test() {
  lasagna.alarm()
  |> should.equal("Ding!")
}
