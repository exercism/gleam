import circular_buffer
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn reading_an_empty_buffer_should_fail_test() {
  let buffer = circular_buffer.new(1)
  let assert Error(Nil) = circular_buffer.read(buffer)
}

pub fn can_read_an_item_just_written_test() {
  let buffer = circular_buffer.new(1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(#(1, _)) = circular_buffer.read(buffer)
}

pub fn each_item_may_only_be_read_once_test() {
  let buffer = circular_buffer.new(1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(#(1, buffer)) = circular_buffer.read(buffer)
  let assert Error(Nil) = circular_buffer.read(buffer)
}

pub fn items_are_read_in_the_order_they_are_written_test() {
  let buffer = circular_buffer.new(2)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 2)
  let assert Ok(#(1, buffer)) = circular_buffer.read(buffer)
  let assert Ok(#(2, _)) = circular_buffer.read(buffer)
}

pub fn full_buffer_cant_be_written_to_test() {
  let buffer = circular_buffer.new(1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Error(Nil) = circular_buffer.write(buffer, 2)
}

pub fn a_read_frees_up_capacity_for_another_write_test() {
  let buffer = circular_buffer.new(1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(#(1, buffer)) = circular_buffer.read(buffer)
  let assert Ok(buffer) = circular_buffer.write(buffer, 2)
  let assert Ok(#(2, _)) = circular_buffer.read(buffer)
}

pub fn read_position_is_maintained_even_across_multiple_writes_test() {
  let buffer = circular_buffer.new(3)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 2)
  let assert Ok(#(1, buffer)) = circular_buffer.read(buffer)
  let assert Ok(buffer) = circular_buffer.write(buffer, 3)
  let assert Ok(#(2, buffer)) = circular_buffer.read(buffer)
  let assert Ok(#(3, _)) = circular_buffer.read(buffer)
}

pub fn items_cleared_out_of_buffer_cant_be_read_test() {
  let buffer = circular_buffer.new(1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let buffer = circular_buffer.clear(buffer)
  let assert Error(Nil) = circular_buffer.read(buffer)
}

pub fn clear_frees_up_capacity_for_another_write_test() {
  let buffer = circular_buffer.new(1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let buffer = circular_buffer.clear(buffer)
  let assert Ok(buffer) = circular_buffer.write(buffer, 2)
  let assert Ok(#(2, _)) = circular_buffer.read(buffer)
}

pub fn clear_does_nothing_on_empty_buffer_test() {
  let buffer = circular_buffer.new(1)
  let buffer = circular_buffer.clear(buffer)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(#(1, _)) = circular_buffer.read(buffer)
}

pub fn overwrite_acts_like_write_on_non_full_buffer_test() {
  let buffer = circular_buffer.new(2)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let buffer = circular_buffer.overwrite(buffer, 2)
  let assert Ok(#(1, buffer)) = circular_buffer.read(buffer)
  let assert Ok(#(2, _)) = circular_buffer.read(buffer)
}

pub fn overwrite_replaces_the_oldest_item_on_full_buffer_test() {
  let buffer = circular_buffer.new(2)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 2)
  let buffer = circular_buffer.overwrite(buffer, 3)
  let assert Ok(#(2, buffer)) = circular_buffer.read(buffer)
  let assert Ok(#(3, _)) = circular_buffer.read(buffer)
}

pub fn overwrite_replaces_the_oldest_item_remaining_in_buffer_following_a_read_test() {
  let buffer = circular_buffer.new(3)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 2)
  let assert Ok(buffer) = circular_buffer.write(buffer, 3)
  let assert Ok(#(1, buffer)) = circular_buffer.read(buffer)
  let assert Ok(buffer) = circular_buffer.write(buffer, 4)
  let buffer = circular_buffer.overwrite(buffer, 5)
  let assert Ok(#(3, buffer)) = circular_buffer.read(buffer)
  let assert Ok(#(4, buffer)) = circular_buffer.read(buffer)
  let assert Ok(#(5, _)) = circular_buffer.read(buffer)
}

pub fn initial_clear_does_not_affect_wrapping_around_test() {
  let buffer = circular_buffer.new(2)
  let buffer = circular_buffer.clear(buffer)
  let assert Ok(buffer) = circular_buffer.write(buffer, 1)
  let assert Ok(buffer) = circular_buffer.write(buffer, 2)
  let buffer = circular_buffer.overwrite(buffer, 3)
  let buffer = circular_buffer.overwrite(buffer, 4)
  let assert Ok(#(3, buffer)) = circular_buffer.read(buffer)
  let assert Ok(#(4, buffer)) = circular_buffer.read(buffer)
  let assert Error(_) = circular_buffer.read(buffer)
}
