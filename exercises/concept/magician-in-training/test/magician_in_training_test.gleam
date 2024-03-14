import exercism/test_runner
import gleam/queue
import magician_in_training

pub fn main() {
  test_runner.main()
}

fn is_equal(a, b) {
  queue.is_logically_equal(a, b, fn(x, y) { x == y })
}

pub fn insert_top_test() {
  let assert True =
    queue.from_list([5, 9, 7, 1])
    |> magician_in_training.insert_top(8)
    |> is_equal(queue.from_list([5, 9, 7, 1, 8]))
}

pub fn remove_top_card_with_empty_stack_test() {
  let assert True =
    queue.from_list([])
    |> magician_in_training.remove_top_card
    |> is_equal(queue.from_list([]))
}

pub fn remove_top_card_test() {
  let assert True =
    queue.from_list([5, 9, 7, 1])
    |> magician_in_training.remove_top_card
    |> is_equal(queue.from_list([5, 9, 7]))
}

pub fn insert_bottom_test() {
  let assert True =
    queue.from_list([5, 9, 7, 1])
    |> magician_in_training.insert_bottom(8)
    |> is_equal(queue.from_list([8, 5, 9, 7, 1]))
}

pub fn remove_bottom_card_with_empty_stack_test() {
  let assert True =
    queue.from_list([])
    |> magician_in_training.remove_bottom_card
    |> is_equal(queue.from_list([]))
}

pub fn remove_bottom_card_test() {
  let assert True =
    queue.from_list([5, 9, 7, 1])
    |> magician_in_training.remove_bottom_card
    |> is_equal(queue.from_list([9, 7, 1]))
}

pub fn check_size_of_stack_not_matching_test() {
  let assert False =
    queue.from_list([5, 9, 7, 1])
    |> magician_in_training.check_size_of_stack(5)
}

pub fn check_size_of_stack_matching_test() {
  let assert True =
    queue.from_list([5, 9, 7, 1])
    |> magician_in_training.check_size_of_stack(4)
}
