import dominoes.{can_chain}
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn empty_input_empty_output_test() {
  let assert True = can_chain([])
}

pub fn singleton_input_singleton_output_test() {
  let assert True = can_chain([#(1, 1)])
}

pub fn singleton_that_cant_be_chained_test() {
  let assert False = can_chain([#(1, 2)])
}

pub fn three_elements_test() {
  let assert True = can_chain([#(1, 2), #(3, 1), #(2, 3)])
}

pub fn can_reverse_dominoes_test() {
  let assert True = can_chain([#(1, 2), #(1, 3), #(2, 3)])
}

pub fn cant_be_chained_test() {
  let assert False = can_chain([#(1, 2), #(4, 1), #(2, 3)])
}

pub fn disconnected_simple_test() {
  let assert False = can_chain([#(1, 1), #(2, 2)])
}

pub fn disconnected_double_loop_test() {
  let assert False = can_chain([#(1, 2), #(2, 1), #(3, 4), #(4, 3)])
}

pub fn disconnected_single_isolated_test() {
  let assert False = can_chain([#(1, 2), #(2, 3), #(3, 1), #(4, 4)])
}

pub fn need_backtrack_test() {
  let assert True = can_chain([#(1, 2), #(2, 3), #(3, 1), #(2, 4), #(2, 4)])
}

pub fn separate_loops_test() {
  let assert True =
    can_chain([#(1, 2), #(2, 3), #(3, 1), #(1, 1), #(2, 2), #(3, 3)])
}

pub fn nine_elements_test() {
  let assert True =
    can_chain([
      #(1, 2),
      #(5, 3),
      #(3, 1),
      #(1, 2),
      #(2, 4),
      #(1, 6),
      #(2, 3),
      #(3, 4),
      #(5, 6),
    ])
}

pub fn separate_three_domino_loops_test() {
  let assert False =
    can_chain([#(1, 2), #(2, 3), #(3, 1), #(4, 5), #(5, 6), #(6, 4)])
}
