import exercism/should
import exercism/test_runner
import flatten_array.{List, Null, Value}

pub fn main() {
  test_runner.main()
}

pub fn empty_test() {
  flatten_array.flatten(List([]))
  |> should.equal([])
}

pub fn no_nesting_test() {
  flatten_array.flatten(List([Value(0), Value(1), Value(2)]))
  |> should.equal([0, 1, 2])
}

pub fn flattens_a_nested_array_test() {
  flatten_array.flatten(List([List([List([])])]))
  |> should.equal([])
}

pub fn flattens_array_with_just_integers_present_test() {
  flatten_array.flatten(
    List([
      Value(1),
      List([Value(2), Value(3), Value(4), Value(5), Value(6), Value(7)]),
      Value(8),
    ]),
  )
  |> should.equal([1, 2, 3, 4, 5, 6, 7, 8])
}

pub fn five_level_nesting_test() {
  flatten_array.flatten(
    List([
      Value(0),
      Value(2),
      List([
        List([Value(2), Value(3)]),
        Value(8),
        Value(100),
        Value(4),
        List([List([List([Value(50)])])]),
      ]),
      Value(-2),
    ]),
  )
  |> should.equal([0, 2, 2, 3, 8, 100, 4, 50, -2])
}

pub fn six_level_nesting_test() {
  flatten_array.flatten(
    List([
      Value(1),
      List([
        Value(2),
        List([List([Value(3)])]),
        List([Value(4), List([List([Value(5)])])]),
        Value(6),
        Value(7),
      ]),
      Value(8),
    ]),
  )
  |> should.equal([1, 2, 3, 4, 5, 6, 7, 8])
}

pub fn null_values_are_omitted_from_the_final_result_test() {
  flatten_array.flatten(List([Value(1), Value(2), Null]))
  |> should.equal([1, 2])
}

pub fn consecutive_null_values_at_the_front_of_the_list_are_omitted_from_the_final_result_test() {
  flatten_array.flatten(List([Null, Null, Value(3)]))
  |> should.equal([3])
}

pub fn consecutive_null_values_in_the_middle_of_the_list_are_omitted_from_the_final_result_test() {
  flatten_array.flatten(List([Value(1), Null, Null, Value(4)]))
  |> should.equal([1, 4])
}

pub fn six_level_nest_list_with_null_values_test() {
  flatten_array.flatten(
    List([
      Value(0),
      Value(2),
      List([
        List([Value(2), Value(3)]),
        Value(8),
        List([List([Value(100)])]),
        Null,
        List([List([Null])]),
      ]),
      Value(-2),
    ]),
  )
  |> should.equal([0, 2, 2, 3, 8, 100, -2])
}

pub fn all_values_in_nested_list_are_null_test() {
  flatten_array.flatten(
    List([
      Null,
      List([List([List([Null])])]),
      Null,
      Null,
      List([List([Null, Null]), Null]),
      Null,
    ]),
  )
  |> should.equal([])
}
