import erlang_extraction.{type GbTree}
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

@external(erlang, "gb_trees", "to_list")
fn to_list(tree: GbTree(k, v)) -> List(#(k, v))

pub fn new_gb_tree_test() {
  erlang_extraction.new_gb_tree()
  |> to_list
  |> should.equal([])
}

pub fn insert_int_string_test() {
  erlang_extraction.new_gb_tree()
  |> erlang_extraction.insert(1, "one")
  |> erlang_extraction.insert(2, "two")
  |> erlang_extraction.insert(3, "three")
  |> to_list
  |> should.equal([#(1, "one"), #(2, "two"), #(3, "three")])
}

pub fn insert_string_int_test() {
  erlang_extraction.new_gb_tree()
  |> erlang_extraction.insert("one", 1)
  |> erlang_extraction.insert("two", 2)
  |> erlang_extraction.insert("three", 3)
  |> to_list
  |> should.equal([#("one", 1), #("three", 3), #("two", 2)])
}

pub fn delete_test() {
  erlang_extraction.new_gb_tree()
  |> erlang_extraction.insert(1, "one")
  |> erlang_extraction.insert(2, "two")
  |> erlang_extraction.insert(3, "three")
  |> erlang_extraction.delete(2)
  |> to_list
  |> should.equal([#(1, "one"), #(3, "three")])
}

pub fn delete_non_existing_test() {
  erlang_extraction.new_gb_tree()
  |> erlang_extraction.insert(1, "one")
  |> erlang_extraction.insert(2, "two")
  |> erlang_extraction.insert(3, "three")
  |> erlang_extraction.delete(4)
  |> to_list
  |> should.equal([#(1, "one"), #(2, "two"), #(3, "three")])
}
