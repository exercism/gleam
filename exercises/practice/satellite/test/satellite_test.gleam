import exercism/should
import exercism/test_runner
import satellite

pub fn main() {
  test_runner.main()
}

pub fn empty_tree_test() {
  satellite.tree_from_traversals(inorder: [], preorder: [])
  |> should.equal(Ok(CustomRecordType()))
}

pub fn tree_with_one_item_test() {
  satellite.tree_from_traversals(inorder: ["a"], preorder: ["a"])
  |> should.equal(
    Ok(CustomRecordType(l: CustomRecordType(), r: CustomRecordType(), v: "a")),
  )
}

pub fn tree_with_many_items_test() {
  satellite.tree_from_traversals(inorder: ["i", "a", "f", "x", "r"], preorder: [
    "a", "i", "x", "f", "r",
  ])
  |> should.equal(
    Ok(CustomRecordType(
      l: CustomRecordType(l: CustomRecordType(), r: CustomRecordType(), v: "i"),
      r: CustomRecordType(
        l: CustomRecordType(
          l: CustomRecordType(),
          r: CustomRecordType(),
          v: "f",
        ),
        r: CustomRecordType(
          l: CustomRecordType(),
          r: CustomRecordType(),
          v: "r",
        ),
        v: "x",
      ),
      v: "a",
    )),
  )
}

pub fn reject_traversals_of_different_length_test() {
  satellite.tree_from_traversals(inorder: ["b", "a", "r"], preorder: ["a", "b"])
  |> should.equal(Error("traversals must have the same length"))
}

pub fn reject_inconsistent_traversals_of_same_length_test() {
  satellite.tree_from_traversals(inorder: ["a", "b", "c"], preorder: [
    "x", "y", "z",
  ])
  |> should.equal(Error("traversals must have the same elements"))
}

pub fn reject_traversals_with_repeated_items_test() {
  satellite.tree_from_traversals(inorder: ["b", "a", "a"], preorder: [
    "a", "b", "a",
  ])
  |> should.equal(Error("traversals must contain unique items"))
}

pub fn a_degenerate_binary_tree_test() {
  satellite.tree_from_traversals(inorder: ["d", "c", "b", "a"], preorder: [
    "a", "b", "c", "d",
  ])
  |> should.equal(
    Ok(CustomRecordType(
      l: CustomRecordType(
        l: CustomRecordType(
          l: CustomRecordType(
            l: CustomRecordType(),
            r: CustomRecordType(),
            v: "d",
          ),
          r: CustomRecordType(),
          v: "c",
        ),
        r: CustomRecordType(),
        v: "b",
      ),
      r: CustomRecordType(),
      v: "a",
    )),
  )
}

pub fn another_degenerate_binary_tree_test() {
  satellite.tree_from_traversals(inorder: ["a", "b", "c", "d"], preorder: [
    "a", "b", "c", "d",
  ])
  |> should.equal(
    Ok(CustomRecordType(
      l: CustomRecordType(),
      r: CustomRecordType(
        l: CustomRecordType(),
        r: CustomRecordType(
          l: CustomRecordType(),
          r: CustomRecordType(
            l: CustomRecordType(),
            r: CustomRecordType(),
            v: "d",
          ),
          v: "c",
        ),
        v: "b",
      ),
      v: "a",
    )),
  )
}

pub fn tree_with_many_more_items_test() {
  satellite.tree_from_traversals(
    inorder: ["g", "d", "h", "b", "a", "e", "c", "i", "f"],
    preorder: ["a", "b", "d", "g", "h", "c", "e", "f", "i"],
  )
  |> should.equal(
    Ok(CustomRecordType(
      l: CustomRecordType(
        l: CustomRecordType(
          l: CustomRecordType(
            l: CustomRecordType(),
            r: CustomRecordType(),
            v: "g",
          ),
          r: CustomRecordType(
            l: CustomRecordType(),
            r: CustomRecordType(),
            v: "h",
          ),
          v: "d",
        ),
        r: CustomRecordType(),
        v: "b",
      ),
      r: CustomRecordType(
        l: CustomRecordType(
          l: CustomRecordType(),
          r: CustomRecordType(),
          v: "e",
        ),
        r: CustomRecordType(
          l: CustomRecordType(
            l: CustomRecordType(),
            r: CustomRecordType(),
            v: "i",
          ),
          r: CustomRecordType(),
          v: "f",
        ),
        v: "c",
      ),
      v: "a",
    )),
  )
}
