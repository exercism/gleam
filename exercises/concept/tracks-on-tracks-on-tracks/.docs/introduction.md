# Introduction

## Lists

A list in Gleam is an immutable collection of zero or more values. The values in a list must all have the same type. As lists are immutable, once a list has been constructed, its value can never change. Any functions/operators that appear to modify a list (such as adding an element), will actually return a new list.

Lists can be defined as follows:

```gleam
let empty = []
let singleValue = [5]
let threeValues = ["a", "b", "c"]
```

The most common way to add an element to a list is through the spread syntax:

```gleam
let two_to_four = [2, 3, 4]
let one_to_four = [1, ..two_to_four]
// -> [1, 2, 3, 4]
```

The [`gleam/list`](https://hexdocs.pm/gleam_stdlib/gleam/list.html) module in the Gleam standard library contains many useful functions for working with lists. This module is very commonly used in Gleam code so it is good to be familiar with it.

Lists patterns can be used in case expressions to match on lists and assign contained values to variables:

```gleam
pub fn describe(list: List(String)) -> String {
  case list {
    [] -> "Empty list"
    [x] -> "List with one item: " <> x
    [x, y] -> "List with two items: " <> x <> " and " <> y
    _ -> "List with three or more items"
  }
}
```

As well as matching on exact length lists, the spread syntax can be used to match on lists of at-least a certain length:

```gleam
pub fn describe(list: List(String)) -> String {
  case list {
    [_, _, ..] -> "List with at least two items"
    [_] -> "List with one item"
    [] -> "Empty list"
  }
}
```

The spread syntax can also be used to assign the rest of the list to a variable:

```gleam
pub fn remove_first_item(list: List(String)) -> List(String) {
  case list {
    // Return the list without the first item
    [_, ..rest] -> rest

    // There's no first item to remove, return an empty list
    _ -> []
  }
}
```

Case expressions should have a pattern for every possible value of the type being matched on, so a final discard pattern (`_`) is often used to handle any remaining values.
