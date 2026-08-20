# Instructions

Creating a zipper for a binary tree.

[Zippers][zipper] are a purely functional way of navigating within a data structure and manipulating it.
They essentially contain a data structure and a pointer into that data structure (called the focus).

For example given a tree a zipper might support these operations:

- `to_zipper` (get a zipper out of a tree, the focus is on the root node)
- `to_tree` (get the tree out of the zipper)
- `left` (move the focus to the left child of the focus)
- `right` (move the focus to right child of the focus)
- `up` (move the focus to the parent, returns a new zipper)
- `set_value` (set the value of the focused node, returns a new zipper)
- `set_left` (set the left child of the focused node, returns a new zipper)
- `set_right` (set the right child of the focused node, returns a new zipper)

[zipper]: https://en.wikipedia.org/wiki/Zipper_%28data_structure%29
