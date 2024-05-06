# Hints

## 1. Define the `GbTree` external type

- External types can be defined using the `pub type TypeName(a, b, c)` syntax.

## 2. Define the `new_gb_tree` function

- External functions can be defined using the `@external(erlang, "module", "function")` syntax.
- The function should use the [`gb_trees:empty/0`][gb_trees_empty].

## 3. Define the `insert` function

- The external function can be a private function that is called by the `insert` function in order to change the order of the arguments.
- The function should use the [`gb_trees:insert/3`][gb_trees_insert].

## 4. Define the `delete` function

- The external function can be a private function that is called by the `delete` function in order to change the order of the arguments.
- The function should use the [`gb_trees:delete_any/2`][gb_trees_delete_any].
- The `gb_trees:delete/2` function will crash if the key is not found, so it should not be used.

[gb_trees_empty]: https://www.erlang.org/doc/man/gb_trees#empty-0
[gb_trees_insert]: https://www.erlang.org/doc/man/gb_trees#insert-3
[gb_trees_delete_any]: https://www.erlang.org/doc/man/gb_trees#delete_any-2
