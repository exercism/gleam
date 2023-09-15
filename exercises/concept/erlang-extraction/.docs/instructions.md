# Instructions

Tadhg has found the perfect Erlang library to help with is project. Being an older Erlang library it is using the `gb_trees` module rather than the newer `maps` module for storing data.

Help out Tadgh by creating external types and functions for working with `gb_trees` in Gleam.

## 1. Define the `GbTree` external type

The `GbTree` type should have two type parameters, a key type and a value type. It should have no constructors, making it an external type.

## 2. Define the `new_gb_tree` function

The `new_gb_tree` function should take no arguments and return an empty `GbTree`.

It should use the `gb_trees:empty/0` function from the Erlang standard library.

## 3. Define the `insert` function

The `insert` function should take a `GbTree` and a key and value to insert into the tree. It should return a new `GbTree` with the key and value inserted.

The should take three arguments:
1. The `GbTree` to insert into.
2. The key to insert.
3. The value to insert.

It should use the `gb_trees:insert/3` function from the Erlang standard library.

## 4. Define the `delete` function

The `delete` function should take a `GbTree` and a key to delete from the tree. It should return a new `GbTree` with the key and value deleted.

The should take two arguments:
1. The `GbTree` to delete from.
2. The key to delete.

It should use the `gb_trees:delete_any/2` function from the Erlang standard library.
