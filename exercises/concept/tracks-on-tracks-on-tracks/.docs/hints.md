# Hints

## 1. Create a new list

- An empty list can be created using the `[]` syntax.

## 2. Define an existing list

- Lists with multiple elements can be created using the `["a", "b", "c"]` syntax.

## 3. Add a new language to a list

- The spread syntax (`[x, ..list]`) can be used to add an element to the beginning of a list.

## 4. Count the languages in the list

- There is a function in the `gleam/list` module to [count the elements in a list][length].

## 5. Reverse the list

- There is a function in the `gleam/list` module to [reverse a list][reverse].

## 6. Check if list is exciting

- You can use pattern matching using the list and list spread patterns to match on specific list structures.

[reverse]: https://hexdocs.pm/gleam_stdlib/gleam/list.html#reverse
[length]: https://hexdocs.pm/gleam_stdlib/gleam/list.html#length
