# Hints

## 1. Extract errors

- Errors can be pattern matched on using the `Error(value)` pattern syntax.

## 2. Remove team prefix

- String prefixes can be match on using the `"prefix" <> rest` pattern syntax.

## 3. Split region and team

- Strings can be split on a character using the [`string.split` function][split].
- Lists can be match on using the `[a, b, c]` pattern syntax.

[split]: https://hexdocs.pm/gleam_stdlib/gleam/string.html#split
