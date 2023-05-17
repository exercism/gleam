# Hints

## General

- The `gleam/string` module has many useful [string functions][stdlib].

## 1. Get message from a log line

- The `<>` operator can be used in pattern matching to match the beginning of a string.
- The `gleam/string` module has a [function][trim] to remove surrounding whitespace from a string.

## 2. Get log level from a log line

- The `gleam/string` module has a [function][lowercase] to convert a string to lowercase.

## 3. Reformat a log line

- The `<>` operator can be used to join strings together.

[stdlib]: https://hexdocs.pm/gleam_stdlib/gleam/string.html
[trim]: https://hexdocs.pm/gleam_stdlib/gleam/string.html#trim
[lowercase]: https://hexdocs.pm/gleam_stdlib/gleam/string.html#lowercase
