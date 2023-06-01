# Hints

## General

- Browse the [functions available in the _string module_][string-module-functions] to discover which operations on strings Gleam's standard library offers.

## 1. Get the name's first letter

- There is a [function][string-first] to get the first character from a string.
- There are multiple [functions][string-trim] to remove leading, trailing, or leading and trailing whitespaces from a string.

## 2. Format the first letter as an initial

- There is a [function][string-upcase] to convert all characters in a string to their uppercase variant.
- The `<>` operator can be used to concatenate strings.

## 3. Split the full name into the first name and the last name

- There is a [function][string-split] that splits a string on whitespace characters.
- A few first elements of a list can be assigned to variables by pattern matching on the list.

## 4. Put the initials inside of the heart

- The `<>` operator can be used to concatenate strings.

[string-module-functions]: https://hexdocs.pm/gleam_stdlib/gleam/string.html
[string-first]: https://hexdocs.pm/gleam_stdlib/gleam/string.html#first
[string-trim]: https://hexdocs.pm/gleam_stdlib/gleam/string.html#trim
[string-upcase]: https://hexdocs.pm/gleam_stdlib/gleam/string.html#uppercase
[string-split]: https://hexdocs.pm/gleam_stdlib/gleam/string.html#split
