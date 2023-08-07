# Hints

## 1. Get all wines of a given color

- Keyword arguments are specified by writing a name before the argument name, separated by a space.
- The [`list.filter`][list-filter] function can be used to select elements from a list that match a given predicate.

## 2. Get all wines of a given color bottled in a given year

- Keyword arguments are specified by writing a name before the argument name, separated by a space.
- The [`list.filter`][list-filter] function can be used to select elements from a list that match a given predicate.

## 3. Get all wines of a given color bottled in a given country

- This function can be implemented by calling `wines_from_country` and then `wines_of_color` on the output.

[list-filter]: https://hexdocs.pm/gleam_stdlib/gleam/list.html#filter
