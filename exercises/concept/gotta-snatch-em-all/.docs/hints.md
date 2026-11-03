# Hints

## General

- Read the documentation for [gleam/set][set]
- The documentation for [gleam/list][list] might be useful too

## 1. Start a collection

- The most appropriate function is [`set.from_list`][from_list]

## 2. Grow the collection

- Use the [`set.insert` function][insert]
- Use the [`set.contains` function][contains]

## 3. Start trading

- Use the [`set.contains` function][contains]
- Use the [`set.insert` function][insert]
- Use the [`set.delete` function][delete]

## 4. There can be only one of each

- A `Set` has the property of only holding unique values
- Use the [`set.from_list` function][from_list]
- Use the [`set.to_list` function][to_list]
- `set.to_list` is not guaranteed to return a sorted list

## 5. Cards they all have

- Use the [`set.intersection` function][intersection]
- Use the [`set.to_list` function][to_list]

## 6. All of the cards

- Use [`set.new`][new] as accumulator
- Use the [`set.union` function][union]
- Use the [`set.size` function][size]

## 7. Shiny for the win

- Use the [`set.filter` function][filter]


[set]: https://hexdocs.pm/gleam_stdlib/gleam/set.html
[list]: https://hexdocs.pm/gleam_stdlib/gleam/list.html
[from_list]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#from_list
[insert]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#insert
[contains]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#contains
[delete]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#delete
[to_list]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#to_list
[filter]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#filter
[new]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#new
[union]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#union
[size]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#size
[intersection]: https://hexdocs.pm/gleam_stdlib/gleam/set.html#intersection
