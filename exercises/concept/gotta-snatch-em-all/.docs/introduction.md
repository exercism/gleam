# Introduction

## Sets

A set is an immutable and unordered collection of unique values. All items in a set have to be of the same type.

The `gleam/set` module defines the `Set` type and provides functions for working with sets.

You can transform lists and sets into each other with `set.to_list` and `set.from_list`. The order of items in a set is not guaranteed.

```gleam
set.from_list([2, 3, 3, 3, 1, 1, 2])
// -> set.from_list([1, 2, 3])

set.to_list(set.from_list([2, 3, 3, 3, 1, 1, 2]))
// -> [1, 2, 3]
```

You can create and populate sets with `set.new`, `set.insert` and `set.delete`.

```gleam
set.new()
// -> set.from_list([])

let eighty_eight = set.from_list([88])

set.insert(eighty_eight, 88)
// -> set.from_list([88])

set.insert(eighty_eight, 89)
// -> set.from_list([88, 89])

set.remove(eighty_eight, 88)
// -> set.from_list([])

set.remove(eighty_eight, 89)
// -> set.from_list([88])
```

You can query the contents of a set with the functions `set.contains`, and `set.size`.

```gleam
set.contains(eighty_eight, 88)
// -> True

set.size(eighty_eight)
// -> 1
```

Set can be combined with `set.union`, and `set.intersect`.

```gleam
let a = set.from_list([1, 5, 10])
let b = set.from_list([1, 2, 3, 4, 5])

set.union(a, b)
// -> set.from_list([1, 2, 3, 4, 5, 10])

set.intersection(a, b)
// -> set.from_list([1, 5])
```

You can filter sets.

```gleam
let is_small = fn(n) { x <= 3 }

set.filter(b, is_small)
// -> set.from_list([1, 2, 3])
```
