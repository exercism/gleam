# Introduction

Maps in Gleam are the data structure for storing information in key-value pairs. In other languages, these might also be known as associative arrays, hashes, or dictionaries.

Any type can be used for the keys and values in a map, and they do not guarantee the order of their entries when accessed or returned.

## Working with maps

Maps are created and manipulated using functions from the `gleam/map` module.

```gleam
// Create an empty map
let map1 = map.new()

// Create a map with some values
let map2 = map.from_list([
  #("name", "Gleam"),
  #("colour", "Pink"),
])

// Add a value to a map
let map3 = map.insert(map2, "website", "https://gleam.run")

// Get a value from a map
let name = map.get(map3, "name")
```
