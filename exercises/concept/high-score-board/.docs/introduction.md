# Introduction

## Type Aliases

### Type aliases

A type alias can use used in Gleam to give a convenient name for an existing type that would be otherwise cumbersome to write.

```gleam
pub type Headers =
  List(#(String, String))

pub fn html_headers() -> Headers {
  [
    #("content-type", "text/html"),
    #("x-frame-options", "DENY"),
  ]
}
```

When written with `pub type` the alias can be used outside of the module it is defined in. If `pub` is omitted then the alias is private and cannot be referenced in other modules.
## Maps

Maps in Gleam are the data structure for storing information in key-value pairs. In other languages, these might also be known as associative arrays, hashes, or dictionaries.

Any type can be used for the keys and values in a map, and they do not guarantee the order of their entries when accessed or returned.

### Working with maps

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
