# Introduction

Dicts in Gleam are the data structure for storing information in key-value pairs. In other languages, these might also be known as associative arrays, hashes, or dictionaries.

Any type can be used for the keys and values in a dict, and they do not guarantee the order of their entries when accessed or returned.

## Working with dicts

Dicts are created and manipulated using functions from the `gleam/dict` module.

```gleam
// Create an empty dict
let dict1 = dict.new()

// Create a dict with some values
let dict2 = dict.from_list([
  #("name", "Gleam"),
  #("colour", "Pink"),
])

// Add a value to a dict
let dict3 = dict.insert(dict2, "website", "https://gleam.run")

// Get a value from a dict
let name = dict.get(dict3, "name")
// -> Ok("Gleam")
```
