# Introduction

Strings in Gleam are immutable text surrounded by double quotes. They support unicode characters, and can be multi-line.

```gleam
let greeting = "Hello, Joe! 📞"

let multi_line_string = "one
two
three"
```

Strings can be joined together using the `<>` operator:

```gleam
let name = "Mike"
"Hello, " <> name <> "!"
// -> "Hello, Mike!"
```

The [`gleam/string`][stdlib] module in the standard library provides functions for working with strings.

Strings can be matched upon in case expressions:

```gleam
pub fn on_an_excellent_adventure(name: String) -> Bool {
  case name {
    "Bill" -> True
    "Ted" -> True
    _ -> False
  }
}
```

If you want to match on the beginning of a string and assign the rest to a variable you can use the `<>` operator pattern:

```gleam
pub fn profession(name: String) -> String {
  case name {
    "Dr " <> rest -> rest <> " is a doctor"
    _ -> "I'm not sure what " <> name <> " does"
  }
}
```

[stdlib]: https://hexdocs.pm/gleam_stdlib/gleam/string.html
