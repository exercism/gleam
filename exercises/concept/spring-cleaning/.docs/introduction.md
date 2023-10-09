# Introduction

## Let Assertions

When pattern matching on a value in Gleam we have to ensure the patterns match every possible value of the type, otherwise the code won't compile.

This function should be rejected as it has not handled case where the list is empty:

```gleam
pub fn get_first_element_or_crash(items: List(e)) -> e {
  case items {
    [item, ..] -> item
  }
}
```

Most of the time in Gleam programs we want to handle every possible case, but in some programs such as prototypes and quick scripts we may want to focus only on the happy path and not write code for other cases. This is where Gleam's `let assert` comes in.

With let assertions a pattern can match only some of the possible values of a type, and if the pattern does not match a value the program will crash. Using `let assert` we can write the above function like this:

```gleam
pub fn get_first_element_or_crash(items: List(e)) -> e {
  let assert [item, ..] = items
  item
}
```

Gleam libraries should never use let assertions as they can crash the program. Instead, they should use the `Result` type and let the application developer decide how to handle errors.
