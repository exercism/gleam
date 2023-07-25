# Introduction

Gleam doesn't use exceptions for error handling, instead the generic `Result` type is used to represent the output of a function that can either succeed or fail.

The `Result` type is built into the language so you don't need to define or import it, but if you were to define it yourself it would look like this:

```gleam
pub type Result(value, error) {
  Ok(value)
  Error(error)
}
```

The `Ok` variant is returned when a function succeeds, and the `Error` variant is returned when a function fails.

Results are very common in Gleam, and Gleam programmers will commonly use the [`gleam/result` module](https://hexdocs.pm/gleam_stdlib/gleam/result.html) to make working with them easier.

// TODO: show some examples of the result module in use

// TODO: exercise based off of Elm's one

