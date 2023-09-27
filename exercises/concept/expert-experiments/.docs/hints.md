# Hints

## 1. Define the `with_retry` function

- A `case` expression can be used to pattern match on a result.

## 2. Define the `record_timing` function

- The `time_logger` function should be called even if the `experiment` function returns an `Error` value.

## 3. Define the `run_experiment` function

- The [`result.try` function][result-try] can be used in a `use` expression to stop if a result is an `Error` value.

[result-try]: https://hexdocs.pm/gleam_stdlib/gleam/result.html#try
