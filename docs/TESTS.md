# Running tests

From the terminal, change to the base directory of the exercise then execute the tests with:

```bash
$ gleam test
```

This will execute the test file found in the `test` subfolder -- a file ending in `_test.gleam`

## Test functions

The tests use a [custom test framework](https://github.com/exercism/gleam-test-runner/tree/main/runner).
Any public function in the `test` directory with a name ending in `_test` will be considered a test by the test framework.

For example:

```gleam
import bob
import exercism/should

pub fn shouting_test {
  bob.hey("WATCH OUT!")
  |> should.equal("Whoa, chill out!")
}
```
