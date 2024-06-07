# Debug

When a test fails, a message is displayed describing what went wrong and for which input.
You can inspect arbitrary values in your program by passing them to the `debug` method (available at `exercism/test_runner`).
This will capture the values and show you the output.


```gleam
import exercism/test_runner.{ debug }

let value: String = "test"
debug("The value is " <> value)
```