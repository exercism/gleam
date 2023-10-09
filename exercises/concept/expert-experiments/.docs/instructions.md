# Instructions

Daphne has been working on a system to run and record the results of her experiments. Some of the code has become a bit verbose and repetitive, so she's asked you to write some `use` expressions to help clean it up.

## 1. Define the `with_retry` function

Sometimes experiments can fail due to a one-off mistake, so if an experiment fails Daphne wants to retry it again to see if it works the second time.

Define the `with_retry` function that takes a result returning function as an argument.

If the function returns an `Ok` value then `with_retry` should return that value.

If the function returns an `Error` value then `with_retry` should call the function again and return the result of that call.

Daphne will use the function like this:

```gleam
pub fn main() {
  use <- with_retry
  // Perform the experiment here
}
```

## 2. Define the `record_timing` function

Daphne records how long each experiment takes to run by calling a time logging function before and after each experiment.

Define the `record_timing` function that takes two arguments:
- A time logging function which takes no arguments and returns `Nil`.
- An experiment function which takes no arguments and returns a result.

`record_timing` should call the time logging function, then call the experiment function, then call the time logging function again, and finally return the result of the experiment function.

Daphne will use the function like this:

```gleam
pub fn main() {
  use <- record_timing(time_logger)
  // Perform the experiment here
}
```

## 3. Define the `run_experiment` function

Experiments are made up of three phases. The setup, the action, and the recording. All three phases return results, and each phase needs the successful result of the previous phase to run.

Define the `run_experiment` function that takes four arguments:
- The name of the experiment as a `String`.
- A setup function which takes no arguments and returns a result.
- An action function which takes the `Ok` value of the setup function as an argument and returns a result.
- A recording function which takes the `Ok` value of the setup and functions as an arguments and returns a result.

If all three functions succeed then `run_experiment` should return `Ok(#(experiment_name, recording_data))`.

If any of the functions return an `Error` value then `run_experiment` should return that value.

Daphne will use the function like this:

```gleam
pub fn main() {
  use setup_data, action_data <- run_experiment("Test 1", setup, action)
  // Record the results here
}
```
