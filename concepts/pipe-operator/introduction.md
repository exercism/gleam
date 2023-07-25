# Introduction

The `|>` operator is called the pipe operator. It can be used to chain function calls together in such a way that the value returned by the previous function call is passed to the next function call.

```gleam
"hello"
|> string.uppercase
|> string.append("?!")
// -> "HELLO?!"
```

The above code is equivalent to the following:

```gleam
string.append(string.uppercase("hello"), "?!")
```

The pipe operator will either pass the value as the first argument to the function call, or the only argument to a new call, selecting whichever would have the correct type.

```gleam
100
|> function_that_takes_two_arguments(1)

// Is equivalent to
function_that_takes_two_arguments(100, 1)
```

```gleam
100
|> function_that_returns_a_function(1)

// Is equivalent to
function_that_returns_a_function(1)(100)
```

Sometimes we want to pass the value into another position, in this case the `_` placeholder can be used to indicate where the value should be inserted.

```gleam
100
|> some_function(1, _, 2)
```
