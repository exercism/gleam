# Introduction

Gleam can run on the Erlang virtual machine (BEAM), or on JavaScript runtimes. There are many other languages that use these runtimes, and it is often useful to be able to call code written in these languages from Gleam.

Gleam's _external functions_ feature permits functions in other languages to be imported into Gleam and called with no runtime overhead.

If your Gleam project runs on the Erlang virtual machine and you wish to call the `reverse` function from the Erlang `lists` module you can do it by adding the `@external` attribute to a Gleam function head like this:

```gleam
@external(erlang, "lists", "reverse")
pub fn reverse_list(x: List(a)) -> List(a)
```

This can then be called as a normal Gleam function:

```rust
let reversed = reverse_list([1, 2, 3])
// -> [3, 2, 1]
```

If you attempt to compile this code for JavaScript runtimes it will fail with an error message as there is no implementation for JavaScript. Another implementation can be specified for JavaScript runtimes like this:

```gleam
@external(erlang, "lists", "reverse")
@external(javascript, "./my_module.mjs", "reverse")
pub fn reverse_list(x: List(a)) -> List(a)
```

It is also possible to write a Gleam implementation that will be used when there is no external implementation for the current compile target:

```gleam
@external(erlang, "lists", "reverse")
pub fn reverse_list(x: List(a)) -> List(a) {
  tail_recursive_reverse(x, [])
}

fn tail_recursive_reverse(list, reversed) {
  case list {
    [] -> reversed
    [x, ..xs] -> tail_recursive_reverse(xs, [x, ..reversed])
  }
}
```
