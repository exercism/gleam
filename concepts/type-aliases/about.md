# About

## Type aliases

A type alias can be used in Gleam to give a convenient name for an existing type that would be otherwise cumbersome to write.

```gleam
pub type Headers =
  List(#(String, String))

pub fn html_headers() -> Headers {
  [
    #("content-type", "text/html"),
    #("x-frame-options", "DENY"),
  ]
}
```

When written with `pub type` the alias can be used outside of the module it is defined in. If `pub` is omitted then the alias is private and cannot be referenced in other modules.
