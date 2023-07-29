# Introduction

Constants in Gleam are a way to give names to values. They are defined using the `const` keyword.

```gleam
pub const pi: Float = 3.14159

pub fn circle_area(radius: Float) -> Float {
  radius *. radius *. pi
}
```

When defined with the `pub` keyword module constants can be accessed from other modules, otherwise they can only be accessed from within the module they are defined in.
