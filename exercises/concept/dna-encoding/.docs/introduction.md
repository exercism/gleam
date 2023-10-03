# Introduction

## Bit Strings

Working with binary data can be tricky, so Gleam provides a `BitString` type and accompanying syntax to construct and to pattern match on binary data.

Bit string literals are defined using the `<<>>` syntax. When defining a bit string literal, it is defined in segments. Each segment has a value and and annotation, separated by a `:`. The annotation specifies how many bits will be used to encode the value, and can be omitted completely, which will default to a 8-bit integer value.

```gleam
// This defines a bit string with three segments of a single bit each
<<0:1, 1:1, 0:1>>

// This defines a bit string with three segments of 8 bits each
<<0, 1, 0>>
```

Specifying the type as `:1` is a shorthand for writing `:size(1)`. You need to use the longer syntax if the bit size comes from a variable.

```gleam
let segment_size = 1
<<0:size(segment_size), 1:size(segment_size), 0:size(segment_size)>>
```

### Binary

When writing binary integer literals, we can write them directly in base-2 notation by prefixing the literal with `0b`. Note that they will be displayed as decimal numbers when printed in tests or in your program.

```gleam
<<0b1011:4>> == <<11:4>>
// -> True
```

### Truncating

If the value of the segment overflows the capacity of the segment's type, it will be truncated from the left.

```gleam
<<0b1011:3>> == <<0b0011:3>>
// -> True
```

### Prepending and appending

You can both prepend and append to an existing bit string using the bit string syntax. The `:bit_string` annotation must be used for the existing bit string.

```gleam
let value = <<0b110:3, 0b001:3>>
let new_value = <<0b011:3, value:bit_string, 0b000:3>>
// -> <<120, 8:size(4)>>
```

### Concatenating

We can concatenate bit strings stored in variables using the syntax. The `:bit_string` annotation must be used when concatenating two bit strings of variable sizes.

```gleam
let first = <<0b110:3>>
let second = <<0b001:3>>
let concatenated = <<first:bit_string, second:bit_string>>
// -> <<49:size(6)>>
```

### Pattern matching

Pattern matching can also be done to obtain values from the bit string. You have to know the number of bits for each fragment you want to capture, with one exception: the `:bit_string` annotation can be used to pattern match on a bit string of an unknown size, but this can only be used for the last fragment.

```gleam
let assert <<value:4, rest:bit_string>> = <<0b01101001:8>>
value == 0b0110
// -> True
```

### Inspecting bit strings

~~~~exercism/note
Bit strings might be printed in a different format than the format that was used
to create them. This often causes confusion when learning bit strings.
~~~~

By default, bit strings are displayed in fragments of 8 bits (a byte), even if you created them with fragments of a different size.

```gleam
<<2011:11>>
// -> <<251, 3:size(3)>>
```

If you create a bit string that represents a printable UTF-8 encoded string, it may displayed as a string by functions such as `io.debug`. This is due to an implementation detail of how Gleam represents strings internally.

```gleam
<<>>
// -> ""

<<65, 66, 67>>
// -> "ABC"
```
