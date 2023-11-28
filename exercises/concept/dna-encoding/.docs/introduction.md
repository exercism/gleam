# Introduction

## Bit Arrays

Working with binary data can be tricky, so Gleam provides a `BitArray` type and accompanying syntax to construct and to pattern match on binary data.

Bit array literals are defined using the `<<>>` syntax. When defining a bit array literal, it is defined in segments. Each segment has a value and annotation, separated by a `:`. The annotation specifies how many bits will be used to encode the value, and can be omitted completely, which will default to a 8-bit integer value.

```gleam
// This defines a bit array with three segments of a single bit each
<<0:1, 1:1, 0:1>>

// This defines a bit array with three segments of 8 bits each
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

You can both prepend and append to an existing bit array using the bit array syntax. The `:bits` annotation must be used for the existing bit array.

```gleam
let value = <<0b110:3, 0b001:3>>
let new_value = <<0b011:3, value:bits, 0b000:3>>
// -> <<120, 8:size(4)>>
```

### Concatenating

We can concatenate bit arrays stored in variables using the bit array syntax. The `:bits` annotation must be used when concatenating two bit strings of variable sizes.

```gleam
let first = <<0b110:3>>
let second = <<0b001:3>>
let concatenated = <<first:bits, second:bits>>
// -> <<49:size(6)>>
```

### Pattern matching

Pattern matching can also be done to obtain values from the bit array. You have to know the number of bits for each segment you want to capture, with one exception: the `:bits` annotation can be used to pattern match on a bit array of an unknown size, but this can only be used for the last segment.

```gleam
let assert <<value:4, rest:bits>> = <<0b01101001:8>>
value == 0b0110
// -> True
```

### Inspecting bit arrays

~~~~exercism/note
Bit arrays might be printed in a different format than the format that was used
to create them. This often causes confusion when learning bit arrays.
~~~~

By default, bit arrays are displayed in segments of 8 bits (a byte), even if you created them with segments of a different size.

```gleam
<<2011:11>>
// -> <<251, 3:size(3)>>
```

If you create a bit array that represents a printable UTF-8 encoded string, it may displayed as a string by functions such as `io.debug`. This is due to an implementation detail of how Gleam represents strings internally.

```gleam
<<>>
// -> ""

<<65, 66, 67>>
// -> "ABC"
```
