# About

Phantom types are types are type parameters of a custom type that are not used in any of the value constructors of that type.

That's a little abstract, so here is an example:

```gleam
pub type Length(unit) {
  Length(amount: Float)
}
```

In this example the `unit` type parameter is not used in the `Length` value constructor, so `unit` is a phantom type.

This unused type parameter may seem useless, but it can be used to add further restrictions on how `Length` values can be used.

For example, we could have a function `double`, which multiplies the length. It works with lengths of any unit, so the type parameter is a generic type variable.

```gleam
// This function accepts all Length values
pub fn double(length: Length(unit)) -> Length(unit) {
  Length(length.amount *. 2.0)
}
```

We could also have a function `add_inch`, which only works if the length is in inches.

```gleam
// A unit type for inches. It is never constructed so we don't
// define any constructors for it.
pub type Inches

pub fn add_inch(length: Length(Inches)) -> Length(Inches) {
  Length(length.amount +. 1.0)
}
```

The `add_inch` function will not accept lengths of any other unit parameter, the phantom type has been used to ensure only the correct unit is used.

A function can also be written to ensure that two length values are of the same unit, by using the same type variable for both.

```gleam
pub fn add(a: Length(unit), b: Length(unit)) -> Length(unit) {
  Length(a.amount +. b.amount)
}
```

Phantom types can work well with opaque types. If other modules cannot construct `Length` values then we can ensure they are not constructed with an invalid unit type, and that only the functions defined above can be used with them.
