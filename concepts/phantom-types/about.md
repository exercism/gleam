# About

Phantom types are types are type parameters of a custom type that are not used in any of the value constructors of that type.

That's a little abstract, so here is an example:

```gleam
pub type Money(currency) {
  Money(quantity: Float)
}
```

In this example the `currency` type parameter is not used in the `Money` value constructor, so it is a phantom type.

This unused type parameter may seem useless, but it can be used to add further restrictions on how `Money` values can be used.

For example, we could have a function `apply_interest`, which multiplies the amount of money. It works with money of any currency, so the type parameter is a generic type variable.

```gleam
// This function accepts all Money values
pub fn apply_interest(money: Money(currency)) -> Money(currency) {
  Money(money.quantity *. 1.1)
}
```

We could also have a function `buy_tea`, which only works if the money is in British pounds.

```gleam
// A type for the British currency. It is never constructed so we don't
// define any constructors for it.
pub type PoundsSterling

pub fn buy_tea(money: Money(PoundsSterling)) -> Money(PoundsSterling) {
  Money(money.quantity -. 1.5)
}
```

The `buy_tea` function will not accept money of any other currency parameter, the phantom type has been used to ensure only the correct currency is used.

A function can also be written to ensure that two money values are of the same currency, by using the same type variable for both.

```gleam
pub fn add(a: Money(currency), b: Money(currency)) -> Money(currency) {
  Money(a.quantity +. b.quantity)
}
```

Phantom types can work well with opaque types. If other modules cannot construct `Money` values then we can ensure they are not constructed with an invalid currency type, and that only the functions defined above can be used with them.
