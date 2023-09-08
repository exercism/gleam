# Hints

## 1. Define the `Usd`, `Eur`, and `Jpy` types

- The `Usd`, `Eur`, and `Jpy` types can be defined without constructors using the `pub type TypeName` syntax.

## 2. Define the `Money` type.

- The `Money` type should have a phantom type parameter for the currency.

## 3. Define `dollar`, `euro`, and `yen` functions

- The return annotation of the `dollar`, `euro`, and `yen` functions should be `Money(currency)` where `currency` is the appropriate type parameter of the `Money` type.

## 4. Define the `total` function

- The return annotation of the `total` function should use the same type parameter as the argument.
