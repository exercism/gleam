# Instructions

In this exercise, you've been tasked with writing the software for an encryption device that works by performing transformations on data. You need a way to flexibly create complicated functions by combining simpler functions together.

For each task, return an anonymous function that can be invoked from the calling scope.

## 1. Create an adder

Implement `secret_add`. It should return a function which takes one argument and adds to it the argument passed in to `secret_add`.

```gleam
let adder = secret_add(2)
adder(2)
// -> 4
```

## 2. Create a subtractor

Implement `secret_subtract`. It should return a function which takes one argument and subtracts the secret passed in to `secret_subtract` from that argument.

```gleam
let subtractor = secret_subtract(2)
subtractor(3)
// -> 1
```

## 3. Create a multiplier

Implement `secret_multiply`. It should return a function which takes one argument and multiplies it by the secret passed in to `secret_multiply`.

```gleam
let multiplier = secret_multiply(7)
multiplier(3)
// -> 21
```

## 4. Create a divider

Implement `secret_divide`. It should return a function which takes one argument and divides it by the secret passed in to `secret_divide`.

```gleam
let divider = secret_divide(3)
divider(32)
// -> 10
```

## 5. Create a function combiner

Implement `secret_combine`. It should return a function which takes one argument and applies to it the two functions passed in to `secret_combine` in order.

```gleam
let multiply = secret_multiply(7)
let divide = secret_divide(3)
let combined = secret_combine(multiply, divide)

combined(6)
// -> 14
```
