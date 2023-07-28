# Hints


## 1. Convert Fahrenheit to Celsius

- Fahrenheit can be converted to Celsius by subtracting 32 and then dividing by 1.8.
- Division has a higher precedence than subtraction, so the subtraction must be wrapped in a block or assigned to variable to ensure it is performed first.

## 2. Compare two temperatures

- Pattern matching on the `Temperature` type can be used to extract the value from the `Celsius` and `Fahrenheit` variants.
- The `float.compare` function can be used to compare two temperatures once they have been converted to the same unit.

## 3. Sort cities by temperature

- The [`list.sort`][list-sort] function can be used to sort a list using a given comparison function.

[list-sort]: https://hexdocs.pm/gleam_stdlib/gleam/list.html#sort
