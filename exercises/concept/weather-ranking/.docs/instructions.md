# Instructions

Tori is a climate journalist and she wants to write an article about the weather in different cities. She has data about the weather in different cities and she wants to rank them based on their temperature, but some of the temperatures are in Fahrenheit and some are in Celsius. She needs your help to convert the temperatures to the same unit and rank them.

## 1. Convert Fahrenheit to Celsius

Define the `fahrenheit_to_celsius` function that takes a temperature in Fahrenheit as an argument and returns the temperature in Celsius.

To convert Fahrenheit to Celsius subtract 32 from the Fahrenheit value, and then divide the result by 1.8.

```gleam
fahrenheit_to_celsius(72.5)
// -> 22.5
```

## 2. Compare two temperatures

Define the `compare_temperature` function that takes two temperatures and returns an `Order` value indicating whether the first temperature is less than, equal to, or greater than the second temperature.

```gleam
compare_temperature(Celsius(30.5), Fahrenheit(82.1))
// -> Gt
```

## 3. Sort cities by temperature

Define the `sort_cities_by_temperature` function that takes a list of cities and returns them from coldest to warmest.

```gleam
sort_cities_by_temperature([
  #("London", Celsius(30.5)),
  #("Paris", Fahrenheit(82.1))
])
// -> [
//   #("Paris", Fahrenheit(82.1)),
//   #("London", Celsius(30.5))
// ]
```
