# Instructions

In this exercise you're working at a pizza place that delivers to customers.

You offer three types of pizzas:

- Margherita: \$7
- Caprese: \$9
- Formaggio: \$10

Customers can also choose two additional options for a small additional fee:

1. Extra sauce: \$1
1. Extra toppings: \$2

When customers place an order, an additional fee is added if they only order one or two pizzas:

- 1 pizza: \$3
- 2 pizzas: \$2

You have three tasks, each of which will work with pizzas and their price.

## 1. Define the pizza types and options

Define the `Pizza` custom type to represent the different types of pizzas and options:

- `Margherita`
- `Caprese`
- `Formaggio`
- `ExtraSauce`
- `ExtraToppings`

## 2. Calculate the price of pizza

Implement the `pizza_price` function to calculate a pizza's price:

```gleam
pizza_price(Caprese)
// -> 9
```

## 3. Calculate the price of an order

Implement the `order_price` function to calculate a pizza order's price:

```gleam
order_price([Margherita, Formaggio])
// -> 19
```
