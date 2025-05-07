# Instructions

Alizé runs an online fashion boutique. The big annual sale is coming up, so she wants to create some functionality to help take stock of the inventory.

A single item in the inventory is represented using a custom type.

```gleam
Item(
  name: "White Shirt",
  price: 40,
  quantity: 6,
)
```

## 1. Return a list of the names of the items

Implement `item_names` function, which takes an iterator of items and returns an iterator of their names in the same order.

```gleam
[
  Item(price: 65, name: "Maxi Brown Dress", quantity: 8),
  Item(price: 50, name: "Red Short Skirt", quantity: 0),
  Item(price: 29, name: "Black Short Skirt", quantity: 4),
  Item(price: 20, name: "Bamboo Socks Cats", quantity: 7),
]
|> iterator.from_list
|> item_names
|> iterator.to_list

// -> ["Maxi Brown Dress", "Red Short Skirt", "Black Short Skirt", "Bamboo Socks Cats"]
```

## 2. Return any items that are cheap

Implement the `cheap` function, which takes an iterator of items and returns an iterator of items that cost less than 30.

```gleam
[
  Item(price: 65, name: "Maxi Brown Dress", quantity: 8),
  Item(price: 50, name: "Red Short Skirt", quantity: 0),
  Item(price: 29, name: "Black Short Skirt", quantity: 4),
  Item(price: 20, name: "Bamboo Socks Cats", quantity: 7),
]
|> iterator.from_list
|> cheap
|> iterator.to_list

// -> [
//   Item(price: 29, name: "Black Short Skirt", quantity: 4),
//   Item(price: 20, name: "Bamboo Socks Cats", quantity: 7),
// ]
```

## 3. Return any items that are out of stock

Implement the `out_of_stock` function which returns any items that have no stock.

```gleam
[
  Item(price: 65, name: "Maxi Brown Dress", quantity: 8),
  Item(price: 50, name: "Red Short Skirt", quantity: 0),
  Item(price: 29, name: "Black Short Skirt", quantity: 4),
  Item(price: 20, name: "Bamboo Socks Cats", quantity: 7),
]
|> iterator.from_list
|> out_of_stock
|> iterator.to_list

// -> [
//   Item(price: 50, name: "Red Short Skirt", quantity: 0),
// ]
```

## 4. Return the total stock

Implement the `total_stock` function which calculates the total amount of items in your storeroom:

```gleam
[
  Item(price: 65, name: "Maxi Brown Dress", quantity: 8),
  Item(price: 50, name: "Red Short Skirt", quantity: 0),
  Item(price: 29, name: "Black Short Skirt", quantity: 4),
  Item(price: 20, name: "Bamboo Socks Cats", quantity: 7),
]
|> iterator.from_list
|> total_stock

// -> 19
```
