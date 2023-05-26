pub type Pizza {
  Caprese
  ExtraSauce(Pizza)
  ExtraToppings(Pizza)
  Formaggio
  Margherita
}

pub fn pizza_price(pizza: Pizza) -> Int {
  count_pizza_price(pizza, 0)
}

fn count_pizza_price(pizza: Pizza, price: Int) -> Int {
  case pizza {
    Caprese -> price + 9
    ExtraSauce(pizza) -> count_pizza_price(pizza, price + 1)
    ExtraToppings(pizza) -> count_pizza_price(pizza, price + 2)
    Formaggio -> price + 10
    Margherita -> price + 7
  }
}

pub fn order_price(order: List(Pizza)) -> Int {
  case order {
    [pizza] -> pizza_price(pizza) + 3
    [pizza1, pizza2] -> pizza_price(pizza1) + pizza_price(pizza2) + 2
    _ -> count_order_price(order, 0)
  }
}

fn count_order_price(order: List(Pizza), price: Int) -> Int {
  case order {
    [] -> price
    [pizza, ..order] -> count_order_price(order, price + pizza_price(pizza))
  }
}
