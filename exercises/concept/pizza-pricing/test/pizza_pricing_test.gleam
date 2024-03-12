import exercism/should
import exercism/test_runner
import gleam/list
import pizza_pricing.{Caprese, ExtraSauce, ExtraToppings, Formaggio, Margherita}

pub fn main() {
  test_runner.main()
}

pub fn price_for_pizza_margherita_test() {
  pizza_pricing.pizza_price(Margherita)
  |> should.equal(7)
}

pub fn price_for_pizza_formaggio_test() {
  pizza_pricing.pizza_price(Formaggio)
  |> should.equal(10)
}

pub fn price_for_pizza_caprese_test() {
  pizza_pricing.pizza_price(Caprese)
  |> should.equal(9)
}

pub fn price_for_pizza_margherita_with_extra_sauce_test() {
  pizza_pricing.pizza_price(ExtraSauce(Margherita))
  |> should.equal(8)
}

pub fn price_for_pizza_caprese_with_extra_toppings_test() {
  pizza_pricing.pizza_price(ExtraToppings(Caprese))
  |> should.equal(11)
}

pub fn price_for_pizza_formaggio_with_extra_sauce_and_toppings_test() {
  pizza_pricing.pizza_price(ExtraSauce(ExtraToppings(Caprese)))
  |> should.equal(12)
}

pub fn price_for_pizza_caprese_with_extra_sauce_and_toppings_test() {
  pizza_pricing.pizza_price(ExtraToppings(ExtraSauce(Formaggio)))
  |> should.equal(13)
}

pub fn order_price_for_no_pizzas_test() {
  pizza_pricing.order_price([])
  |> should.equal(0)
}

pub fn order_price_for_single_pizza_caprese_test() {
  pizza_pricing.order_price([Caprese])
  |> should.equal(12)
}

pub fn order_price_for_single_pizza_formaggio_with_extra_sauce_test() {
  pizza_pricing.order_price([ExtraSauce(Formaggio)])
  |> should.equal(14)
}

pub fn order_price_for_one_pizza_margherita_and_one_pizza_caprese_with_extra_toppings_test() {
  pizza_pricing.order_price([Margherita, ExtraToppings(Caprese)])
  |> should.equal(20)
}

pub fn order_price_for_very_large_order_test() {
  pizza_pricing.order_price([
    Margherita,
    ExtraSauce(Margherita),
    Caprese,
    ExtraToppings(Caprese),
    Formaggio,
    ExtraSauce(Formaggio),
    ExtraToppings(ExtraSauce(Formaggio)),
    ExtraToppings(ExtraSauce(Formaggio)),
  ])
  |> should.equal(82)
}

pub fn order_price_for_gigantic_order_test() {
  Margherita
  |> list.repeat(10_000_000)
  |> pizza_pricing.order_price
  |> should.equal(70_000_000)
}
