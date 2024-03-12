import exercism/should
import exercism/test_runner
import largest_series_product

pub fn main() {
  test_runner.main()
}

pub fn finds_the_largest_product_if_span_equals_length_test() {
  largest_series_product.largest_product("29", 2)
  |> should.equal(Ok(18))
}

pub fn can_find_the_largest_product_of_2_with_numbers_in_order_test() {
  largest_series_product.largest_product("0123456789", 2)
  |> should.equal(Ok(72))
}

pub fn can_find_the_largest_product_of_2_test() {
  largest_series_product.largest_product("576802143", 2)
  |> should.equal(Ok(48))
}

pub fn can_find_the_largest_product_of_3_with_numbers_in_order_test() {
  largest_series_product.largest_product("0123456789", 3)
  |> should.equal(Ok(504))
}

pub fn can_find_the_largest_product_of_3_test() {
  largest_series_product.largest_product("1027839564", 3)
  |> should.equal(Ok(270))
}

pub fn can_find_the_largest_product_of_5_with_numbers_in_order_test() {
  largest_series_product.largest_product("0123456789", 5)
  |> should.equal(Ok(15_120))
}

pub fn can_get_the_largest_product_of_a_big_number_test() {
  largest_series_product.largest_product(
    "73167176531330624919225119674426574742355349194934",
    6,
  )
  |> should.equal(Ok(23_520))
}

pub fn reports_one_for_empty_string_and_empty_products_test() {
  largest_series_product.largest_product("", 0)
  |> should.equal(Ok(1))
}

pub fn reports_one_for_nonempty_string_and_empty_products_test() {
  largest_series_product.largest_product("123", 0)
  |> should.equal(Ok(1))
}

pub fn reports_zero_if_the_only_digits_are_zero_test() {
  largest_series_product.largest_product("0000", 2)
  |> should.equal(Ok(0))
}

pub fn reports_zero_if_all_spans_include_zero_test() {
  largest_series_product.largest_product("99099", 3)
  |> should.equal(Ok(0))
}

pub fn rejects_span_longer_than_string_length_test() {
  largest_series_product.largest_product("123", 4)
  |> should.equal(Error(Nil))
}

pub fn rejects_empty_string_and_nonzero_span_test() {
  largest_series_product.largest_product("", 1)
  |> should.equal(Error(Nil))
}

pub fn rejects_invalid_character_in_digits_test() {
  largest_series_product.largest_product("1234a5", 2)
  |> should.equal(Error(Nil))
}

pub fn rejects_negative_span_test() {
  largest_series_product.largest_product("12345", -1)
  |> should.equal(Error(Nil))
}
