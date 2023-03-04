import gleeunit
import gleeunit/should
import variable_length_quantity.{IncompleteSequence}

pub fn main() {
  gleeunit.main()
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_zero_test() {
  variable_length_quantity.encode([0x0])
  |> should.equal(<<0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_single_byte_test() {
  variable_length_quantity.encode([0x40])
  |> should.equal(<<0x40>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_largest_single_byte_test() {
  variable_length_quantity.encode([0x7F])
  |> should.equal(<<0x7F>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_smallest_double_byte_test() {
  variable_length_quantity.encode([0x80])
  |> should.equal(<<0x81, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_double_byte_test() {
  variable_length_quantity.encode([0x2000])
  |> should.equal(<<0xC0, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_largest_double_byte_test() {
  variable_length_quantity.encode([0x3FFF])
  |> should.equal(<<0xFF, 0x7F>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_smallest_triple_byte_test() {
  variable_length_quantity.encode([0x4000])
  |> should.equal(<<0x81, 0x80, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_triple_byte_test() {
  variable_length_quantity.encode([0x100000])
  |> should.equal(<<0xC0, 0x80, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_largest_triple_byte_test() {
  variable_length_quantity.encode([0x1FFFFF])
  |> should.equal(<<0xFF, 0xFF, 0x7F>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quadruple_byte_test() {
  variable_length_quantity.encode([0x200000])
  |> should.equal(<<0x81, 0x80, 0x80, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quadruple_byte_test() {
  variable_length_quantity.encode([0x8000000])
  |> should.equal(<<0xC0, 0x80, 0x80, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_largest_quadruple_byte_test() {
  variable_length_quantity.encode([0xFFFFFFF])
  |> should.equal(<<0xFF, 0xFF, 0xFF, 0x7F>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quintuple_byte_test() {
  variable_length_quantity.encode([0x10000000])
  |> should.equal(<<0x81, 0x80, 0x80, 0x80, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quintuple_byte_test() {
  variable_length_quantity.encode([0xFF000000])
  |> should.equal(<<0x8F, 0xF8, 0x80, 0x80, 0x0>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_maximum_32_bit_integer_input_test() {
  variable_length_quantity.encode([0xFFFFFFFF])
  |> should.equal(<<0x8F, 0xFF, 0xFF, 0xFF, 0x7F>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_two_single_byte_values_test() {
  variable_length_quantity.encode([0x40, 0x7F])
  |> should.equal(<<0x40, 0x7F>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_two_multi_byte_values_test() {
  variable_length_quantity.encode([0x4000, 0x123456])
  |> should.equal(<<0x81, 0x80, 0x0, 0xC8, 0xE8, 0x56>>)
}

pub fn encode_a_series_of_integers_producing_a_series_of_bytes_many_multi_byte_values_test() {
  variable_length_quantity.encode([
    0x2000, 0x123456, 0xFFFFFFF, 0x0, 0x3FFF, 0x4000,
  ])
  |> should.equal(<<
    0xC0, 0x0, 0xC8, 0xE8, 0x56, 0xFF, 0xFF, 0xFF, 0x7F, 0x0, 0xFF, 0x7F, 0x81,
    0x80, 0x0,
  >>)
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_one_byte_test() {
  variable_length_quantity.decode(<<0x7F>>)
  |> should.equal(Ok([0x7F]))
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_two_bytes_test() {
  variable_length_quantity.decode(<<0xC0, 0x0>>)
  |> should.equal(Ok([0x2000]))
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_three_bytes_test() {
  variable_length_quantity.decode(<<0xFF, 0xFF, 0x7F>>)
  |> should.equal(Ok([0x1FFFFF]))
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_four_bytes_test() {
  variable_length_quantity.decode(<<0x81, 0x80, 0x80, 0x0>>)
  |> should.equal(Ok([0x200000]))
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_maximum_32_bit_integer_test() {
  variable_length_quantity.decode(<<0x8F, 0xFF, 0xFF, 0xFF, 0x7F>>)
  |> should.equal(Ok([0xFFFFFFFF]))
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_test() {
  variable_length_quantity.decode(<<0xFF>>)
  |> should.equal(Error(IncompleteSequence))
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_even_if_value_is_zero_test() {
  variable_length_quantity.decode(<<0x80>>)
  |> should.equal(Error(IncompleteSequence))
}

pub fn decode_a_series_of_bytes_producing_a_series_of_integers_multiple_values_test() {
  variable_length_quantity.decode(<<
    0xC0, 0x0, 0xC8, 0xE8, 0x56, 0xFF, 0xFF, 0xFF, 0x7F, 0x0, 0xFF, 0x7F, 0x81,
    0x80, 0x0,
  >>)
  |> should.equal(Ok([0x2000, 0x123456, 0xFFFFFFF, 0x0, 0x3FFF, 0x4000]))
}
