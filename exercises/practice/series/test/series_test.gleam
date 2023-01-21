import series
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn slices_of_one_from_one() {
  "1"
  |> series.slices(1)
  |> should.equal(Ok(["1"]))
}

pub fn slices_of_one_from_two() {
  "12"
  |> series.slices(1)
  |> should.equal(Ok(["1", "2"]))
}

pub fn slices_of_two() {
  "35"
  |> series.slices(2)
  |> should.equal(Ok(["35"]))
}

pub fn slices_of_two_overlap() {
  "9142"
  |> series.slices(2)
  |> should.equal(Ok(["91", "14", "42"]))
}

pub fn slices_can_include_duplicates() {
  "777777"
  |> series.slices(3)
  |> should.equal(Ok(["777", "777", "777", "777"]))
}

pub fn slices_of_a_long_series() {
  "918493904243"
  |> series.slices(5)
  |> should.equal(Ok([
    "91849", "18493", "84939", "49390", "93904", "39042", "90424", "04243",
  ]))
}

pub fn slice_length_is_too_large() {
  "12345"
  |> series.slices(6)
  |> should.equal(Error(series.SliceLengthTooLarge))
}

pub fn slice_length_is_way_too_large() {
  "12345"
  |> series.slices(42)
  |> should.equal(Error(series.SliceLengthTooLarge))
}

pub fn slice_length_cannot_be_zero() {
  "12345"
  |> series.slices(0)
  |> should.equal(Error(series.SliceLengthZero))
}

pub fn slice_length_cannot_be_negative() {
  "123"
  |> series.slices(-1)
  |> should.equal(Error(series.SliceLengthNegative))
}

pub fn empty_series_is_invalid() {
  ""
  |> series.slices(1)
  |> should.equal(Error(series.EmptySeries))
}
