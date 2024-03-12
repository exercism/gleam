import exercism/should
import exercism/test_runner
import say.{OutOfRange}

pub fn main() {
  test_runner.main()
}

pub fn zero_test() {
  say.say(0)
  |> should.equal(Ok("zero"))
}

pub fn one_test() {
  say.say(1)
  |> should.equal(Ok("one"))
}

pub fn fourteen_test() {
  say.say(14)
  |> should.equal(Ok("fourteen"))
}

pub fn twenty_test() {
  say.say(20)
  |> should.equal(Ok("twenty"))
}

pub fn twenty_two_test() {
  say.say(22)
  |> should.equal(Ok("twenty-two"))
}

pub fn thirty_test() {
  say.say(30)
  |> should.equal(Ok("thirty"))
}

pub fn ninety_nine_test() {
  say.say(99)
  |> should.equal(Ok("ninety-nine"))
}

pub fn one_hundred_test() {
  say.say(100)
  |> should.equal(Ok("one hundred"))
}

pub fn one_hundred_twenty_three_test() {
  say.say(123)
  |> should.equal(Ok("one hundred twenty-three"))
}

pub fn two_hundred_test() {
  say.say(200)
  |> should.equal(Ok("two hundred"))
}

pub fn nine_hundred_ninety_nine_test() {
  say.say(999)
  |> should.equal(Ok("nine hundred ninety-nine"))
}

pub fn one_thousand_test() {
  say.say(1000)
  |> should.equal(Ok("one thousand"))
}

pub fn one_thousand_two_hundred_thirty_four_test() {
  say.say(1234)
  |> should.equal(Ok("one thousand two hundred thirty-four"))
}

pub fn one_million_test() {
  say.say(1_000_000)
  |> should.equal(Ok("one million"))
}

pub fn one_million_two_thousand_three_hundred_forty_five_test() {
  say.say(1_002_345)
  |> should.equal(Ok("one million two thousand three hundred forty-five"))
}

pub fn one_billion_test() {
  say.say(1_000_000_000)
  |> should.equal(Ok("one billion"))
}

pub fn a_big_number_test() {
  say.say(987_654_321_123)
  |> should.equal(Ok(
    "nine hundred eighty-seven billion six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three",
  ))
}

pub fn numbers_below_zero_are_out_of_range_test() {
  say.say(-1)
  |> should.equal(Error(OutOfRange))
}

pub fn numbers_above_999_999_999_999_are_out_of_range_test() {
  say.say(1_000_000_000_000)
  |> should.equal(Error(OutOfRange))
}
