import gleam/int
import gleam/float
import gleam/result

pub type RationalNumber {
  RationalNumber(numerator: Int, denominator: Int)
}

pub fn add(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  let RationalNumber(a, b) = r1
  let RationalNumber(c, d) = r2

  reduce(RationalNumber(numerator: a * d + c * b, denominator: b * d))
}

pub fn subtract(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  let RationalNumber(a, b) = r1
  let RationalNumber(c, d) = r2

  reduce(RationalNumber(numerator: a * d - c * b, denominator: b * d))
}

pub fn multiply(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  let RationalNumber(a, b) = r1
  let RationalNumber(c, d) = r2

  reduce(RationalNumber(numerator: a * c, denominator: b * d))
}

pub fn divide(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  let RationalNumber(a, b) = r1
  let RationalNumber(c, d) = r2

  reduce(RationalNumber(numerator: a * d, denominator: c * b))
}

pub fn absolute_value(r: RationalNumber) -> RationalNumber {
  let RationalNumber(numerator, denominator) = reduce(r)

  RationalNumber(
    numerator: int.absolute_value(numerator),
    denominator: denominator,
  )
}

pub fn power_of_rational(
  number base: RationalNumber,
  to exponent: Int,
) -> RationalNumber {
  case base {
    RationalNumber(numerator, denominator) if exponent < 0 ->
      power_of_rational(
        RationalNumber(numerator: denominator, denominator: numerator),
        to: int.absolute_value(exponent),
      )

    RationalNumber(numerator, denominator) -> {
      let numerator = power_of_integer(numerator, to: exponent)
      let denominator = power_of_integer(denominator, to: exponent)

      reduce(RationalNumber(numerator, denominator))
    }
  }
}

pub fn power_of_real(
  number base: Float,
  to exponent: RationalNumber,
) -> Result(Float, Nil) {
  let RationalNumber(numerator, denominator) = exponent

  use power <- result.then(float.power(base, int.to_float(numerator)))

  nth_root(denominator, of: power)
}

pub fn reduce(r: RationalNumber) -> RationalNumber {
  let RationalNumber(numerator, denominator) = r

  let gcd = gcd(numerator, denominator)

  case numerator / gcd, denominator / gcd {
    numerator, denominator if denominator < 0 ->
      RationalNumber(
        numerator: int.negate(numerator),
        denominator: int.negate(denominator),
      )

    numerator, denominator -> RationalNumber(numerator, denominator)
  }
}

fn power_of_integer(number base: Int, to exponent: Int) -> Int {
  let assert Ok(power) = int.power(base, int.to_float(exponent))

  float.round(power)
}

fn nth_root(n: Int, of p: Float) -> Result(Float, Nil) {
  let n = int.to_float(n)

  float.power(p, 1.0 /. n)
}

fn gcd(a: Int, b: Int) -> Int {
  case a, b {
    a, 0 -> a
    a, b -> gcd(b, a % b)
  }
}
