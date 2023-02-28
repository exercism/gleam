pub type RationalNumber {
  RationalNumber(numerator: Int, denominator: Int)
}

pub fn add(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  todo
}

pub fn subtract(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  todo
}

pub fn multiply(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  todo
}

pub fn divide(r1: RationalNumber, r2: RationalNumber) -> RationalNumber {
  todo
}

pub fn absolute_value(r: RationalNumber) -> RationalNumber {
  todo
}

pub fn power_of_rational(
  number base: RationalNumber,
  to exponent: Int,
) -> Result(RationalNumber, Nil) {
  todo
}

pub fn power_of_real(
  number base: Int,
  to exponent: RationalNumber,
) -> Result(Float, Nil) {
  todo
}

pub fn reduce(r: RationalNumber) -> RationalNumber {
  todo
}
