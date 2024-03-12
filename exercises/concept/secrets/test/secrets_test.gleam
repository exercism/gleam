import exercism/should
import exercism/test_runner
import secrets

pub fn main() {
  test_runner.main()
}

pub fn add_3_test() {
  let add = secrets.secret_add(3)
  should.equal(6, add(3))
}

pub fn add_6_test() {
  let add = secrets.secret_add(6)
  should.equal(15, add(9))
}

pub fn subtract_3_test() {
  let subtract = secrets.secret_subtract(3)
  should.equal(3, subtract(6))
}

pub fn subtract_6_test() {
  let subtract = secrets.secret_subtract(6)
  should.equal(-3, subtract(3))
}

pub fn multiply_by_3_test() {
  let multiply = secrets.secret_multiply(3)
  should.equal(18, multiply(6))
}

pub fn multiply_by_6_test() {
  let multiply = secrets.secret_multiply(6)
  should.equal(42, multiply(7))
}

pub fn divide_by_3_test() {
  let divide = secrets.secret_divide(3)
  should.equal(2, divide(6))
}

pub fn divide_by_6_test() {
  let divide = secrets.secret_divide(6)
  should.equal(1, divide(7))
}

pub fn combine_5_add_10_then_subtract_5_test() {
  let f = secrets.secret_add(10)
  let g = secrets.secret_subtract(5)
  let h = secrets.secret_combine(f, g)

  should.equal(10, h(5))
}

pub fn combine_100_multiply_by_2_then_subtract_20_test() {
  let f = secrets.secret_multiply(2)
  let g = secrets.secret_subtract(20)
  let h = secrets.secret_combine(f, g)

  should.equal(180, h(100))
}

pub fn combine_100_divide_by_10_then_add_10_test() {
  let f = secrets.secret_divide(10)
  let g = secrets.secret_add(10)
  let h = secrets.secret_combine(f, g)

  should.equal(20, h(100))
}

pub fn combine_32_divide_by_3_then_add_5_test() {
  let f = secrets.secret_divide(3)
  let g = secrets.secret_add(5)
  let h = secrets.secret_combine(f, g)

  should.equal(15, h(32))
}

pub fn combine_7_multiply_3_then_add_5_test() {
  let f = secrets.secret_multiply(3)
  let g = secrets.secret_add(5)
  let h = secrets.secret_combine(f, g)

  should.equal(26, h(7))
}

pub fn combine_7_multiply_7_then_multiply_7_test() {
  let f = secrets.secret_multiply(7)
  let g = secrets.secret_multiply(7)
  let h = secrets.secret_combine(f, g)

  should.equal(343, h(7))
}

pub fn combine_4_divide_1_then_divide_2_test() {
  let f = secrets.secret_divide(1)
  let g = secrets.secret_divide(2)
  let h = secrets.secret_combine(f, g)

  should.equal(2, h(4))
}

pub fn combine_7_divide_7_then_divide_7_test() {
  let f = secrets.secret_divide(7)
  let g = secrets.secret_divide(7)
  let h = secrets.secret_combine(f, g)

  should.equal(0, h(7))
}

pub fn combine_4_add_3_then_divide_7_test() {
  let f = secrets.secret_add(3)
  let g = secrets.secret_divide(7)
  let h = secrets.secret_combine(f, g)

  should.equal(1, h(4))
}

pub fn combine_81_divide_by_9_then_multiply_7_test() {
  let f = secrets.secret_divide(9)
  let g = secrets.secret_multiply(7)
  let h = secrets.secret_combine(f, g)

  should.equal(63, h(81))
}
