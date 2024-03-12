import exercism/should
import exercism/test_runner
import nth_prime

pub fn main() {
  test_runner.main()
}

pub fn first_prime_test() {
  nth_prime.prime(1)
  |> should.equal(Ok(2))
}

pub fn second_prime_test() {
  nth_prime.prime(2)
  |> should.equal(Ok(3))
}

pub fn sixth_prime_test() {
  nth_prime.prime(6)
  |> should.equal(Ok(13))
}

pub fn big_prime_test() {
  nth_prime.prime(10_001)
  |> should.equal(Ok(104_743))
}

pub fn there_is_no_zeroth_prime_test() {
  nth_prime.prime(0)
  |> should.equal(Error(Nil))
}
