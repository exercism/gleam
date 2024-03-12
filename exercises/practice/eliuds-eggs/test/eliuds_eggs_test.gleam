import eliuds_eggs
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn zero_eggs_test() {
  eliuds_eggs.egg_count(0)
  |> should.equal(0)
}

pub fn one_egg_test() {
  eliuds_eggs.egg_count(16)
  |> should.equal(1)
}

pub fn four_eggs_test() {
  eliuds_eggs.egg_count(89)
  |> should.equal(4)
}

pub fn thirteen_eggs_test() {
  eliuds_eggs.egg_count(2_000_000_000)
  |> should.equal(13)
}
