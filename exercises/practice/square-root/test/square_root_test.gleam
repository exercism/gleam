import exercism/should
import exercism/test_runner
import square_root

pub fn main() {
  test_runner.main()
}

pub fn root_of_1_test() {
  square_root.square_root(1)
  |> should.equal(1)
}

pub fn root_of_4_test() {
  square_root.square_root(4)
  |> should.equal(2)
}

pub fn root_of_25_test() {
  square_root.square_root(25)
  |> should.equal(5)
}

pub fn root_of_81_test() {
  square_root.square_root(81)
  |> should.equal(9)
}

pub fn root_of_196_test() {
  square_root.square_root(196)
  |> should.equal(14)
}

pub fn root_of_65025_test() {
  square_root.square_root(65_025)
  |> should.equal(255)
}
