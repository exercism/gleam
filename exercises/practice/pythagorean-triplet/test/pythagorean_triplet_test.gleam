import exercism/should
import exercism/test_runner
import pythagorean_triplet.{Triplet}

pub fn main() {
  test_runner.main()
}

pub fn triplets_whose_sum_is_12_test() {
  pythagorean_triplet.triplets_with_sum(12)
  |> should.equal([Triplet(3, 4, 5)])
}

pub fn triplets_whose_sum_is_108_test() {
  pythagorean_triplet.triplets_with_sum(108)
  |> should.equal([Triplet(27, 36, 45)])
}

pub fn triplets_whose_sum_is_1000_test() {
  pythagorean_triplet.triplets_with_sum(1000)
  |> should.equal([Triplet(200, 375, 425)])
}

pub fn no_matching_triplets_for_1001_test() {
  pythagorean_triplet.triplets_with_sum(1001)
  |> should.equal([])
}

pub fn returns_all_matching_triplets_test() {
  pythagorean_triplet.triplets_with_sum(90)
  |> should.equal([Triplet(9, 40, 41), Triplet(15, 36, 39)])
}

pub fn several_matching_triplets_test() {
  pythagorean_triplet.triplets_with_sum(840)
  |> should.equal([
    Triplet(40, 399, 401),
    Triplet(56, 390, 394),
    Triplet(105, 360, 375),
    Triplet(120, 350, 370),
    Triplet(140, 336, 364),
    Triplet(168, 315, 357),
    Triplet(210, 280, 350),
    Triplet(240, 252, 348),
  ])
}
