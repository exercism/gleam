import exercism/should
import exercism/test_runner
import high_scores

pub fn main() {
  test_runner.main()
}

pub fn list_of_scores_test() {
  high_scores.scores([30, 50, 20, 70])
  |> should.equal([30, 50, 20, 70])
}

pub fn latest_score_test() {
  high_scores.latest([100, 0, 90, 30])
  |> should.equal(Ok(30))
}

pub fn latest_score_without_scores_test() {
  high_scores.latest([])
  |> should.equal(Error(Nil))
}

pub fn personal_best_test() {
  high_scores.personal_best([40, 100, 70])
  |> should.equal(Ok(100))
}

pub fn personal_best_without_scores_test() {
  high_scores.personal_best([])
  |> should.equal(Error(Nil))
}

pub fn personal_top_three_from_a_list_of_scores_test() {
  high_scores.personal_top_three([
    10, 30, 90, 30, 100, 20, 10, 0, 30, 40, 40, 70, 70,
  ])
  |> should.equal([100, 90, 70])
}

pub fn personal_top_highest_to_lowest_test() {
  high_scores.personal_top_three([20, 10, 30])
  |> should.equal([30, 20, 10])
}

pub fn personal_top_when_there_is_a_tie_test() {
  high_scores.personal_top_three([40, 20, 40, 30])
  |> should.equal([40, 40, 30])
}

pub fn personal_top_when_there_are_less_than_3_test() {
  high_scores.personal_top_three([30, 70])
  |> should.equal([70, 30])
}

pub fn personal_top_when_there_is_only_one_test() {
  high_scores.personal_top_three([40])
  |> should.equal([40])
}
