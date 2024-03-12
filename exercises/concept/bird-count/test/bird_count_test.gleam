import bird_count
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn today_returns_0_if_no_birds_were_seen_test() {
  bird_count.today([])
  |> should.equal(0)
}

pub fn today_returns_todays_bird_count_test() {
  bird_count.today([7])
  |> should.equal(7)

  bird_count.today([2, 4, 11, 10, 6, 8])
  |> should.equal(2)
}

pub fn today_create_entry_for_today_if_no_bird_watching_data_recorded_test() {
  bird_count.increment_day_count([])
  |> should.equal([1])
}

pub fn today_adds_1_to_todays_bird_count_test() {
  bird_count.increment_day_count([7])
  |> should.equal([8])

  bird_count.increment_day_count([4, 2, 1, 0, 10])
  |> should.equal([5, 2, 1, 0, 10])
}

pub fn has_day_without_birds_returns_false_if_no_bird_watching_data_recorded_test() {
  let assert False = bird_count.has_day_without_birds([])
}

pub fn has_day_without_birds_returns_false_if_there_are_no_zeros_in_bird_watching_data_test() {
  let assert False = bird_count.has_day_without_birds([1])
}

pub fn has_day_without_birds_returns_true_if_there_are_is_at_least_one_zero_in_bird_watching_data_test() {
  let assert True = bird_count.has_day_without_birds([0])
  let assert True = bird_count.has_day_without_birds([4, 4, 0, 1])
  let assert True = bird_count.has_day_without_birds([0, 0, 3, 0, 5, 6, 0])
}

pub fn total_zero_if_no_bird_watching_data_recorded_test() {
  bird_count.total([])
  |> should.equal(0)
}

pub fn total_sums_up_bird_counts_test() {
  bird_count.total([4])
  |> should.equal(4)

  bird_count.total([3, 0, 0, 4, 4, 0, 0, 10])
  |> should.equal(21)
}

pub fn busy_days_zero_if_no_bird_watching_data_recorded_test() {
  bird_count.busy_days([])
  |> should.equal(0)
}

pub fn busy_days_counts_days_with_bird_count_of_5_or_more_test() {
  bird_count.busy_days([1])
  |> should.equal(0)

  bird_count.busy_days([0, 5])
  |> should.equal(1)

  bird_count.busy_days([0, 6, 10, 4, 4, 5, 0])
  |> should.equal(3)
}
