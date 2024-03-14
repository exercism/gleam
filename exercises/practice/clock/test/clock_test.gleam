import clock
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn create_a_new_clock_with_an_initial_time_on_the_hour_test() {
  clock.create(hour: 8, minute: 0)
  |> clock.display()
  |> should.equal("08:00")
}

pub fn create_a_new_clock_with_an_initial_time_past_the_hour_test() {
  clock.create(hour: 11, minute: 9)
  |> clock.display()
  |> should.equal("11:09")
}

pub fn create_a_new_clock_with_an_initial_time_midnight_is_zero_hours_test() {
  clock.create(hour: 24, minute: 0)
  |> clock.display()
  |> should.equal("00:00")
}

pub fn create_a_new_clock_with_an_initial_time_hour_rolls_over_test() {
  clock.create(hour: 25, minute: 0)
  |> clock.display()
  |> should.equal("01:00")
}

pub fn create_a_new_clock_with_an_initial_time_hour_rolls_over_continuously_test() {
  clock.create(hour: 100, minute: 0)
  |> clock.display()
  |> should.equal("04:00")
}

pub fn create_a_new_clock_with_an_initial_time_sixty_minutes_is_next_hour_test() {
  clock.create(hour: 1, minute: 60)
  |> clock.display()
  |> should.equal("02:00")
}

pub fn create_a_new_clock_with_an_initial_time_minutes_roll_over_test() {
  clock.create(hour: 0, minute: 160)
  |> clock.display()
  |> should.equal("02:40")
}

pub fn create_a_new_clock_with_an_initial_time_minutes_roll_over_continuously_test() {
  clock.create(hour: 0, minute: 1723)
  |> clock.display()
  |> should.equal("04:43")
}

pub fn create_a_new_clock_with_an_initial_time_hour_and_minutes_roll_over_test() {
  clock.create(hour: 25, minute: 160)
  |> clock.display()
  |> should.equal("03:40")
}

pub fn create_a_new_clock_with_an_initial_time_hour_and_minutes_roll_over_continuously_test() {
  clock.create(hour: 201, minute: 3001)
  |> clock.display()
  |> should.equal("11:01")
}

pub fn create_a_new_clock_with_an_initial_time_hour_and_minutes_roll_over_to_exactly_midnight_test() {
  clock.create(hour: 72, minute: 8640)
  |> clock.display()
  |> should.equal("00:00")
}

pub fn create_a_new_clock_with_an_initial_time_negative_hour_test() {
  clock.create(hour: -1, minute: 15)
  |> clock.display()
  |> should.equal("23:15")
}

pub fn create_a_new_clock_with_an_initial_time_negative_hour_rolls_over_test() {
  clock.create(hour: -25, minute: 0)
  |> clock.display()
  |> should.equal("23:00")
}

pub fn create_a_new_clock_with_an_initial_time_negative_hour_rolls_over_continuously_test() {
  clock.create(hour: -91, minute: 0)
  |> clock.display()
  |> should.equal("05:00")
}

pub fn create_a_new_clock_with_an_initial_time_negative_minutes_test() {
  clock.create(hour: 1, minute: -40)
  |> clock.display()
  |> should.equal("00:20")
}

pub fn create_a_new_clock_with_an_initial_time_negative_minutes_roll_over_test() {
  clock.create(hour: 1, minute: -160)
  |> clock.display()
  |> should.equal("22:20")
}

pub fn create_a_new_clock_with_an_initial_time_negative_minutes_roll_over_continuously_test() {
  clock.create(hour: 1, minute: -4820)
  |> clock.display()
  |> should.equal("16:40")
}

pub fn create_a_new_clock_with_an_initial_time_negative_sixty_minutes_is_previous_hour_test() {
  clock.create(hour: 2, minute: -60)
  |> clock.display()
  |> should.equal("01:00")
}

pub fn create_a_new_clock_with_an_initial_time_negative_hour_and_minutes_both_roll_over_test() {
  clock.create(hour: -25, minute: -160)
  |> clock.display()
  |> should.equal("20:20")
}

pub fn create_a_new_clock_with_an_initial_time_negative_hour_and_minutes_both_roll_over_continuously_test() {
  clock.create(hour: -121, minute: -5810)
  |> clock.display()
  |> should.equal("22:10")
}

pub fn add_minutes_add_minutes_test() {
  clock.create(hour: 10, minute: 0)
  |> clock.add(minutes: 3)
  |> clock.display()
  |> should.equal("10:03")
}

pub fn add_minutes_add_no_minutes_test() {
  clock.create(hour: 6, minute: 41)
  |> clock.add(minutes: 0)
  |> clock.display()
  |> should.equal("06:41")
}

pub fn add_minutes_add_to_next_hour_test() {
  clock.create(hour: 0, minute: 45)
  |> clock.add(minutes: 40)
  |> clock.display()
  |> should.equal("01:25")
}

pub fn add_minutes_add_more_than_one_hour_test() {
  clock.create(hour: 10, minute: 0)
  |> clock.add(minutes: 61)
  |> clock.display()
  |> should.equal("11:01")
}

pub fn add_minutes_add_more_than_two_hours_with_carry_test() {
  clock.create(hour: 0, minute: 45)
  |> clock.add(minutes: 160)
  |> clock.display()
  |> should.equal("03:25")
}

pub fn add_minutes_add_across_midnight_test() {
  clock.create(hour: 23, minute: 59)
  |> clock.add(minutes: 2)
  |> clock.display()
  |> should.equal("00:01")
}

pub fn add_minutes_add_more_than_one_day_1500_min__25_hrs_test() {
  clock.create(hour: 5, minute: 32)
  |> clock.add(minutes: 1500)
  |> clock.display()
  |> should.equal("06:32")
}

pub fn add_minutes_add_more_than_two_days_test() {
  clock.create(hour: 1, minute: 1)
  |> clock.add(minutes: 3500)
  |> clock.display()
  |> should.equal("11:21")
}

pub fn subtract_minutes_subtract_minutes_test() {
  clock.create(hour: 10, minute: 3)
  |> clock.subtract(minutes: 3)
  |> clock.display()
  |> should.equal("10:00")
}

pub fn subtract_minutes_subtract_to_previous_hour_test() {
  clock.create(hour: 10, minute: 3)
  |> clock.subtract(minutes: 30)
  |> clock.display()
  |> should.equal("09:33")
}

pub fn subtract_minutes_subtract_more_than_an_hour_test() {
  clock.create(hour: 10, minute: 3)
  |> clock.subtract(minutes: 70)
  |> clock.display()
  |> should.equal("08:53")
}

pub fn subtract_minutes_subtract_across_midnight_test() {
  clock.create(hour: 0, minute: 3)
  |> clock.subtract(minutes: 4)
  |> clock.display()
  |> should.equal("23:59")
}

pub fn subtract_minutes_subtract_more_than_two_hours_test() {
  clock.create(hour: 0, minute: 0)
  |> clock.subtract(minutes: 160)
  |> clock.display()
  |> should.equal("21:20")
}

pub fn subtract_minutes_subtract_more_than_two_hours_with_borrow_test() {
  clock.create(hour: 6, minute: 15)
  |> clock.subtract(minutes: 160)
  |> clock.display()
  |> should.equal("03:35")
}

pub fn subtract_minutes_subtract_more_than_one_day_1500_min__25_hrs_test() {
  clock.create(hour: 5, minute: 32)
  |> clock.subtract(minutes: 1500)
  |> clock.display()
  |> should.equal("04:32")
}

pub fn subtract_minutes_subtract_more_than_two_days_test() {
  clock.create(hour: 2, minute: 20)
  |> clock.subtract(minutes: 3000)
  |> clock.display()
  |> should.equal("00:20")
}

pub fn compare_two_clocks_for_equality_clocks_with_same_time_test() {
  let assert True =
    clock.create(hour: 15, minute: 37) == clock.create(hour: 15, minute: 37)
}

pub fn compare_two_clocks_for_equality_clocks_a_minute_apart_test() {
  let assert False =
    clock.create(hour: 15, minute: 36) == clock.create(hour: 15, minute: 37)
}

pub fn compare_two_clocks_for_equality_clocks_an_hour_apart_test() {
  let assert False =
    clock.create(hour: 14, minute: 37) == clock.create(hour: 15, minute: 37)
}

pub fn compare_two_clocks_for_equality_clocks_with_hour_overflow_test() {
  let assert True =
    clock.create(hour: 10, minute: 37) == clock.create(hour: 34, minute: 37)
}

pub fn compare_two_clocks_for_equality_clocks_with_hour_overflow_by_several_days_test() {
  let assert True =
    clock.create(hour: 3, minute: 11) == clock.create(hour: 99, minute: 11)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_hour_test() {
  let assert True =
    clock.create(hour: 22, minute: 40) == clock.create(hour: -2, minute: 40)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_hour_that_wraps_test() {
  let assert True =
    clock.create(hour: 17, minute: 3) == clock.create(hour: -31, minute: 3)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_hour_that_wraps_multiple_times_test() {
  let assert True =
    clock.create(hour: 13, minute: 49) == clock.create(hour: -83, minute: 49)
}

pub fn compare_two_clocks_for_equality_clocks_with_minute_overflow_test() {
  let assert True =
    clock.create(hour: 0, minute: 1) == clock.create(hour: 0, minute: 1441)
}

pub fn compare_two_clocks_for_equality_clocks_with_minute_overflow_by_several_days_test() {
  let assert True =
    clock.create(hour: 2, minute: 2) == clock.create(hour: 2, minute: 4322)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_minute_test() {
  let assert True =
    clock.create(hour: 2, minute: 40) == clock.create(hour: 3, minute: -20)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_minute_that_wraps_test() {
  let assert True =
    clock.create(hour: 4, minute: 10) == clock.create(hour: 5, minute: -1490)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_minute_that_wraps_multiple_times_test() {
  let assert True =
    clock.create(hour: 6, minute: 15) == clock.create(hour: 6, minute: -4305)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_hours_and_minutes_test() {
  let assert True =
    clock.create(hour: 7, minute: 32) == clock.create(hour: -12, minute: -268)
}

pub fn compare_two_clocks_for_equality_clocks_with_negative_hours_and_minutes_that_wrap_test() {
  let assert True =
    clock.create(hour: 18, minute: 7)
    == clock.create(hour: -54, minute: -11_513)
}

pub fn compare_two_clocks_for_equality_full_clock_and_zeroed_clock_test() {
  let assert True =
    clock.create(hour: 24, minute: 0) == clock.create(hour: 0, minute: 0)
}
