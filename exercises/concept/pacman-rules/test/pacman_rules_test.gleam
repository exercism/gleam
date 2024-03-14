import exercism/test_runner
import pacman_rules

pub fn main() {
  test_runner.main()
}

pub fn ghost_gets_eaten_test() {
  let assert True = pacman_rules.eat_ghost(True, True)
}

pub fn ghost_does_not_get_eaten_because_no_power_pellet_active_test() {
  let assert False = pacman_rules.eat_ghost(False, True)
}

pub fn ghost_does_not_get_eaten_because_not_touching_ghost_test() {
  let assert False = pacman_rules.eat_ghost(True, False)
}

pub fn ghost_does_not_get_eaten_because_no_power_pellet_is_active_even_if_not_touching_ghost_test() {
  let assert False = pacman_rules.eat_ghost(False, False)
}

pub fn score_when_eating_dot_test() {
  let assert True = pacman_rules.score(True, False)
}

pub fn score_when_eating_power_pellet_test() {
  let assert True = pacman_rules.score(False, True)
}

pub fn no_score_when_nothing_eaten_test() {
  let assert False = pacman_rules.score(False, False)
}

pub fn lose_if_touching_ghost_without_power_pellet_active_test() {
  let assert True = pacman_rules.lose(False, True)
}

pub fn dont_lose_if_touching_ghost_with_power_pellet_active_test() {
  let assert False = pacman_rules.lose(True, True)
}

pub fn dont_lose_if_not_touching_ghost_test() {
  let assert False = pacman_rules.lose(True, False)
}

pub fn win_if_all_dots_eaten_test() {
  let assert True = pacman_rules.win(True, False, False)
}

pub fn dont_win_if_all_dots_eaten_but_touching_ghost_test() {
  let assert False = pacman_rules.win(True, False, True)
}

pub fn win_if_all_dots_eaten_and_touching_ghost_with_power_pellet_active_test() {
  let assert True = pacman_rules.win(True, True, True)
}
