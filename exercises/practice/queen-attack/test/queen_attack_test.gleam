import exercism/should
import exercism/test_runner
import queen_attack.{
  ColumnTooLarge, ColumnTooSmall, Position, RowTooLarge, RowTooSmall,
}

pub fn main() {
  test_runner.main()
}

pub fn test_creation_of_queens_with_valid_and_invalid_positions_queen_with_a_valid_position_test() {
  queen_attack.create(Position(row: 2, column: 2))
  |> should.equal(Ok(Nil))
}

pub fn test_creation_of_queens_with_valid_and_invalid_positions_queen_must_have_positive_row_test() {
  queen_attack.create(Position(row: -2, column: 2))
  |> should.equal(Error(RowTooSmall))
}

pub fn test_creation_of_queens_with_valid_and_invalid_positions_queen_must_have_row_on_board_test() {
  queen_attack.create(Position(row: 8, column: 4))
  |> should.equal(Error(RowTooLarge))
}

pub fn test_creation_of_queens_with_valid_and_invalid_positions_queen_must_have_positive_column_test() {
  queen_attack.create(Position(row: 2, column: -2))
  |> should.equal(Error(ColumnTooSmall))
}

pub fn test_creation_of_queens_with_valid_and_invalid_positions_queen_must_have_column_on_board_test() {
  queen_attack.create(Position(row: 4, column: 8))
  |> should.equal(Error(ColumnTooLarge))
}

pub fn test_the_ability_of_one_queen_to_attack_another_cannot_attack_test() {
  let assert False =
    queen_attack.can_attack(
      black_queen: Position(row: 6, column: 6),
      white_queen: Position(row: 2, column: 4),
    )
}

pub fn test_the_ability_of_one_queen_to_attack_another_can_attack_on_same_row_test() {
  let assert True =
    queen_attack.can_attack(
      black_queen: Position(row: 2, column: 6),
      white_queen: Position(row: 2, column: 4),
    )
}

pub fn test_the_ability_of_one_queen_to_attack_another_can_attack_on_same_column_test() {
  let assert True =
    queen_attack.can_attack(
      black_queen: Position(row: 2, column: 5),
      white_queen: Position(row: 4, column: 5),
    )
}

pub fn test_the_ability_of_one_queen_to_attack_another_can_attack_on_first_diagonal_test() {
  let assert True =
    queen_attack.can_attack(
      black_queen: Position(row: 0, column: 4),
      white_queen: Position(row: 2, column: 2),
    )
}

pub fn test_the_ability_of_one_queen_to_attack_another_can_attack_on_second_diagonal_test() {
  let assert True =
    queen_attack.can_attack(
      black_queen: Position(row: 3, column: 1),
      white_queen: Position(row: 2, column: 2),
    )
}

pub fn test_the_ability_of_one_queen_to_attack_another_can_attack_on_third_diagonal_test() {
  let assert True =
    queen_attack.can_attack(
      black_queen: Position(row: 1, column: 1),
      white_queen: Position(row: 2, column: 2),
    )
}

pub fn test_the_ability_of_one_queen_to_attack_another_can_attack_on_fourth_diagonal_test() {
  let assert True =
    queen_attack.can_attack(
      black_queen: Position(row: 0, column: 6),
      white_queen: Position(row: 1, column: 7),
    )
}

pub fn test_the_ability_of_one_queen_to_attack_another_cannot_attack_if_falling_diagonals_are_only_the_same_when_reflected_across_the_longest_falling_diagonal_test() {
  let assert False =
    queen_attack.can_attack(
      black_queen: Position(row: 2, column: 5),
      white_queen: Position(row: 4, column: 1),
    )
}
