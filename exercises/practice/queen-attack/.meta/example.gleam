pub type Position {
  Position(row: Int, column: Int)
}

pub type Error {
  RowTooSmall
  RowTooLarge
  ColumnTooSmall
  ColumnTooLarge
}

pub fn create(queen: Position) -> Result(Nil, Error) {
  let Position(row, column) = queen
  case Nil {
    _ if row < 0 -> Error(RowTooSmall)
    _ if row > 7 -> Error(RowTooLarge)
    _ if column < 0 -> Error(ColumnTooSmall)
    _ if column > 7 -> Error(ColumnTooLarge)
    _ -> Ok(Nil)
  }
}

pub fn can_attack(
  black_queen black_queen: Position,
  white_queen white_queen: Position,
) -> Bool {
  let Position(row_b, column_b) = black_queen
  let Position(row_w, column_w) = white_queen

  let valid_b = create(black_queen) == Ok(Nil)
  let valid_w = create(white_queen) == Ok(Nil)
  let distinct = black_queen != white_queen
  let same_row = row_b == row_w
  let same_column = column_b == column_w
  let same_diag = row_b - column_b == row_w - column_w
  let same_anti_diag = row_b + column_b == row_w + column_w

  valid_b && valid_w && distinct && {
    same_row || same_column || same_diag || same_anti_diag
  }
}
