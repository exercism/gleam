import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string

pub type Player {
  X
  O
}

type Position {
  Position(row: Int, column: Int)
}

type Size {
  Size(rows: Int, columns: Int)
}

pub fn winner(board: String) -> Result(Player, Nil) {
  let rows =
    board
    |> string.trim()
    |> string.split("\n")

  case rows {
    [] -> Error(Nil)
    [first_row, ..] -> {
      let size =
        Size(rows: list.length(rows), columns: string.length(first_row) / 2 + 1)

      let board = parse(rows)

      result.lazy_or(o_wins(board, size), fn() { x_wins(board, size) })
    }
  }
}

fn parse(rows: List(String)) -> Dict(Position, Player) {
  rows
  |> list.index_map(fn(line, row) {
    line
    |> string.to_graphemes
    |> list.index_map(fn(char, col) {
      case char {
        "X" -> [#(Position(row, col), X)]
        "O" -> [#(Position(row, col), O)]
        _ -> []
      }
    })
    |> list.flatten
  })
  |> list.flatten
  |> dict.from_list
}

fn o_wins(board: Dict(Position, Player), size: Size) -> Result(Player, Nil) {
  let start_positions =
    dict.filter(board, fn(pos, player) { player == O && pos.row == 0 })

  let end_positions =
    dict.filter(board, fn(pos, player) {
      player == O && pos.row == size.rows - 1
    })

  case dict.size(start_positions) > 0 && dict.size(end_positions) > 0 {
    False -> Error(Nil)
    True ->
      traverse_board(
        board,
        dict.keys(start_positions),
        start_positions,
        end_positions,
        O,
      )
  }
}

fn x_wins(board: Dict(Position, Player), size: Size) -> Result(Player, Nil) {
  let start_positions =
    dict.filter(board, fn(pos, player) { player == X && pos.row == pos.column })

  let end_positions =
    dict.filter(board, fn(pos, player) {
      player == X && pos.column - pos.row == 2 * size.columns - 2
    })

  case dict.size(start_positions) > 0 && dict.size(end_positions) > 0 {
    False -> Error(Nil)
    True ->
      traverse_board(
        board,
        dict.keys(start_positions),
        start_positions,
        end_positions,
        X,
      )
  }
}

fn traverse_board(
  board: Dict(Position, Player),
  current: List(Position),
  explored: Dict(Position, Player),
  target: Dict(Position, Player),
  player: Player,
) -> Result(Player, Nil) {
  case current {
    [] -> Error(Nil)
    [position, ..rest] ->
      case dict.has_key(target, position) {
        True -> Ok(player)
        False -> {
          let next_positions =
            board
            |> get_next_positions(position, player)
            |> list.filter(fn(pos) { !dict.has_key(explored, pos) })

          let explored =
            list.fold(
              over: next_positions,
              from: explored,
              with: fn(explored, pos) { dict.insert(explored, pos, player) },
            )
          traverse_board(
            board,
            list.append(next_positions, rest),
            explored,
            target,
            player,
          )
        }
      }
  }
}

fn get_next_positions(
  board: Dict(Position, Player),
  position: Position,
  player: Player,
) -> List(Position) {
  let Position(row, col) = position
  let neighbors = [
    Position(row, col + 2),
    Position(row, col - 2),
    Position(row + 1, col + 1),
    Position(row + 1, col - 1),
    Position(row - 1, col + 1),
    Position(row - 1, col - 1),
  ]

  list.filter(neighbors, fn(pos) { dict.get(board, pos) == Ok(player) })
}
