import gleam/list
import gleam/map.{type Map}
import gleam/string
import gleam/result

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

fn parse(rows: List(String)) -> Map(Position, Player) {
  rows
  |> list.index_map(fn(row, line) {
    line
    |> string.to_graphemes
    |> list.index_map(fn(col, char) {
      case char {
        "X" -> [#(Position(row, col), X)]
        "O" -> [#(Position(row, col), O)]
        _ -> []
      }
    })
    |> list.flatten
  })
  |> list.flatten
  |> map.from_list
}

fn o_wins(board: Map(Position, Player), size: Size) -> Result(Player, Nil) {
  let start_positions =
    map.filter(board, fn(pos, player) { player == O && pos.row == 0 })

  let end_positions =
    map.filter(
      board,
      fn(pos, player) { player == O && pos.row == size.rows - 1 },
    )

  case map.size(start_positions) > 0 && map.size(end_positions) > 0 {
    False -> Error(Nil)
    True ->
      traverse_board(
        board,
        map.keys(start_positions),
        start_positions,
        end_positions,
        O,
      )
  }
}

fn x_wins(board: Map(Position, Player), size: Size) -> Result(Player, Nil) {
  let start_positions =
    map.filter(board, fn(pos, player) { player == X && pos.row == pos.column })

  let end_positions =
    map.filter(
      board,
      fn(pos, player) {
        player == X && pos.column - pos.row == 2 * size.columns - 2
      },
    )

  case map.size(start_positions) > 0 && map.size(end_positions) > 0 {
    False -> Error(Nil)
    True ->
      traverse_board(
        board,
        map.keys(start_positions),
        start_positions,
        end_positions,
        X,
      )
  }
}

fn traverse_board(
  board: Map(Position, Player),
  current: List(Position),
  explored: Map(Position, Player),
  target: Map(Position, Player),
  player: Player,
) -> Result(Player, Nil) {
  case current {
    [] -> Error(Nil)
    [position, ..rest] ->
      case map.has_key(target, position) {
        True -> Ok(player)
        False -> {
          let next_positions =
            board
            |> get_next_positions(position, player)
            |> list.filter(fn(pos) { !map.has_key(explored, pos) })

          let explored =
            list.fold(
              over: next_positions,
              from: explored,
              with: fn(explored, pos) { map.insert(explored, pos, player) },
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
  board: Map(Position, Player),
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

  list.filter(neighbors, fn(pos) { map.get(board, pos) == Ok(player) })
}
