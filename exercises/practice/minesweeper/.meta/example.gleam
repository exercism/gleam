import gleam/string
import gleam/int
import gleam/list
import gleam/pair
import gleam/dict.{type Dict}

type Position {
  Position(Int, Int)
}

type Cell {
  Empty
  Bomb
  Neighbors(Int)
}

pub fn annotate(minefield: String) -> String {
  minefield
  |> parse
  |> do_annotate
  |> print
}

fn parse(minefield: String) -> Dict(Position, Cell) {
  minefield
  |> string.split("\n")
  |> list.index_map(fn(line, row) {
    line
    |> string.to_graphemes
    |> list.index_map(fn(char, col) {
      case char {
        "_" -> #(Position(row, col), Empty)
        _star -> #(Position(row, col), Bomb)
      }
    })
  })
  |> list.flatten
  |> dict.from_list
}

fn do_annotate(cells: Dict(Position, Cell)) -> Dict(Position, Cell) {
  dict.fold(over: cells, from: cells, with: count_neighbors)
}

fn count_neighbors(
  cells: Dict(Position, Cell),
  pos: Position,
  cell: Cell,
) -> Dict(Position, Cell) {
  case cell {
    Empty -> {
      let Position(row, col) = pos
      let neighbors =
        [
          Position(row - 1, col - 1),
          Position(row - 1, col),
          Position(row - 1, col + 1),
          Position(row + 1, col - 1),
          Position(row + 1, col),
          Position(row + 1, col + 1),
          Position(row, col - 1),
          Position(row, col + 1),
        ]
        |> list.filter(fn(pos) { dict.get(cells, pos) == Ok(Bomb) })
        |> list.length

      dict.insert(cells, pos, Neighbors(neighbors))
    }
    _ -> cells
  }
}

fn print(cells: Dict(Position, Cell)) -> String {
  cells
  |> dict.to_list
  |> list.group(fn(pair) {
    let #(Position(row, _), _) = pair
    row
  })
  |> dict.map_values(fn(_row, cells) {
    cells
    |> list.sort(fn(a, b) {
      let #(Position(_, col_a), _) = a
      let #(Position(_, col_b), _) = b
      int.compare(col_a, col_b)
    })
    |> list.map(pair.second)
    |> list.map(print_cell)
    |> string.concat
  })
  |> dict.to_list
  |> list.sort(fn(a, b) { int.compare(pair.first(a), pair.first(b)) })
  |> list.map(pair.second)
  |> string.join("\n")
}

fn print_cell(cell: Cell) -> String {
  case cell {
    Neighbors(0) -> "_"
    Neighbors(n) -> int.to_string(n)
    _ -> "*"
  }
}
