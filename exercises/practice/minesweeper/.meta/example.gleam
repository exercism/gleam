import gleam/string
import gleam/int
import gleam/list
import gleam/pair
import gleam/map.{type Map}

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

fn parse(minefield: String) -> Map(Position, Cell) {
  minefield
  |> string.split("\n")
  |> list.index_map(fn(row, line) {
    line
    |> string.to_graphemes
    |> list.index_map(fn(col, char) {
      case char {
        "_" -> #(Position(row, col), Empty)
        "*" -> #(Position(row, col), Bomb)
      }
    })
  })
  |> list.flatten
  |> map.from_list
}

fn do_annotate(cells: Map(Position, Cell)) -> Map(Position, Cell) {
  map.fold(over: cells, from: cells, with: count_neighbors)
}

fn count_neighbors(
  cells: Map(Position, Cell),
  pos: Position,
  cell: Cell,
) -> Map(Position, Cell) {
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
        |> list.filter(fn(pos) { map.get(cells, pos) == Ok(Bomb) })
        |> list.length

      map.insert(cells, pos, Neighbors(neighbors))
    }
    _ -> cells
  }
}

fn print(cells: Map(Position, Cell)) -> String {
  cells
  |> map.to_list
  |> list.group(fn(pair) {
    let #(Position(row, _), _) = pair
    row
  })
  |> map.map_values(fn(_row, cells) {
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
  |> map.to_list
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
