import gleam/list
import gleam/set

pub type Position {
  Position(row: Int, column: Int)
}

pub fn saddle_points(matrix: List(List(Int))) -> List(Position) {
  let row_maxima =
    matrix
    |> list.index_map(find_row_maxima)
    |> list.flatten
    |> set.from_list

  let column_minima =
    matrix
    |> list.transpose
    |> list.index_map(find_column_minima)
    |> list.flatten
    |> set.from_list

  set.intersection(row_maxima, column_minima)
  |> set.to_list
}

fn find_row_maxima(values: List(Int), row: Int) -> List(Position) {
  let #(_, positions) =
    list.index_fold(over: values, from: #(0, []), with: fn(acc, value, column) {
      case acc {
        #(kept, positions) if value > kept || positions == [] -> #(value, [
          Position(row + 1, column + 1),
        ])

        #(kept, positions) if value == kept -> #(value, [
          Position(row + 1, column + 1),
          ..positions
        ])

        _ -> acc
      }
    })
  positions
}

fn find_column_minima(values: List(Int), column: Int) -> List(Position) {
  let #(_, positions) =
    list.index_fold(over: values, from: #(0, []), with: fn(acc, value, row) {
      case acc {
        #(kept, positions) if value < kept || positions == [] -> #(value, [
          Position(row + 1, column + 1),
        ])

        #(kept, positions) if value == kept -> #(value, [
          Position(row + 1, column + 1),
          ..positions
        ])

        _ -> acc
      }
    })
  positions
}
