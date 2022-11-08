import gleam/list
import gleam/string
import gleam/string_builder

fn permutations(l: List(a)) -> List(List(a)) {
  case l {
    [] -> [[]]
    _ ->
      l
      |> list.index_map(fn(i_idx, i) {
        l
        |> list.index_fold(
          [],
          fn(acc, j, j_idx) {
            case i_idx == j_idx {
              True -> acc
              False -> [j, ..acc]
            }
          },
        )
        |> list.reverse
        |> permutations
        |> list.map(fn(permutation) { [i, ..permutation] })
      })
      |> list.flatten
  }
}

fn join(builders: List(string_builder.StringBuilder), with sep: String) {
  builders
  |> list.intersperse(string_builder.from_string(sep))
  |> string_builder.concat
}

fn list_anagrams(word: String) {
  word
  |> string.lowercase
  |> string.to_graphemes
  |> permutations
  |> list.map(list.map(_, string_builder.from_string))
  |> list.map(join(_, ""))
  |> list.map(string_builder.to_string)
  |> list.filter(fn(anagram) { anagram != word })
}

pub fn find_anagrams(word: String, candidates: List(String)) -> List(String) {
  let anagrams = list_anagrams(word)

  list.filter(
    candidates,
    fn(candidate) {
      candidate
      |> string.lowercase
      |> list.contains(anagrams, _)
    },
  )
}
