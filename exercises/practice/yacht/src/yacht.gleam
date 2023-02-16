import gleam/list
import gleam/int
import gleam/function
import gleam/map
import gleam/pair

pub type Category {
  Ones
  Twos
  Threes
  Fours
  Fives
  Sixes
  FullHouse
  FourOfAKind
  LittleStraight
  BigStraight
  Choice
  Yacht
}

pub fn score(category: Category, dice: List(Int)) -> Int {
  case category {
    Ones -> score_single_number(dice, 1)
    Twos -> score_single_number(dice, 2)
    Threes -> score_single_number(dice, 3)
    Fours -> score_single_number(dice, 4)
    Fives -> score_single_number(dice, 5)
    Sixes -> score_single_number(dice, 6)
    FullHouse -> score_full_house(dice)
    FourOfAKind -> score_four_of_a_kind(dice)
    LittleStraight -> score_little_straight(dice)
    BigStraight -> score_big_straight(dice)
    Choice -> score_choice(dice)
    Yacht -> score_yacht(dice)
  }
}

fn score_single_number(dice: List(Int), number: Int) -> Int {
  dice
  |> list.filter(fn(die) { die == number })
  |> int.sum
}

fn score_full_house(dice: List(Int)) -> Int {
  case dice_with_count(dice) {
    [#(_, 2), #(_, 3)] -> int.sum(dice)
    _ -> 0
  }
}

fn score_four_of_a_kind(dice: List(Int)) -> Int {
  case dice_with_count(dice) {
    [#(number, 5)] -> number * 4
    [_, #(number, 4)] -> number * 4
    _ -> 0
  }
}

fn score_little_straight(dice: List(Int)) -> Int {
  case list.sort(dice, int.compare) {
    [1, 2, 3, 4, 5] -> 30
    _ -> 0
  }
}

fn score_big_straight(dice: List(Int)) -> Int {
  case list.sort(dice, int.compare) {
    [2, 3, 4, 5, 6] -> 30
    _ -> 0
  }
}

fn score_choice(dice: List(Int)) -> Int {
  int.sum(dice)
}

fn score_yacht(dice: List(Int)) -> Int {
  case list.unique(dice) {
    [_] -> 50
    _ -> 0
  }
}

fn dice_with_count(dice: List(Int)) -> List(#(Int, Int)) {
  dice
  |> list.group(function.identity)
  |> map.map_values(fn(_, matching_dice) { list.length(matching_dice) })
  |> map.to_list()
  |> list.sort(fn(a, b) { int.compare(pair.second(a), pair.second(b)) })
}
