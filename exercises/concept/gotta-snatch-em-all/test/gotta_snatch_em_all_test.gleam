import exercism/should
import exercism/test_runner
import gleam/set
import gotta_snatch_em_all

pub fn main() {
  test_runner.main()
}

pub fn new_collection_bleakachu_test() {
  gotta_snatch_em_all.new_collection("Bleakachu")
  |> should.equal(set.from_list(["Bleakachu"]))
}

pub fn new_collection_shiny_bleakachu_test() {
  gotta_snatch_em_all.new_collection("Shiny Bleakachu")
  |> should.equal(set.from_list(["Shiny Bleakachu"]))
}

pub fn add_card_to_a_collection_that_doesnt_have_the_card_test() {
  gotta_snatch_em_all.add_card(set.from_list(["Bleakachu"]), "Veevee")
  |> should.equal(#(False, set.from_list(["Bleakachu", "Veevee"])))
}

pub fn add_card_to_a_collection_that_already_has_the_card_test() {
  gotta_snatch_em_all.add_card(set.from_list(["Veevee", "Bleakachu"]), "Veevee")
  |> should.equal(#(True, set.from_list(["Veevee", "Bleakachu"])))
}

pub fn trade_card_trading_a_card_i_dont_have_for_a_card_i_dont_have_test() {
  gotta_snatch_em_all.trade_card(
    "Charilord",
    "Gyros",
    set.from_list(["Bleakachu"]),
  )
  |> should.equal(#(False, set.from_list(["Bleakachu", "Gyros"])))
}

pub fn trade_card_trading_a_card_i_dont_have_for_a_card_i_have_test() {
  gotta_snatch_em_all.trade_card("Charilord", "Gyros", set.from_list(["Gyros"]))
  |> should.equal(#(False, set.from_list(["Gyros"])))
}

pub fn trade_card_trading_a_card_i_have_for_a_card_i_have_test() {
  gotta_snatch_em_all.trade_card(
    "Charilord",
    "Gyros",
    set.from_list(["Charilord", "Gyros"]),
  )
  |> should.equal(#(False, set.from_list(["Gyros"])))
}

pub fn trade_card_trading_a_card_i_have_for_a_card_i_dont_have_test() {
  gotta_snatch_em_all.trade_card(
    "Charilord",
    "Gyros",
    set.from_list(["Charilord", "Bleakachu", "Veevee"]),
  )
  |> should.equal(#(True, set.from_list(["Gyros", "Bleakachu", "Veevee"])))
}

pub fn boring_cards_of_no_collection_test() {
  gotta_snatch_em_all.boring_cards([])
  |> should.equal([])
}

pub fn boring_cards_of_empty_collections_test() {
  gotta_snatch_em_all.boring_cards([set.from_list([]), set.from_list([])])
  |> should.equal([])
}

pub fn boring_cards_of_identical_collections_test() {
  gotta_snatch_em_all.boring_cards([
    set.from_list(["Shazam", "Wigglycream"]),
    set.from_list(["Shazam", "Wigglycream"]),
  ])
  |> should.equal(["Shazam", "Wigglycream"])
}

pub fn boring_cards_of_collections_with_no_overlap_test() {
  gotta_snatch_em_all.boring_cards([
    set.from_list(["Gyros", "Wigglycream", "Veevee"]),
    set.from_list(["Bleakachu", "Veevee"]),
    set.from_list(["Shazam", "Gyros", "Cooltentbro", "Wigglycream"]),
  ])
  |> should.equal([])
}

pub fn boring_cards_of_collections_with_some_overlap_test() {
  gotta_snatch_em_all.boring_cards([
    set.from_list(["Gyros", "Wigglycream", "Veevee"]),
    set.from_list(["Gyros", "Bleakachu", "Veevee"]),
    set.from_list([
      "Gyros", "Shazam", "Gyros", "Veevee", "Cooltentbro", "Wigglycream",
    ]),
  ])
  |> should.equal(["Gyros", "Veevee"])
}

pub fn total_cards_of_no_collection_test() {
  gotta_snatch_em_all.total_cards([])
  |> should.equal(0)
}

pub fn total_cards_of_empty_collections_test() {
  gotta_snatch_em_all.total_cards([set.from_list([]), set.from_list([])])
  |> should.equal(0)
}

pub fn total_cards_of_same_collections_test() {
  gotta_snatch_em_all.total_cards([
    set.from_list(["Shazam", "Wigglycream"]),
    set.from_list(["Shazam", "Wigglycream"]),
  ])
  |> should.equal(2)
}

pub fn total_cards_of_two_collections_test() {
  gotta_snatch_em_all.total_cards([
    set.from_list(["Shazam", "Cooltentbro", "Wigglycream"]),
    set.from_list(["Gyros", "Wigglycream"]),
  ])
  |> should.equal(4)
}

pub fn total_cards_of_three_collections_test() {
  gotta_snatch_em_all.total_cards([
    set.from_list(["Gyros", "Wigglycream", "Veevee"]),
    set.from_list(["Bleakachu", "Veevee"]),
    set.from_list(["Shazam", "Gyros", "Cooltentbro", "Wigglycream"]),
  ])
  |> should.equal(6)
}

pub fn shiny_cards_with_none_test() {
  gotta_snatch_em_all.shiny_cards(
    set.from_list(["Blasturtle", "Zumbat", "Hitmonchuck"]),
  )
  |> should.equal(set.new())
}

pub fn shiny_cards_with_one_test() {
  gotta_snatch_em_all.shiny_cards(
    set.from_list(["Blasturtle", "Shiny Phiswan", "Zumbat", "Hitmonchuck"]),
  )
  |> should.equal(set.from_list(["Shiny Phiswan"]))
}

pub fn shiny_cards_with_many_test() {
  gotta_snatch_em_all.shiny_cards(
    set.from_list([
      "Shiny Hitmonchuck", "Blasturtle", "Shiny Shazam", "Shiny Phiswan",
      "Zumbat", "Hitmonchuck",
    ]),
  )
  |> should.equal(
    set.from_list(["Shiny Hitmonchuck", "Shiny Phiswan", "Shiny Shazam"]),
  )
}

pub fn fake_shiny_not_included_test() {
  gotta_snatch_em_all.shiny_cards(
    set.from_list(["Shiny Hitmonchuck", "Shinychu"]),
  )
  |> should.equal(set.from_list(["Shiny Hitmonchuck"]))
}
