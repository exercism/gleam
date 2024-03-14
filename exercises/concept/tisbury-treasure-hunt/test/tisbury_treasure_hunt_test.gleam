import exercism/should
import exercism/test_runner
import tisbury_treasure_hunt

pub fn main() {
  test_runner.main()
}

pub fn place_location_to_treasure_location_test() {
  tisbury_treasure_hunt.place_location_to_treasure_location(#("C", 1))
  |> should.equal(#(1, "C"))
}

pub fn seaside_cottages_is_not_at_1f_test() {
  let assert False =
    tisbury_treasure_hunt.treasure_location_matches_place_location(#("C", 1), #(
      1,
      "F",
    ))
}

pub fn aqua_lagoon_is_at_1f_test() {
  let assert True =
    tisbury_treasure_hunt.treasure_location_matches_place_location(#("F", 1), #(
      1,
      "F",
    ))
}

pub fn places_should_know_how_many_treasures_are_available_test() {
  let treasures = [
    #("Amethyst Octopus", #(1, "F")),
    #("Scrimshaw Whale's Tooth", #(1, "F")),
  ]
  let assert 2 =
    tisbury_treasure_hunt.count_place_treasures(
      #("Aqua Lagoon (Island of Mystery)", #("F", 1)),
      treasures,
    )
}

pub fn can_swap_amethyst_octopus_for_crystal_crab_at_stormy_breakwater_test() {
  let assert True =
    tisbury_treasure_hunt.special_case_swap_possible(
      #("Amethyst Octopus", #(1, "F")),
      #("Stormy Breakwater", #("B", 5)),
      #("Crystal Crab", #(6, "A")),
    )
}

pub fn can_swap_amethyst_octopus_for_glass_starfish_at_stormy_breakwater_test() {
  let assert True =
    tisbury_treasure_hunt.special_case_swap_possible(
      #("Amethyst Octopus", #(1, "F")),
      #("Stormy Breakwater", #("B", 5)),
      #("Glass Starfish", #(6, "D")),
    )
}

pub fn cannot_swap_amethyst_octopus_for_angry_monkey_figurine_at_stormy_breakwater_test() {
  let assert False =
    tisbury_treasure_hunt.special_case_swap_possible(
      #("Amethyst Octopus", #(1, "F")),
      #("Stormy Breakwater", #("B", 5)),
      #("Angry Monkey Figurine", #(5, "B")),
    )
}

pub fn can_swap_vintage_pirate_hat_for_model_ship_in_large_bottle_at_harbor_managers_office_test() {
  let assert True =
    tisbury_treasure_hunt.special_case_swap_possible(
      #("Vintage Pirate Hat", #(7, "E")),
      #("Harbor Managers Office", #("A", 8)),
      #("Model Ship in Large Bottle", #(8, "A")),
    )
}

pub fn can_swap_vintage_pirate_hat_for_antique_glass_fishnet_float_at_harbor_managers_office_test() {
  let assert True =
    tisbury_treasure_hunt.special_case_swap_possible(
      #("Vintage Pirate Hat", #(7, "E")),
      #("Harbor Managers Office", #("A", 8)),
      #("Antique Glass Fishnet Float", #(3, "D")),
    )
}

pub fn cannot_swap_vintage_pirate_hat_for_carved_wooden_elephant_at_harbor_managers_office_test() {
  let assert False =
    tisbury_treasure_hunt.special_case_swap_possible(
      #("Vintage Pirate Hat", #(7, "E")),
      #("Harbor Managers Office", #("A", 8)),
      #("Carved Wooden Elephant", #(8, "C")),
    )
}

pub fn cannot_swap_vintage_pirate_hat_for_model_ship_in_large_bottle_at_old_schooner_test() {
  let assert False =
    tisbury_treasure_hunt.special_case_swap_possible(
      #("Vintage Pirate Hat", #(7, "E")),
      #("Old Schooner", #("A", 6)),
      #("Model Ship in Large Bottle", #(8, "A")),
    )
}
