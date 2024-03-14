import allergies
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn testing_for_eggs_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Eggs, 0)
}

pub fn testing_for_eggs_allergy_allergic_only_to_eggs_test() {
  let assert True = allergies.allergic_to(allergies.Eggs, 1)
}

pub fn testing_for_eggs_allergy_allergic_to_eggs_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Eggs, 3)
}

pub fn testing_for_eggs_allergy_allergic_to_something_but_not_eggs_test() {
  let assert False = allergies.allergic_to(allergies.Eggs, 2)
}

pub fn testing_for_eggs_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Eggs, 255)
}

pub fn testing_for_peanuts_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Peanuts, 0)
}

pub fn testing_for_peanuts_allergy_allergic_only_to_peanuts_test() {
  let assert True = allergies.allergic_to(allergies.Peanuts, 2)
}

pub fn testing_for_peanuts_allergy_allergic_to_peanuts_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Peanuts, 7)
}

pub fn testing_for_peanuts_allergy_allergic_to_something_but_not_peanuts_test() {
  let assert False = allergies.allergic_to(allergies.Peanuts, 5)
}

pub fn testing_for_peanuts_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Peanuts, 255)
}

pub fn testing_for_shellfish_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Shellfish, 0)
}

pub fn testing_for_shellfish_allergy_allergic_only_to_shellfish_test() {
  let assert True = allergies.allergic_to(allergies.Shellfish, 4)
}

pub fn testing_for_shellfish_allergy_allergic_to_shellfish_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Shellfish, 14)
}

pub fn testing_for_shellfish_allergy_allergic_to_something_but_not_shellfish_test() {
  let assert False = allergies.allergic_to(allergies.Shellfish, 10)
}

pub fn testing_for_shellfish_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Shellfish, 255)
}

pub fn testing_for_strawberries_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Strawberries, 0)
}

pub fn testing_for_strawberries_allergy_allergic_only_to_strawberries_test() {
  let assert True = allergies.allergic_to(allergies.Strawberries, 8)
}

pub fn testing_for_strawberries_allergy_allergic_to_strawberries_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Strawberries, 28)
}

pub fn testing_for_strawberries_allergy_allergic_to_something_but_not_strawberries_test() {
  let assert False = allergies.allergic_to(allergies.Strawberries, 20)
}

pub fn testing_for_strawberries_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Strawberries, 255)
}

pub fn testing_for_tomatoes_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Tomatoes, 0)
}

pub fn testing_for_tomatoes_allergy_allergic_only_to_tomatoes_test() {
  let assert True = allergies.allergic_to(allergies.Tomatoes, 16)
}

pub fn testing_for_tomatoes_allergy_allergic_to_tomatoes_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Tomatoes, 56)
}

pub fn testing_for_tomatoes_allergy_allergic_to_something_but_not_tomatoes_test() {
  let assert False = allergies.allergic_to(allergies.Tomatoes, 40)
}

pub fn testing_for_tomatoes_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Tomatoes, 255)
}

pub fn testing_for_chocolate_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Chocolate, 0)
}

pub fn testing_for_chocolate_allergy_allergic_only_to_chocolate_test() {
  let assert True = allergies.allergic_to(allergies.Chocolate, 32)
}

pub fn testing_for_chocolate_allergy_allergic_to_chocolate_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Chocolate, 112)
}

pub fn testing_for_chocolate_allergy_allergic_to_something_but_not_chocolate_test() {
  let assert False = allergies.allergic_to(allergies.Chocolate, 80)
}

pub fn testing_for_chocolate_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Chocolate, 255)
}

pub fn testing_for_pollen_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Pollen, 0)
}

pub fn testing_for_pollen_allergy_allergic_only_to_pollen_test() {
  let assert True = allergies.allergic_to(allergies.Pollen, 64)
}

pub fn testing_for_pollen_allergy_allergic_to_pollen_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Pollen, 224)
}

pub fn testing_for_pollen_allergy_allergic_to_something_but_not_pollen_test() {
  let assert False = allergies.allergic_to(allergies.Pollen, 160)
}

pub fn testing_for_pollen_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Pollen, 255)
}

pub fn testing_for_cats_allergy_not_allergic_to_anything_test() {
  let assert False = allergies.allergic_to(allergies.Cats, 0)
}

pub fn testing_for_cats_allergy_allergic_only_to_cats_test() {
  let assert True = allergies.allergic_to(allergies.Cats, 128)
}

pub fn testing_for_cats_allergy_allergic_to_cats_and_something_else_test() {
  let assert True = allergies.allergic_to(allergies.Cats, 192)
}

pub fn testing_for_cats_allergy_allergic_to_something_but_not_cats_test() {
  let assert False = allergies.allergic_to(allergies.Cats, 64)
}

pub fn testing_for_cats_allergy_allergic_to_everything_test() {
  let assert True = allergies.allergic_to(allergies.Cats, 255)
}

pub fn list_when_no_allergies_test() {
  allergies.list(0)
  |> should.equal([])
}

pub fn list_when_just_eggs_test() {
  allergies.list(1)
  |> should.equal([allergies.Eggs])
}

pub fn list_when_just_peanuts_test() {
  allergies.list(2)
  |> should.equal([allergies.Peanuts])
}

pub fn list_when_just_strawberries_test() {
  allergies.list(8)
  |> should.equal([allergies.Strawberries])
}

pub fn list_when_eggs_and_peanuts_test() {
  allergies.list(3)
  |> should.equal([allergies.Eggs, allergies.Peanuts])
}

pub fn list_when_more_than_eggs_but_not_peanuts_test() {
  allergies.list(5)
  |> should.equal([allergies.Eggs, allergies.Shellfish])
}

pub fn list_when_lots_of_stuff_test() {
  allergies.list(248)
  |> should.equal([
    allergies.Strawberries,
    allergies.Tomatoes,
    allergies.Chocolate,
    allergies.Pollen,
    allergies.Cats,
  ])
}

pub fn list_when_everything_test() {
  allergies.list(255)
  |> should.equal([
    allergies.Eggs,
    allergies.Peanuts,
    allergies.Shellfish,
    allergies.Strawberries,
    allergies.Tomatoes,
    allergies.Chocolate,
    allergies.Pollen,
    allergies.Cats,
  ])
}

pub fn list_when_no_allergen_score_parts_test() {
  allergies.list(509)
  |> should.equal([
    allergies.Eggs,
    allergies.Shellfish,
    allergies.Strawberries,
    allergies.Tomatoes,
    allergies.Chocolate,
    allergies.Pollen,
    allergies.Cats,
  ])
}

pub fn list_when_no_allergen_score_parts_without_highest_valid_score_test() {
  allergies.list(257)
  |> should.equal([allergies.Eggs])
}
