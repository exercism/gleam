import exercism/should
import exercism/test_runner
import kindergarten_garden.{
  Alice, Bob, Charlie, Clover, David, Eve, Fred, Ginny, Grass, Harriet, Ileana,
  Joseph, Kincaid, Larry, Radishes, Violets,
}

pub fn main() {
  test_runner.main()
}

pub fn partial_garden_garden_with_single_student_test() {
  kindergarten_garden.plants("RC\nGG", Alice)
  |> should.equal([Radishes, Clover, Grass, Grass])
}

pub fn partial_garden_different_garden_with_single_student_test() {
  kindergarten_garden.plants("VC\nRC", Alice)
  |> should.equal([Violets, Clover, Radishes, Clover])
}

pub fn partial_garden_garden_with_two_students_test() {
  kindergarten_garden.plants("VVCG\nVVRC", Bob)
  |> should.equal([Clover, Grass, Radishes, Clover])
}

pub fn partial_garden_multiple_students_for_the_same_garden_with_three_students_second_student_s_garden_test() {
  kindergarten_garden.plants("VVCCGG\nVVCCGG", Bob)
  |> should.equal([Clover, Clover, Clover, Clover])
}

pub fn partial_garden_multiple_students_for_the_same_garden_with_three_students_third_student_s_garden_test() {
  kindergarten_garden.plants("VVCCGG\nVVCCGG", Charlie)
  |> should.equal([Grass, Grass, Grass, Grass])
}

pub fn full_garden_for_alice_first_student_s_garden_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Alice,
  )
  |> should.equal([Violets, Radishes, Violets, Radishes])
}

pub fn full_garden_for_bob_second_student_s_garden_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Bob,
  )
  |> should.equal([Clover, Grass, Clover, Clover])
}

pub fn full_garden_for_charlie_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Charlie,
  )
  |> should.equal([Violets, Violets, Clover, Grass])
}

pub fn full_garden_for_david_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    David,
  )
  |> should.equal([Radishes, Violets, Clover, Radishes])
}

pub fn full_garden_for_eve_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Eve,
  )
  |> should.equal([Clover, Grass, Radishes, Grass])
}

pub fn full_garden_for_fred_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Fred,
  )
  |> should.equal([Grass, Clover, Violets, Clover])
}

pub fn full_garden_for_ginny_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Ginny,
  )
  |> should.equal([Clover, Grass, Grass, Clover])
}

pub fn full_garden_for_harriet_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Harriet,
  )
  |> should.equal([Violets, Radishes, Radishes, Violets])
}

pub fn full_garden_for_ileana_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Ileana,
  )
  |> should.equal([Grass, Clover, Violets, Clover])
}

pub fn full_garden_for_joseph_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Joseph,
  )
  |> should.equal([Violets, Clover, Violets, Grass])
}

pub fn full_garden_for_kincaid_second_to_last_student_s_garden_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Kincaid,
  )
  |> should.equal([Grass, Clover, Clover, Grass])
}

pub fn full_garden_for_larry_last_student_s_garden_test() {
  kindergarten_garden.plants(
    "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
    Larry,
  )
  |> should.equal([Grass, Violets, Clover, Violets])
}
