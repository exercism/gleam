import exercism/should
import exercism/test_runner
import grade_school

// Given students' names along with the grade that they are in, 
// create a roster for the school.

pub fn main() {
  test_runner.main()
}

pub fn roster_is_empty_when_no_student_is_added_test() {
  grade_school.roster([])
  |> should.equal([])
}

pub fn add_a_student_test() {
  grade_school.add([["Aimee", 2]])
  |> should.equal([True])
}

pub fn student_is_added_to_the_roster_test() {
  grade_school.roster([["Aimee", 2]])
  |> should.equal(["Aimee"])
}

pub fn adding_multiple_students_in_the_same_grade_in_the_roster_test() {
  grade_school.add([["Blair", 2], ["James", 2], ["Paul", 2]])
  |> should.equal([True, True, True])
}

pub fn multiple_students_in_the_same_grade_are_added_to_the_roster_test() {
  grade_school.roster([["Blair", 2], ["James", 2], ["Paul", 2]])
  |> should.equal(["Blair", "James", "Paul"])
}

pub fn cannot_add_student_to_same_grade_in_the_roster_more_than_once_test() {
  grade_school.add([["Blair", 2], ["James", 2], ["James", 2], ["Paul", 2]])
  |> should.equal([True, True, False, True])
}

pub fn a_student_can_t_be_in_two_different_grades_test() {
  grade_school.roster(2, [["Aimee", 2], ["Aimee", 1]])
  |> should.equal([])
}

// This test reimplements the test with uuid c125dab7-2a53-492f-a99a-56ad511940d8
// Please identify that test and remove it. Link:
// https://github.com/exercism/problem-specifications/blob/main/exercises/grade-school/canonical-data.json
// Reimplemented to be logically consistent
pub fn a_student_can_only_be_added_to_the_same_grade_in_the_roster_once_test() {
  grade_school.roster([["Aimee", 2], ["Aimee", 2]])
  |> should.equal(["Aimee"])
}

// This test reimplements the test with uuid a0c7b9b8-0e89-47f8-8b4a-c50f885e79d1
// Please identify that test and remove it. Link:
// https://github.com/exercism/problem-specifications/blob/main/exercises/grade-school/canonical-data.json
// Ensure the implementation does not affect students already added
// Ensure the implementation does not prevent addition of other students after preventing the error case
pub fn student_not_added_to_same_grade_in_the_roster_more_than_once_test() {
  grade_school.roster([["Blair", 2], ["James", 2], ["James", 2], ["Paul", 2]])
  |> should.equal(["Blair", "James", "Paul"])
}

pub fn adding_students_in_multiple_grades_test() {
  grade_school.add([["Chelsea", 3], ["Logan", 7]])
  |> should.equal([True, True])
}

pub fn students_in_multiple_grades_are_added_to_the_roster_test() {
  grade_school.roster([["Chelsea", 3], ["Logan", 7]])
  |> should.equal(["Chelsea", "Logan"])
}

pub fn cannot_add_same_student_to_multiple_grades_in_the_roster_test() {
  grade_school.add([["Blair", 2], ["James", 2], ["James", 3], ["Paul", 3]])
  |> should.equal([True, True, False, True])
}

// This test reimplements the test with uuid c125dab7-2a53-492f-a99a-56ad511940d8
// Please identify that test and remove it. Link:
// https://github.com/exercism/problem-specifications/blob/main/exercises/grade-school/canonical-data.json
// Reimplemented to be logically consistent
pub fn a_student_cannot_be_added_to_more_than_one_grade_in_the_sorted_roster_test() {
  grade_school.roster([["Aimee", 2], ["Aimee", 1]])
  |> should.equal(["Aimee"])
}

// This test reimplements the test with uuid 6a03b61e-1211-4783-a3cc-fc7f773fba3f
// Please identify that test and remove it. Link:
// https://github.com/exercism/problem-specifications/blob/main/exercises/grade-school/canonical-data.json
// Ensure the implementation does not affect students already added
// Ensure the implementation does not prevent addition of other students after preventing the error case
pub fn student_not_added_to_multiple_grades_in_the_roster_test() {
  grade_school.roster([["Blair", 2], ["James", 2], ["James", 3], ["Paul", 3]])
  |> should.equal(["Blair", "James", "Paul"])
}

pub fn students_are_sorted_by_grades_in_the_roster_test() {
  grade_school.roster([["Jim", 3], ["Peter", 2], ["Anna", 1]])
  |> should.equal(["Anna", "Peter", "Jim"])
}

pub fn students_are_sorted_by_name_in_the_roster_test() {
  grade_school.roster([["Peter", 2], ["Zoe", 2], ["Alex", 2]])
  |> should.equal(["Alex", "Peter", "Zoe"])
}

pub fn students_are_sorted_by_grades_and_then_by_name_in_the_roster_test() {
  grade_school.roster([
    ["Peter", 2],
    ["Anna", 1],
    ["Barb", 1],
    ["Zoe", 2],
    ["Alex", 2],
    ["Jim", 3],
    ["Charlie", 1],
  ])
  |> should.equal(["Anna", "Barb", "Charlie", "Alex", "Peter", "Zoe", "Jim"])
}

pub fn grade_is_empty_if_no_students_in_the_roster_test() {
  grade_school.grade(1, [])
  |> should.equal([])
}

pub fn grade_is_empty_if_no_students_in_that_grade_test() {
  grade_school.grade(1, [["Peter", 2], ["Zoe", 2], ["Alex", 2], ["Jim", 3]])
  |> should.equal([])
}

pub fn student_not_added_to_same_grade_more_than_once_test() {
  grade_school.grade(2, [["Blair", 2], ["James", 2], ["James", 2], ["Paul", 2]])
  |> should.equal(["Blair", "James", "Paul"])
}

pub fn student_not_added_to_multiple_grades_test() {
  grade_school.grade(2, [["Blair", 2], ["James", 2], ["James", 3], ["Paul", 3]])
  |> should.equal(["Blair", "James"])
}

pub fn student_not_added_to_other_grade_for_multiple_grades_test() {
  grade_school.grade(3, [["Blair", 2], ["James", 2], ["James", 3], ["Paul", 3]])
  |> should.equal(["Paul"])
}

pub fn students_are_sorted_by_name_in_a_grade_test() {
  grade_school.grade(5, [["Franklin", 5], ["Bradley", 5], ["Jeff", 1]])
  |> should.equal(["Bradley", "Franklin"])
}
