import exercism/should
import exercism/test_runner
import gleam/result
import grade_school

pub fn main() {
  test_runner.main()
}

pub fn roster_is_empty_when_no_student_is_added_test() {
  grade_school.create()
  |> grade_school.roster()
  |> should.equal([])
}

pub fn add_a_student_test() {
  let assert Ok(_) =
    grade_school.create()
    |> grade_school.add(student: "Aimee", grade: 2)
    |> result.map(grade_school.roster)
}

pub fn student_is_added_to_the_roster_test() {
  grade_school.create()
  |> grade_school.add(student: "Aimee", grade: 2)
  |> result.map(grade_school.roster)
  |> should.equal(Ok(["Aimee"]))
}

pub fn adding_multiple_students_in_the_same_grade_in_the_roster_test() {
  let assert Ok(_) =
    grade_school.create()
    |> grade_school.add(student: "Blair", grade: 2)
    |> result.try(grade_school.add(to: _, student: "James", grade: 2))
    |> result.try(grade_school.add(to: _, student: "Paul", grade: 2))
    |> result.map(grade_school.roster)
}

pub fn multiple_students_in_the_same_grade_are_added_to_the_roster_test() {
  grade_school.create()
  |> grade_school.add(student: "Blair", grade: 2)
  |> result.try(grade_school.add(to: _, student: "James", grade: 2))
  |> result.try(grade_school.add(to: _, student: "Paul", grade: 2))
  |> result.map(grade_school.roster)
  |> should.equal(Ok(["Blair", "James", "Paul"]))
}

pub fn students_in_multiple_grades_are_added_to_the_roster_test() {
  grade_school.create()
  |> grade_school.add(student: "Chelsea", grade: 3)
  |> result.try(grade_school.add(to: _, student: "Logan", grade: 7))
  |> result.map(grade_school.roster)
  |> should.equal(Ok(["Chelsea", "Logan"]))
}

pub fn student_cant_be_added_to_same_grade_in_the_roster_more_than_once_test() {
  let assert Error(_) =
    grade_school.create()
    |> grade_school.add(student: "Blair", grade: 2)
    |> result.try(grade_school.add(to: _, student: "James", grade: 2))
    |> result.try(grade_school.add(to: _, student: "James", grade: 2))
    |> result.try(grade_school.add(to: _, student: "Paul", grade: 2))
}

pub fn student_cant_be_added_to_multiple_grades_in_the_roster_test() {
  let assert Error(_) =
    grade_school.create()
    |> grade_school.add(student: "Blair", grade: 2)
    |> result.try(grade_school.add(to: _, student: "James", grade: 2))
    |> result.try(grade_school.add(to: _, student: "James", grade: 3))
    |> result.try(grade_school.add(to: _, student: "Paul", grade: 2))
}

pub fn students_are_sorted_by_grades_in_the_roster_test() {
  grade_school.create()
  |> grade_school.add(student: "Jim", grade: 3)
  |> result.try(grade_school.add(to: _, student: "Peter", grade: 2))
  |> result.try(grade_school.add(to: _, student: "Anna", grade: 1))
  |> result.map(grade_school.roster)
  |> should.equal(Ok(["Anna", "Peter", "Jim"]))
}

pub fn students_are_sorted_by_name_in_the_roster_test() {
  grade_school.create()
  |> grade_school.add(student: "Peter", grade: 2)
  |> result.try(grade_school.add(to: _, student: "Zoe", grade: 2))
  |> result.try(grade_school.add(to: _, student: "Alex", grade: 2))
  |> result.map(grade_school.roster)
  |> should.equal(Ok(["Alex", "Peter", "Zoe"]))
}

pub fn students_are_sorted_by_grades_and_then_by_name_in_the_roster_test() {
  grade_school.create()
  |> grade_school.add(student: "Peter", grade: 2)
  |> result.try(grade_school.add(to: _, student: "Anna", grade: 1))
  |> result.try(grade_school.add(to: _, student: "Barb", grade: 1))
  |> result.try(grade_school.add(to: _, student: "Zoe", grade: 2))
  |> result.try(grade_school.add(to: _, student: "Alex", grade: 2))
  |> result.try(grade_school.add(to: _, student: "Jim", grade: 3))
  |> result.try(grade_school.add(to: _, student: "Charlie", grade: 1))
  |> result.map(grade_school.roster)
  |> should.equal(
    Ok(["Anna", "Barb", "Charlie", "Alex", "Peter", "Zoe", "Jim"]),
  )
}

pub fn grade_is_empty_if_no_students_in_the_roster_test() {
  grade_school.create()
  |> grade_school.grade(1)
  |> should.equal([])
}

pub fn grade_is_empty_if_no_students_in_that_grade_test() {
  grade_school.create()
  |> grade_school.add(student: "Peter", grade: 2)
  |> result.try(grade_school.add(to: _, student: "Zoe", grade: 2))
  |> result.try(grade_school.add(to: _, student: "Alex", grade: 2))
  |> result.try(grade_school.add(to: _, student: "Jim", grade: 3))
  |> result.map(grade_school.grade(_, 1))
  |> should.equal(Ok([]))
}

pub fn students_are_sorted_by_name_in_a_grade_test() {
  grade_school.create()
  |> grade_school.add(student: "Franklin", grade: 5)
  |> result.try(grade_school.add(to: _, student: "Bradley", grade: 5))
  |> result.try(grade_school.add(to: _, student: "Jeff", grade: 1))
  |> result.map(grade_school.grade(_, 5))
  |> should.equal(Ok(["Bradley", "Franklin"]))
}
