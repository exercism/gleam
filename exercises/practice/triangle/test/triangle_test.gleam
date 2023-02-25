import gleeunit
import gleeunit/should
import triangle

pub fn main() {
  gleeunit.main()
}

pub fn equilateral_triangle_all_sides_are_equal_test() {
  triangle.equilateral([2, 2, 2])
  |> should.equal(True)
}

pub fn equilateral_triangle_any_side_is_unequal_test() {
  triangle.equilateral([2, 3, 2])
  |> should.equal(False)
}

pub fn equilateral_triangle_no_sides_are_equal_test() {
  triangle.equilateral([5, 4, 6])
  |> should.equal(False)
}

pub fn equilateral_triangle_all_zero_sides_is_not_a_triangle_test() {
  triangle.equilateral([0, 0, 0])
  |> should.equal(False)
}

pub fn isosceles_triangle_last_two_sides_are_equal_test() {
  triangle.isosceles([3, 4, 4])
  |> should.equal(True)
}

pub fn isosceles_triangle_first_two_sides_are_equal_test() {
  triangle.isosceles([4, 4, 3])
  |> should.equal(True)
}

pub fn isosceles_triangle_first_and_last_sides_are_equal_test() {
  triangle.isosceles([4, 3, 4])
  |> should.equal(True)
}

pub fn isosceles_triangle_equilateral_triangles_are_also_isosceles_test() {
  triangle.isosceles([4, 4, 4])
  |> should.equal(True)
}

pub fn isosceles_triangle_no_sides_are_equal_test() {
  triangle.isosceles([2, 3, 4])
  |> should.equal(False)
}

pub fn isosceles_triangle_first_triangle_inequality_violation_test() {
  triangle.isosceles([1, 1, 3])
  |> should.equal(False)
}

pub fn isosceles_triangle_second_triangle_inequality_violation_test() {
  triangle.isosceles([1, 3, 1])
  |> should.equal(False)
}

pub fn isosceles_triangle_third_triangle_inequality_violation_test() {
  triangle.isosceles([3, 1, 1])
  |> should.equal(False)
}

pub fn scalene_triangle_no_sides_are_equal_test() {
  triangle.scalene([5, 4, 6])
  |> should.equal(True)
}

pub fn scalene_triangle_all_sides_are_equal_test() {
  triangle.scalene([4, 4, 4])
  |> should.equal(False)
}

pub fn scalene_triangle_first_and_second_sides_are_equal_test() {
  triangle.scalene([4, 4, 3])
  |> should.equal(False)
}

pub fn scalene_triangle_first_and_third_sides_are_equal_test() {
  triangle.scalene([3, 4, 3])
  |> should.equal(False)
}

pub fn scalene_triangle_second_and_third_sides_are_equal_test() {
  triangle.scalene([4, 3, 3])
  |> should.equal(False)
}

pub fn scalene_triangle_may_not_violate_triangle_inequality_test() {
  triangle.scalene([7, 3, 2])
  |> should.equal(False)
}
