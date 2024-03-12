import diamond
import exercism/should
import exercism/test_runner
import gleam/string

pub fn main() {
  test_runner.main()
}

pub fn degenerate_case_with_a_single_a_row_test() {
  diamond.build("A")
  |> should.equal("A")
}

pub fn degenerate_case_with_no_row_containing_3_distinct_groups_of_spaces_test() {
  diamond.build("B")
  |> should.equal(string.join([" A ", "B B", " A "], "\n"))
}

pub fn smallest_non_degenerate_case_with_odd_diamond_side_length_test() {
  diamond.build("C")
  |> should.equal(string.join(
    ["  A  ", " B B ", "C   C", " B B ", "  A  "],
    "\n",
  ))
}

pub fn smallest_non_degenerate_case_with_even_diamond_side_length_test() {
  diamond.build("D")
  |> should.equal(string.join(
    [
      "   A   ", "  B B  ", " C   C ", "D     D", " C   C ", "  B B  ",
      "   A   ",
    ],
    "\n",
  ))
}

pub fn largest_possible_diamond_test() {
  diamond.build("Z")
  |> should.equal(string.join(
    [
      "                         A                         ",
      "                        B B                        ",
      "                       C   C                       ",
      "                      D     D                      ",
      "                     E       E                     ",
      "                    F         F                    ",
      "                   G           G                   ",
      "                  H             H                  ",
      "                 I               I                 ",
      "                J                 J                ",
      "               K                   K               ",
      "              L                     L              ",
      "             M                       M             ",
      "            N                         N            ",
      "           O                           O           ",
      "          P                             P          ",
      "         Q                               Q         ",
      "        R                                 R        ",
      "       S                                   S       ",
      "      T                                     T      ",
      "     U                                       U     ",
      "    V                                         V    ",
      "   W                                           W   ",
      "  X                                             X  ",
      " Y                                               Y ",
      "Z                                                 Z",
      " Y                                               Y ",
      "  X                                             X  ",
      "   W                                           W   ",
      "    V                                         V    ",
      "     U                                       U     ",
      "      T                                     T      ",
      "       S                                   S       ",
      "        R                                 R        ",
      "         Q                               Q         ",
      "          P                             P          ",
      "           O                           O           ",
      "            N                         N            ",
      "             M                       M             ",
      "              L                     L              ",
      "               K                   K               ",
      "                J                 J                ",
      "                 I               I                 ",
      "                  H             H                  ",
      "                   G           G                   ",
      "                    F         F                    ",
      "                     E       E                     ",
      "                      D     D                      ",
      "                       C   C                       ",
      "                        B B                        ",
      "                         A                         ",
    ],
    "\n",
  ))
}
