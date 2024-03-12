import exercism/should
import exercism/test_runner
import high_school_sweetheart

pub fn main() {
  test_runner.main()
}

pub fn first_letter_test() {
  high_school_sweetheart.first_letter("Mary")
  |> should.equal("M")
}

pub fn first_letter_does_not_change_case_test() {
  high_school_sweetheart.first_letter("john")
  |> should.equal("j")
}

pub fn first_letter_removes_whitespace_test() {
  high_school_sweetheart.first_letter("\n\t   Sarah   ")
  |> should.equal("S")
}

pub fn initial_test() {
  high_school_sweetheart.initial("Betty")
  |> should.equal("B.")
}

pub fn initial_uppercases_letter_test() {
  high_school_sweetheart.initial("james")
  |> should.equal("J.")
}

pub fn initials_test() {
  high_school_sweetheart.initials("Linda Miller")
  |> should.equal("L. M.")
}

pub fn pair_test() {
  high_school_sweetheart.pair("Avery Bryant", "Charlie Dixon")
  |> should.equal(
    "
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     A. B.  +  C. D.     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
",
  )
}
