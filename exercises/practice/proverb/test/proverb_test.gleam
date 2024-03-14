import exercism/should
import exercism/test_runner
import proverb

pub fn main() {
  test_runner.main()
}

pub fn zero_pieces_test() {
  proverb.recite([])
  |> should.equal("")
}

pub fn one_piece_test() {
  proverb.recite(["nail"])
  |> should.equal("And all for the want of a nail.")
}

pub fn two_pieces_test() {
  proverb.recite(["nail", "shoe"])
  |> should.equal(
    "For want of a nail the shoe was lost.
And all for the want of a nail.",
  )
}

pub fn three_pieces_test() {
  proverb.recite(["nail", "shoe", "horse"])
  |> should.equal(
    "For want of a nail the shoe was lost.
For want of a shoe the horse was lost.
And all for the want of a nail.",
  )
}

pub fn full_proverb_test() {
  proverb.recite([
    "nail", "shoe", "horse", "rider", "message", "battle", "kingdom",
  ])
  |> should.equal(
    "For want of a nail the shoe was lost.
For want of a shoe the horse was lost.
For want of a horse the rider was lost.
For want of a rider the message was lost.
For want of a message the battle was lost.
For want of a battle the kingdom was lost.
And all for the want of a nail.",
  )
}

pub fn four_pieces_modernized_test() {
  proverb.recite(["pin", "gun", "soldier", "battle"])
  |> should.equal(
    "For want of a pin the gun was lost.
For want of a gun the soldier was lost.
For want of a soldier the battle was lost.
And all for the want of a pin.",
  )
}
