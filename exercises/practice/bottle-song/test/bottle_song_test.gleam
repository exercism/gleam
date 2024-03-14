import bottle_song
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn verse_single_verse_first_generic_verse_test() {
  bottle_song.recite(start_bottles: 10, take_down: 1)
  |> should.equal(
    "Ten green bottles hanging on the wall,
Ten green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be nine green bottles hanging on the wall.",
  )
}

pub fn verse_single_verse_last_generic_verse_test() {
  bottle_song.recite(start_bottles: 3, take_down: 1)
  |> should.equal(
    "Three green bottles hanging on the wall,
Three green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be two green bottles hanging on the wall.",
  )
}

pub fn verse_single_verse_verse_with_2_bottles_test() {
  bottle_song.recite(start_bottles: 2, take_down: 1)
  |> should.equal(
    "Two green bottles hanging on the wall,
Two green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be one green bottle hanging on the wall.",
  )
}

pub fn verse_single_verse_verse_with_1_bottle_test() {
  bottle_song.recite(start_bottles: 1, take_down: 1)
  |> should.equal(
    "One green bottle hanging on the wall,
One green bottle hanging on the wall,
And if one green bottle should accidentally fall,
There'll be no green bottles hanging on the wall.",
  )
}

pub fn lyrics_multiple_verses_first_two_verses_test() {
  bottle_song.recite(start_bottles: 10, take_down: 2)
  |> should.equal(
    "Ten green bottles hanging on the wall,
Ten green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be nine green bottles hanging on the wall.

Nine green bottles hanging on the wall,
Nine green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be eight green bottles hanging on the wall.",
  )
}

pub fn lyrics_multiple_verses_last_three_verses_test() {
  bottle_song.recite(start_bottles: 3, take_down: 3)
  |> should.equal(
    "Three green bottles hanging on the wall,
Three green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be two green bottles hanging on the wall.

Two green bottles hanging on the wall,
Two green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be one green bottle hanging on the wall.

One green bottle hanging on the wall,
One green bottle hanging on the wall,
And if one green bottle should accidentally fall,
There'll be no green bottles hanging on the wall.",
  )
}

pub fn lyrics_multiple_verses_all_verses_test() {
  bottle_song.recite(start_bottles: 10, take_down: 10)
  |> should.equal(
    "Ten green bottles hanging on the wall,
Ten green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be nine green bottles hanging on the wall.

Nine green bottles hanging on the wall,
Nine green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be eight green bottles hanging on the wall.

Eight green bottles hanging on the wall,
Eight green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be seven green bottles hanging on the wall.

Seven green bottles hanging on the wall,
Seven green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be six green bottles hanging on the wall.

Six green bottles hanging on the wall,
Six green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be five green bottles hanging on the wall.

Five green bottles hanging on the wall,
Five green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be four green bottles hanging on the wall.

Four green bottles hanging on the wall,
Four green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be three green bottles hanging on the wall.

Three green bottles hanging on the wall,
Three green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be two green bottles hanging on the wall.

Two green bottles hanging on the wall,
Two green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be one green bottle hanging on the wall.

One green bottle hanging on the wall,
One green bottle hanging on the wall,
And if one green bottle should accidentally fall,
There'll be no green bottles hanging on the wall.",
  )
}
