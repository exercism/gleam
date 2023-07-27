# Instructions

In this exercise, you are implementing a way to keep track of the high scores for the most popular game in your local arcade hall.

You have 6 functions to implement, mostly related to manipulating an object that holds high scores.

## 1. Create a new high score board

Create a function `create_score_board` that returns a map that serves as a high score board.
The keys of this object will be the names of the players, the values will be their scores.
For testing purposes, you want to directly include one entry in the object.
This initial entry should consist of `"The Best Ever"` as player name and `1000000` as score.

```gleam
create_score_board()
// returns an object with one initial entry
```

## 2. Add players to a score board

To add a player to the high score board, define the function `add_player`.
It accepts 3 parameters:

- The first parameter is an existing score board map.
- The second parameter is the name of a player as a string.
- The third parameter is the score as an int.

The function returns a map with the new player and score added.

## 3. Remove players from a score board

If players violate the rules of the arcade hall, they are manually removed from the high score board.
Define `remove_player` which takes 2 parameters:

- The first parameter is an existing score board map.
- The second parameter is the name of the player as a string.

This function should return the map without the player that was removed.

If the player was not on the board in the first place, nothing should happen to the board, it should be returned as is.

## 4. Increase a player's score

If a player finishes another game at the arcade hall, a certain amount of points will be added to the previous score on the board.
Implement `update_score`, which takes 3 parameters:

- The first parameter is an existing score board map.
- The second parameter is the name of the player whose score should be increased.
- The third parameter is the score that you wish to **add** to the stored high score.

The function should return a map with the updated score.

## 5. Apply Monday bonus points

The arcade hall keeps a separate score board on Mondays.
At the end of the day, each player on that board gets 100 additional points.

Implement the function `apply_monday_bonus` that accepts a score board.
The function returns a map with the bonus points added for each player that is listed on that board.
