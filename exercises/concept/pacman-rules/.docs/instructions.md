# Instructions

In this exercise, you need to translate some rules from the classic game Pac-Man into Gleam functions.

You have four rules to translate, all related to the game states.

> Don't worry about how the arguments are derived, just focus on combining the arguments to return the intended result.

## 1. Define if Pac-Man eats a ghost

Define the `eat_ghost` function that takes two arguments (_if Pac-Man has a power pellet active_ and _if Pac-Man is touching a ghost_) and returns a boolean value if Pac-Man is able to eat the ghost. The function should return true only if Pac-Man has a power pellet active and is touching a ghost.

```gleam
eat_ghost(False, True)
// -> False
```

## 2. Define if Pac-Man scores

Define the `score` function that takes two arguments (_if Pac-Man is touching a power pellet_ and _if Pac-Man is touching a dot_) and returns a boolean value if Pac-Man scored. The function should return true if Pac-Man is touching a power pellet or a dot.

```gleam
score(True, True)
// -> True
```

## 3. Define if Pac-Man loses

Define the `lose` function that takes two arguments (_if Pac-Man has a power pellet active_ and _if Pac-Man is touching a ghost_) and returns a boolean value if Pac-Man loses. The function should return true if Pac-Man is touching a ghost and does not have a power pellet active.

```gleam
lose(False, True)
// -> True
```

## 4. Define if Pac-Man wins

Define the `win` function that takes three arguments (_if Pac-Man has eaten all of the dots_, _if Pac-Man has a power pellet active_, and _if Pac-Man is touching a ghost_) and returns a boolean value if Pac-Man wins. The function should return true if Pac-Man has eaten all of the dots and has not lost based on the arguments defined in part 3.

```gleam
win(False, True, False)
// -> False
```
