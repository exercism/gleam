# Instructions

Izzy is an avid bird watcher that keeps track of how many birds have visited her garden on any given day.

She's asked you to help bring her bird watching to a new level and implement a few tools that will help her track and process the data.

You have chosen to store the data as a list of integers. The first number in the list is the number of birds that visited your garden today, the second yesterday, and so on.

## 1. Check how many birds visited today

Implement the `bird_count.today` function. It should take a list of daily bird counts and return today's count. If the list is empty, it should return `0`.

```gleam
bird_count.today([2, 5, 1])
// -> 2
```

## 2. Increment today's count

Implement the `bird_count.increment_day_count` function. It should take a list of daily bird counts and increment the today's count by 1. If the list is empty, return `[1]`.

```gleam
bird_count.increment_day_count([4, 0, 2])
// -> [5, 0, 2]
```

## 3. Check if there was a day with no visiting birds

Implement the `bird_count.has_day_without_birds` function. It should take a list of daily bird counts. It should return `True` if there was at least one day when no birds visited the garden, and `False` otherwise.

```gleam
bird_count.has_day_without_birds([2, 0, 4])
// -> True

bird_count.has_day_without_birds([3, 8, 1, 5])
// -> False
```

## 4. Calculate the total number of visiting birds

Implement the `bird_count.total` function. It should take a list of daily bird counts and return the total number that visited your garden since you started collecting the data.

```gleam
bird_count.total([4, 0, 9, 0, 5])
// -> 18
```

## 5. Calculate the number of busy days

Some days are busier than others. A busy day is one where five or more birds have visited your garden.

Implement the `bird_count.busy_days` function. It should take a list of daily bird counts and return the number of busy days.

```gleam
bird_count.busy_days([4, 5, 0, 0, 6])
// -> 2
```
