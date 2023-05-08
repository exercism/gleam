# About

Floats are numbers with one or more digits behind the decimal separator. They use the 64-bit double precision floating-point format.

```gleam
let float = 3.45
# -> 3.45
```

Floats also have their own set of operators.

```gleam
1.0 +. 1.4 // -> 2.4
5.0 -. 1.5 // -> 3.5
5.0 /. 2.0 // -> 2.5
3.0 *. 3.1 // -> 9.3

2.0 >. 1.0  // -> True
2.0 <. 1.0  // -> False
2.0 >=. 1.0 // -> True
2.0 <=. 1.0 // -> False
```

Underscores can also be added to floats for clarity.

```gleam
1_000_000.0 // One million
```

Scientific notation can also be used with floats:

```gleam
1.01e3 // -> 1010
15.1e-3 // -> 0.0151
```


