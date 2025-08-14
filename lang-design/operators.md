# Operators

## Arithmetic Operators

| Operator | Description                                                 | Example |
| :------- | :---------------------------------------------------------- | :------ |
| `+`      | Addition (allows overflow)                                  | `x + y` |
| `-`      | Subtraction (allows underflow)                              | `a - b` |
| `*`      | Multiplication (allows overflow)                            | `m * n` |
| `/`      | Division (dividing by `0` returns `0`)                      | `p / q` |
| `%`      | Modulo (remainder of division, works with negative numbers) | `r % s` |

All operators can be combined with `=` for assignment operations (`+=`, `-=`, etc.).

## Comparison Operators

| Operator | Description              | Example       |
| :------- | :----------------------- | :------------ |
| `==`     | Equal to                 | `if (a == b)` |
| `!=`     | Not equal to             | `if (a != b)` |
| `<`      | Less than                | `if (a < b)`  |
| `>`      | Greater than             | `if (a > b)`  |
| `<=`     | Less than or equal to    | `if (a <= b)` |
| `>=`     | Greater than or equal to | `if (a >= b)` |

## Logical Operators

| Operator | Description                    | Example              |
| :------- | :----------------------------- | :------------------- |
| `and`    | Logical AND (short-circuiting) | `if (a and b)`       |
| `or`     | Logical OR (short-circuiting)  | `if (c or d)`        |
| `not`    | Logical NOT                    | `if (not condition)` |

## Safe Arithmetic

For operations that need to prevent overflow/underflow:

```rust
// These will return an error if the result would overflow/underflow
sum :: try @add_s(x, y)
diff :: try @sub_s(a, b)
product :: try @mul_s(c, d)
```

## Bitwise Operators

| Operator | Description     | Example (0b values)                  |
| :------- | :-------------- | :----------------------------------- |
| `bOr`    | Bitwise OR      | `00001111 bOr 10110101  = 10111111`  |
| `bAnd`   | Bitwise AND     | `0001111  bAnd 10110101  = 00000101` |
| `bNot`   | Bitwise NOT     | ` bNot 10110101 = 01001010`          |
| `bXor`   | Bitwise XOR     | `00001111 bXor 10110101 = 10111010`  |
| `bShL`   | Bitshift left   | `00001111 bShL 5 = 111000000`        |
| `bShR`   | Bitshift right  | `00001111 bShR 2 = 00000011`         |
| `bRotL`  | Bitrotate left  | `00001111 bRotL 5 = 111000001`       |
| `bRotR`  | Bitrotate right | `00001111 bRotR 5 = 01111000`        |

## Operator Precedence

Operators are evaluated in the following order of precedence (from highest to lowest):

1. Grouping `( )`
2. Field access `.`, array access `[]`, function call `()`
3. Unary `-`, `not`, `bNot`
4. `*`, `/`, `%`
5. `+`, `-`
6. `<<`, `>>`
7. `bAnd`
8. `bXor`
9. `bOr`
10. `<`, `>`, `<=`, `>=`
11. `==`, `!=`
12. `and`
13. `or`
14. Pipe operator `|>`
15. Assignment `=`, `+=`, `-=`, etc.

## Examples

```rust
// Arithmetic
sum :: 5 + 3 * 2       // 11, not 16 (multiplication has higher precedence)
grouped :: (5 + 3) * 2 // 16 (grouping changes precedence)

// Logical operators
combined :: a < 5 and b > 10 // AND has higher precedence than comparison
with_grouping :: a < (5 and b) > 10 // Changes meaning with grouping

// Assignment
x := 10
x += 5 // Same as x = x + 5

// Bit manipulation
flags1 :: 0b0101
flags2 :: 0b1100
combined_flags :: flags1 bOr flags2  // 0b1101
common_flags :: flags1 bAnd flags2   // 0b0100
toggled_flags :: flags1 bXor flags2  // 0b1001
inverted_flags :: bNot flags1        // Inverts all bits

// Pipe operator
processed :: value |> process |> format
// Equivalent to: format(process(value))

// you can also capture the value
a :: abc() |> |abcVal| getName(2, abcVal)


```

## Null-Safety Operators

Howl provides operators for safe handling of optional values:

| Operator | Description                       | Example                            |
| :------- | :-------------------------------- | :--------------------------------- |
| `orelse` | Null-coalescing (provide default) | `maybe_value orelse default_value` |

```rust
// Null-coalescing with orelse
username :: getUserInput() orelse "anonymous"
config_value :: getConfig("timeout") orelse 30

// Can be chained for multiple fallbacks
result :: primarySource() orelse secondarySource() orelse defaultValue
```

## Custom Operators

Howl doesn't support custom operators, but you can achieve similar effects using functions:

```rust
// Instead of a custom operator, use a descriptive function name
combine :: fn (a: Vector, b: Vector) Vector {
    return .{
        .x = a.x + b.x,
        .y = a.y + b.y,
    }
}

// Usage
result :: combine(vec1, vec2)

// For a more functional style, use the pipe operator
result :: vec1 |> combine(vec2)
```
