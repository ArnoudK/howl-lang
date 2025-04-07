# Keywords

Reserved identifiers in the Howl programming language.

## Keyword list

---

| Keyword          | Description                                                                |
| :--------------- | :------------------------------------------------------------------------- |
| **Declarations** |                                                                            |
| `let`            | Non-reassignable variable declaration                                      |
| `var`            | Mutable variable declaration                                               |
| `const`          | Declare immutable compile-time known value                                 |
| `comp`           | Declare compile-time variable                                              |
| `comptime`       | compile time functions, for metaprogramming and generics and comp values   |
| `fn`             | Function declaration                                                       |
| `test`           | Create a test `test "My Test Name" { std.test.expectEq(1+1, 2) }`          |
| `pub`            | Export the variable or function declared after this keyword                |
| `struct`         | Define a struct; behaves like a namespace if it has no values              |
| `error`          | Define an error set                                                        |
| `enum`           | Define an enumeration set                                                  |
| `tag`            | Define a tagged union set                                                  |
| `if`             | Start conditional expression (e.g., `let value = if (condition) 1 else 8`) |
| `else`           | Alternative branch in conditional expressions                              |
| `orelse`         | Provide a fallback value when encountering `None`                          |
| **Types**        |                                                                            |
| `Self`           | Type of nearest enclosing scope that's not a function                      |
| `None`           | Null/void value                                                            |
| `Some`           | Non-null value (used in match statements)                                  |
| `AnyType`        | Generic type placeholder                                                   |
| `AnyError`       | Any error type (used in match statements)                                  |
| `i8`             | 8-bit signed integer                                                       |
| `i16`            | 16-bit signed integer                                                      |
| `i32`            | 32-bit signed integer                                                      |
| `i64`            | 64-bit signed integer                                                      |
| `i128`           | 128-bit signed integer                                                     |
| `u8`             | 8-bit unsigned integer                                                     |
| `u16`            | 16-bit unsigned integer                                                    |
| `u32`            | 32-bit unsigned integer                                                    |
| `u64`            | 64-bit unsigned integer                                                    |
| `u128`           | 128-bit unsigned integer                                                   |
| `isize`          | Platform-specific sized signed integer                                     |
| `usize`          | Platform-specific sized unsigned integer                                   |
| `f16`            | 16-bit floating point number                                               |
| `f32`            | 32-bit floating point number                                               |
| `f64`            | 64-bit floating point number                                               |
| `f80`            | 80-bit floating point number                                               |
| `f128`           | 128-bit floating point number                                              |
| `comptime_int`   | Compile-time integer with extended precision                               |
| `comptime_float` | Compile-time floating point with extended precision                        |
| `bool`           | Boolean value type                                                         |
| `true`           | Boolean true value                                                         |
| `false`          | Boolean false value                                                        |
| `str`            | Read-only string (Note: `[]const u8` can be coerced into `const str`)      |
| `strb`           | String builder for efficient string manipulation                           |
| `noreturn`       | Type specifying that function will not return (exits program)              |
| **Flow**         |                                                                            |
| `match`          | Pattern matching construct for branching code                              |
| `for`            | Loop over a slice or repeat call optional function                         |
| `while`          | Loop while a condition is true                                             |
| `break`          | exit a loop                                                                |
| `defer`          | Schedule code to execute when the current scope ends                       |
| `errdefer`       | Schedule code to execute if an uncaught error occurs                       |
| `try`            | Error propagation shorthand                                                |
| `catch`          | Error handling with fallback value                                         |
| **Operators**    |                                                                            |
| `and`            | Logical AND (short-circuiting)                                             |
| `or`             | Logical OR (short-circuiting)                                              |
| `not`            | Logical NOT                                                                |
| `bOr`            | Bitwise OR                                                                 |
| `bAnd`           | Bitwise AND                                                                |
| `bNot`           | Bitwise NOT                                                                |
| `bXor`           | Bitwise XOR                                                                |
| **Concurrency**  |                                                                            |
| `async`          | Indicates an asynchronous function.                                        |
| `await`          | Waits for an asynchronous operation to complete.                           |
| `spawn`          | Creates a new concurrent task or thread.                                   |
| `channel`        | Creates a channel for communication between concurrent tasks.              |
| `mutex`          | Defines a mutex lock for protecting shared resources.                      |
| `lock`           | Acquires a mutex lock.                                                     |
| `unlock`         | Releases a mutex lock.                                                     |

## Built-ins

Built-ins are functions prefixed with the `@` symbol.

| Keyword           | Description                                                                                     |
| :---------------- | :---------------------------------------------------------------------------------------------- |
| `@import`         | Import a file as a module, `const std = @import("std")`, `const myLib = @import("./my_lib.hl")` |
| **Special Types** |                                                                                                 |
| `@Vector`         | Create a SIMD vector type optimized for supported hardware                                      |
| `@TypeOf`         | Get the type of a variable or expression                                                        |
| `@Type`           | Programmatically create a type (see std.howl for details)                                       |
| `@FieldType`      | Get the type of a struct field                                                                  |
| `@sizeOf`         | Returns the size in bytes of a type (only on machine targets)                                   |
| **Operations**    |                                                                                                 |
| `@add_s`          | Safe addition that errors on overflow                                                           |
| `@sub_s`          | Safe subtraction that errors on underflow                                                       |
| `@mul_s`          | Safe multiplication that errors on overflow                                                     |
| `@div_s`          | Safe division that checks for division by zero                                                  |
| `@mod`            | Modulo operation (remainder of division with sign of dividend)                                  |
| `@mod_s`          | Safe modulo operation that checks for division by zero                                          |
| `@rem`            | Remainder operation (remainder of division with sign of divisor)                                |
| `@rem_s`          | Safe remainder operation that checks for division by zero                                       |
| **Errors**        |                                                                                                 |
| `@panic`          | Crash the program with the given message                                                        |
| `@compileError`   | Trigger a compile-time error with the given message                                             |
| **Casting**       |                                                                                                 |
| `@castUp`         | Cast a number to a larger type (e.g., i32 to i64)                                               |
| `@truncate`       | Convert a number to a smaller type by discarding excess bits                                    |
| `@castDown`       | Cast a number to a smaller type, but only if the value fits without loss                        |
