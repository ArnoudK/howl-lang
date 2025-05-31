# Variable Declarations

Howl uses a simplified declaration syntax with two main operators and compile-time support.

## Declaration Syntax

| Syntax | Description                                      | Use Case                                                 |
| :----- | :----------------------------------------------- | :------------------------------------------------------- |
| `::`   | Immutable declaration (cannot be reassigned)     | For constants, functions, types, and immutable variables |
| `:=`   | Mutable variable declaration (can be reassigned) | For variables that need to be reassigned                 |
| `comp` | Compile-time prefix for functions and variables  | For values calculated at compile time                    |

## Examples

```rust
// Immutable variable (contents may be modified, but cannot reassign)
user :: Person{.name = "Alice", .age = 30}
user.age = 31 // OK: changing a field
// user = Person{.name = "Bob", .age = 25} // Error: can't reassign '::'

// Constants (immutable values)
PI :: 3.14159
MAX_SIZE : i32 : 1000  // with explicit type

// Mutable variable
counter := 0
counter = 1 // OK: can reassign ':='
// Mutable with type
large_int : i28 = 10e25
large_int += 1

// Compile-time values
comp BUILD_FLAGS :: ["debug", "verbose"]
comp fibonacci := [_]u32{0, 1, 1, 2, 3, 5, 8, 13}
fibonacci[7] = 21 // OK: can modify compile-time value

// Type inference works with both operators
name := "Alice"  // mutable string
config :: load_config()  // immutable result
```
