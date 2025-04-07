# Built-in Functions

Howl provides a set of built-in functions prefixed with `@` for special operations.

## Built-in Function Reference

| Built-in                 | Description                                           | Example                                  |
| :----------------------- | :---------------------------------------------------- | :--------------------------------------- |
| `@import(path)`          | Imports a module at the given path                    | `const std = @import("std")`             |
| `@TypeOf(x)`             | Returns the type of a variable or expression          | `const T = @TypeOf(value)`               |
| `@Type`                  | Programmatically create a type                        | `const T = @Type(type_info)`             |
| `@FieldType`             | Get the type of a struct field                        | `const FT = @FieldType(Struct, "field")` |
| `@sizeOf(T)`             | Returns the size in bytes of type T                   | `const size = @sizeOf(u32)`              |
| `@Vector`                | Create a SIMD vector type for hardware optimization   | `const Vec4 = @Vector(4, f32)`           |
| `@panic(message)`        | Crashes the program with the given message            | `@panic("Unexpected error")`             |
| `@compileError(message)` | Triggers a compile-time error with a message          | `@compileError("Invalid configuration")` |
| `@add_s(a, b)`           | Safe addition that errors on overflow                 | `let sum = try @add_s(x, y)`             |
| `@sub_s(a, b)`           | Safe subtraction that errors on underflow             | `let diff = try @sub_s(a, b)`            |
| `@mul_s(a, b)`           | Safe multiplication that errors on overflow           | `let product = try @mul_s(c, d)`         |
| `@div_s(a, b)`           | Safe division that checks for division by zero        | `let quotient = try @div_s(a, b)`        |
| `@mod`                   | Modulo operation (with sign of dividend)              | `let mod = @mod(10, 3)` // 1            |
| `@mod_s`                 | Safe modulo operation that checks for division by zero| `let mod = try @mod_s(10, n)`           |
| `@rem`                   | Remainder operation (with sign of divisor)            | `let rem = @rem(-10, 3)` // -1          |
| `@rem_s`                 | Safe remainder operation checking for division by zero| `let rem = try @rem_s(-10, n)`          |
| `@castUp`                | Cast a number to a larger type                        | `let big = @castUp(i64, small_int)`     |
| `@truncate`              | Convert a number to smaller type, discard excess bits | `let small = @truncate(i8, large_int)`  |
| `@castDown`              | Cast a number to a smaller type if it fits            | `let small = try @castDown(i8, value)`  |

## Usage Examples

### Type Operations

```rust
const std = @import("std")

fn type_examples() void {
    // Get the type of a variable
    var value: i32 = 42
    const ValueType = @TypeOf(value)
    
    // Get size of a type
    const int_size = @sizeOf(i32) // 4 bytes
    
    // Create a vector type
    const Vec3f = @Vector(3, f32)
    var position = Vec3f{1.0, 2.0, 3.0}
}
```

### Safe Arithmetic

```rust
fn safe_math() !void {
    var a: u8 = 250;
    var b: u8 = 10;
    
    // These will return errors if they would overflow/underflow
    var sum = try @add_s(a, b)      // Returns error.Overflow
    var diff = try @sub_s(5, 10)    // Returns error.Underflow
    var product = try @mul_s(100, 3) // Returns error.Overflow for u8
    
    // Safe division prevents divide-by-zero
    var divisor: i32 = get_user_input();
    var result = try @div_s(100, divisor) // Returns error.DivisionByZero if divisor is 0
}
```

### Casting

```rust
fn casting_examples() !void {
    var small: i16 = 1000;
    var large: i32 = 100000;
    
    // Safe upcasting (always succeeds)
    var bigger = @castUp(i32, small) // i16 -> i32
    
    // Unsafe truncation (may lose data)
    var smaller = @truncate(i8, large) // Truncates to i8 range
    
    // Safe downcasting (returns error if value doesn't fit)
    var safe_small = try @castDown(i8, small) // Returns error.ValueTooLarge
}
```
