# Built-in Functions

Howl provides a set of built-in functions prefixed with `@` for special operations.

## Built-in Function Reference

| Built-in                 | Description                                            | Example                                  |
| :----------------------- | :----------------------------------------------------- | :--------------------------------------- |
| `@import(path)`          | Imports a module at the given path                     | `std :: @import("std")`                  |
| `@TypeOf(x)`             | Returns the type of a variable or expression           | `T :: @TypeOf(value)`                    |
| `@Type`                  | Programmatically create a type                         | `T :: @Type(type_info)`                  |
| `@FieldType`             | Get the type of a struct field                         | `FT :: @FieldType(Struct, "field")`      |
| `@sizeOf(T)`             | Returns the size in bytes of type T                    | `size :: @sizeOf(u32)`                   |
| `@Vector`                | Create a SIMD vector type for hardware optimization    | `Vec4 :: @Vector(4, f32)`                |
| `@panic(message)`        | Crashes the program with the given message             | `@panic("Unexpected error")`             |
| `@compileError(message)` | Triggers a compile-time error with a message           | `@compileError("Invalid configuration")` |
| `@add_s(a, b)`           | Safe addition that errors on overflow                  | `sum :: try @add_s(x, y)`                |
| `@sub_s(a, b)`           | Safe subtraction that errors on underflow              | `diff :: try @sub_s(a, b)`               |
| `@mul_s(a, b)`           | Safe multiplication that errors on overflow            | `product :: try @mul_s(c, d)`            |
| `@div_s(a, b)`           | Safe division that checks for division by zero         | `quotient :: try @div_s(a, b)`           |
| `@mod`                   | Modulo operation (with sign of dividend)               | `mod :: @mod(10, 3)` // 1                |
| `@mod_s`                 | Safe modulo operation that checks for division by zero | `mod :: try @mod_s(10, n)`               |
| `@rem`                   | Remainder operation (with sign of divisor)             | `rem :: @rem(-10, 3)` // -1              |
| `@rem_s`                 | Safe remainder operation checking for division by zero | `rem :: try @rem_s(-10, n)`              |
| `@castUp`                | Cast a number to a larger type                         | `big :: @castUp(i64, small_int)`         |
| `@truncate`              | Convert a number to smaller type, discard excess bits  | `small :: @truncate(i8, large_int)`      |
| `@castDown`              | Cast a number to a smaller type if it fits             | `small :: try @castDown(i8, value)`      |

## Usage Examples

### Type Operations

```rust
std :: @import("std")

type_examples : fn() void {
    // Get the type of a variable
    value : i32 = 42
    ValueType :: @TypeOf(value)

    // Get size of a type
    int_size :: @sizeOf(i32) // 4 bytes

    // Create a vector type
    Vec3f :: @Vector(3, f32)
    position := Vec3f{1.0, 2.0, 3.0}
}
```

### Safe Arithmetic

```rust
safe_math :: fn() !void {
    a : u8 = 250;
    b : u8 = 10;

    // These will return errors if they would overflow/underflow
    sum := try @add_s(a, b)      // Returns error.Overflow
    diff := try @sub_s(5, 10)    // Returns error.Underflow
    product := try @mul_s(100, 3) // Returns error.Overflow for u8

    // Safe division prevents divide-by-zero
    divisor : i32 = get_user_input();
    result := try @div_s(100, divisor) // Returns error.DivisionByZero if divisor is 0
}
```

### Casting

```rust
casting_examples:: fn() !void {
    small : i16 = 1000;
    large : i32 = 100000;

    // Safe upcasting (always succeeds)
    bigger := @castUp(i32, small) // i16 -> i32

    // Unsafe truncation (may lose data)
    smaller := @truncate(i8, large) // Truncates to i8 range

    // Safe downcasting (returns error if value doesn't fit)
    safe_small := try @castDown(i8, small) // Returns error.ValueTooLarge
}
```
