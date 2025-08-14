# Built-in Functions

Howl provides a comprehensive set of built-in functions prefixed with `@` for special operations that require compiler support.

## Built-in Function Reference

### Module System

| Built-in         | Description                              | Example                    |
| :--------------- | :--------------------------------------- | :------------------------- |
| `@import(path)`  | Imports a module at the given path       | `std :: @import("std")`    |

### Type Introspection

| Built-in                    | Description                                      | Example                             |
| :-------------------------- | :----------------------------------------------- | :---------------------------------- |
| `@TypeOf(x)`                | Returns the type of a variable or expression     | `T :: @TypeOf(value)`              |
| `@Type(type_info)`          | Programmatically create a type                   | `T :: @Type(type_info)`            |
| `@FieldType(Struct, field)` | Get the type of a struct field                   | `FT :: @FieldType(Point, "x")`     |
| `@sizeOf(T)`                | Returns the size in bytes of type T              | `size :: @sizeOf(u32)` // 4        |
| `@alignOf(T)`               | Returns the alignment requirement of type T      | `align :: @alignOf(f64)` // 8      |
| `@offsetOf(Struct, field)`  | Returns the byte offset of a field in a struct   | `offset :: @offsetOf(Point, "y")`  |

### SIMD and Vectors

| Built-in              | Description                                    | Example                      |
| :-------------------- | :--------------------------------------------- | :--------------------------- |
| `@Vector(len, T)`     | Create a SIMD vector type                      | `Vec4f :: @Vector(4, f32)`   |
| `@splat(vec_type, x)` | Create vector with all elements set to value  | `v := @splat(Vec4f, 1.0)`    |
| `@shuffle(a, b, mask)`| Shuffle elements between two vectors           | `result := @shuffle(v1, v2, mask)` |

### Safe Arithmetic

| Built-in        | Description                                      | Example                        |
| :-------------- | :----------------------------------------------- | :----------------------------- |
| `@add_s(a, b)`  | Safe addition that errors on overflow           | `sum := try @add_s(x, y)`      |
| `@sub_s(a, b)`  | Safe subtraction that errors on underflow       | `diff := try @sub_s(a, b)`     |
| `@mul_s(a, b)`  | Safe multiplication that errors on overflow     | `product := try @mul_s(c, d)`  |
| `@div_s(a, b)`  | Safe division that checks for division by zero  | `quotient := try @div_s(a, b)` |
| `@mod_s(a, b)`  | Safe modulo that checks for division by zero    | `mod := try @mod_s(10, n)`     |
| `@rem_s(a, b)`  | Safe remainder that checks for division by zero | `rem := try @rem_s(-10, n)`    |

### Regular Arithmetic

| Built-in      | Description                               | Example                 |
| :------------ | :---------------------------------------- | :---------------------- |
| `@mod(a, b)`  | Modulo operation (with sign of dividend) | `mod := @mod(10, 3)` // 1 |
| `@rem(a, b)`  | Remainder operation (with sign of divisor) | `rem := @rem(-10, 3)` // -1 |

### Type Casting

| Built-in               | Description                                    | Example                           |
| :--------------------- | :--------------------------------------------- | :-------------------------------- |
| `@castUp(T, value)`    | Cast a number to a larger type                 | `big := @castUp(i64, small_int)`  |
| `@truncate(T, value)`  | Convert to smaller type, discard excess bits  | `small := @truncate(i8, large)`   |
| `@castDown(T, value)`  | Cast to smaller type if it fits                | `small := try @castDown(i8, val)` |
| `@intCast(T, value)`   | Cast between integer types with safety checks | `result := try @intCast(u16, x)`  |
| `@floatCast(T, value)` | Cast between floating point types             | `f32_val := @floatCast(f32, f64_val)` |
| `@bitCast(T, value)`   | Reinterpret bits as different type            | `int_bits := @bitCast(u32, float_val)` |

### Compilation Control

| Built-in                   | Description                                | Example                               |
| :------------------------- | :----------------------------------------- | :------------------------------------ |
| `@panic(message)`          | Crashes the program with the given message | `@panic("Unexpected error")`         |
| `@compileError(message)`   | Triggers a compile-time error              | `@compileError("Invalid config")`     |
| `@compileLog(args...)`     | Print to compiler output at compile time  | `@compileLog("Compiling:", @TypeOf(x))` |

### Memory Operations

| Built-in              | Description                                | Example                        |
| :-------------------- | :----------------------------------------- | :----------------------------- |
| `@memcpy(dest, src)`  | Copy memory from source to destination    | `@memcpy(buffer, source)`      |
| `@memset(ptr, val)`   | Set memory to a specific value             | `@memset(buffer, 0)`           |
| `@memcmp(a, b)`       | Compare two memory regions                 | `equal := @memcmp(buf1, buf2) == 0` |

### Bit Operations

| Built-in                | Description                              | Example                          |
| :---------------------- | :--------------------------------------- | :------------------------------- |
| `@clz(x)`               | Count leading zeros                      | `zeros := @clz(u32, 0x0000FFFF)` |
| `@ctz(x)`               | Count trailing zeros                     | `zeros := @ctz(u32, 0xFFFF0000)` |
| `@popCount(x)`          | Count number of set bits                 | `bits := @popCount(u32, 0xF0F0)` |
| `@byteSwap(x)`          | Reverse byte order                       | `swapped := @byteSwap(u32, x)`   |
| `@bitReverse(x)`        | Reverse bit order                        | `reversed := @bitReverse(u8, x)` |

## Usage Examples

### Type Operations

```rust
std :: @import("std")

type_examples :: fn() void {
    // Get the type of a variable
    value : i32 = 42
    ValueType :: @TypeOf(value)  // i32
    
    // Get size and alignment information
    int_size :: @sizeOf(i32)     // 4 bytes
    int_align :: @alignOf(i32)   // 4 bytes
    
    // Create a vector type
    Vec3f :: @Vector(3, f32)
    position := Vec3f{1.0, 2.0, 3.0}
    
    // Struct field information
    Point :: struct {
        x: f32,
        y: f32,
    }
    
    field_type :: @FieldType(Point, "x")    // f32
    y_offset :: @offsetOf(Point, "y")       // 4 (bytes)
}
```

### Safe Arithmetic

```rust
safe_math :: fn() !void {
    a : u8 = 250
    b : u8 = 10
    
    // These will return errors if they would overflow/underflow
    sum := @add_s(a, b) catch |err| switch (err) {
        error.Overflow => {
            std.debug.print("Addition overflow detected!")
            return err
        }
    }
    
    // Safe division prevents divide-by-zero
    divisor : i32 = get_user_input()
    result := @div_s(100, divisor) catch |err| switch (err) {
        error.DivisionByZero => {
            std.debug.print("Cannot divide by zero!")
            return err
        }
    }
}
```

### Type Casting

```rust
casting_examples :: fn() !void {
    small : i16 = 1000
    large : i32 = 100000
    
    // Safe upcasting (always succeeds)
    bigger := @castUp(i32, small)  // i16 -> i32
    
    // Unsafe truncation (may lose data)
    smaller := @truncate(i8, large)  // Truncates to i8 range
    
    // Safe downcasting (returns error if value doesn't fit)
    safe_small := @castDown(i8, small) catch |err| switch (err) {
        error.ValueTooLarge => {
            std.debug.print("Value {} too large for i8", .{small})
            return err
        }
    }
    
    // Integer casting with overflow checking
    result := @intCast(u16, large) catch |err| switch (err) {
        error.Overflow => {
            std.debug.print("Value overflows u16 range")
            return err
        }
    }
}
```

### SIMD Operations

```rust
simd_examples :: fn() void {
    // Create vector types
    Vec4f :: @Vector(4, f32)
    Vec4i :: @Vector(4, i32)
    
    // Create vectors
    a := Vec4f{1.0, 2.0, 3.0, 4.0}
    b := @splat(Vec4f, 2.0)  // {2.0, 2.0, 2.0, 2.0}
    
    // Vector arithmetic (element-wise)
    result := a + b  // {3.0, 4.0, 5.0, 6.0}
    
    // Shuffle operations
    mask := @Vector(4, i32){0, 2, 1, 3}
    shuffled := @shuffle(a, b, mask)
}
```

### Memory Operations

```rust
memory_examples :: fn() !void {
    allocator := std.mem.page_allocator
    
    // Allocate buffers
    source := try allocator.alloc(u8, 100)
    dest := try allocator.alloc(u8, 100)
    defer {
        allocator.free(source)
        allocator.free(dest)
    }
    
    // Initialize source
    @memset(source.ptr, 42, source.len)
    
    // Copy data
    @memcpy(dest.ptr, source.ptr, source.len)
    
    // Compare memory
    equal := @memcmp(source.ptr, dest.ptr, source.len) == 0
    std.debug.print("Buffers equal: {}", .{equal})
}
```

### Bit Manipulation

```rust
bit_examples :: fn() void {
    value : u32 = 0xF0F0F0F0
    
    // Count bits
    leading_zeros := @clz(u32, value)     // Count leading zeros
    trailing_zeros := @ctz(u32, value)    // Count trailing zeros
    set_bits := @popCount(u32, value)     // Count set bits
    
    // Byte operations
    swapped := @byteSwap(u32, value)      // Reverse byte order
    
    // Bit operations
    byte_val : u8 = 0xF0
    bit_reversed := @bitReverse(u8, byte_val)  // Reverse bit order
    
    std.debug.print("Original: 0x{X}, Swapped: 0x{X}", .{value, swapped})
}
```

## Implementation Status

- ✅ **Module system** - `@import()` parsing and basic handling
- 🚧 **Type introspection** - `@TypeOf`, `@sizeOf` being implemented  
- ⏳ **Safe arithmetic** - Error handling infrastructure needed
- ⏳ **Type casting** - Basic casting operations
- ⏳ **SIMD operations** - Vector type support
- ⏳ **Memory operations** - Low-level memory functions
- ⏳ **Bit operations** - Bit manipulation utilities

## Error Types

Built-in functions that can fail return these error types:

```rust
// Arithmetic errors
error.Overflow      // Value too large for target type
error.Underflow     // Value too small for target type  
error.DivisionByZero // Attempted division by zero

// Casting errors
error.ValueTooLarge // Value doesn't fit in target type
error.InvalidCast   // Invalid type conversion

// Memory errors
error.OutOfMemory   // Memory allocation failed
error.InvalidAlign  // Invalid alignment requirement
```
