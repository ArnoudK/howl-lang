## Types in HowlLang

| sdaf | asdf |
| :--- | :--- |

### `struct`

To create a standard type you can use the `struct` keyword.

Example:

```rust
myType2 :: struct {
    name : str,
    job : str
}

myType :: struct {
    field: f32,
    field2: str,
    field3: myType2,
}
```

#### Using struct as a return Type

Struct can also be used as the `return` of a function

Example:

```rust
createLinkedListType :: (T: Type) : Type {
    return struct {
        next : ?Self,
        value: T
        init :: (val: T) Self {
            return .Self {
                next: None,
                value: T
            }
        }
    }
}
```

### Programmatically creating a type

You can import the `howl` namespace from the `std` and create types using it.

### Primitive Types

Howl includes a comprehensive set of primitive types:

| Category                 | Types                              | Description                                 |
| :----------------------- | :--------------------------------- | :------------------------------------------ |
| **Integer Types**        | `i8`, `i16`, `i32`, `i64`, `i128`  | Signed integers of various bit widths       |
|                          | `u8`, `u16`, `u32`, `u64`, `u128`  | Unsigned integers of various bit widths     |
| **Floating Point Types** | `f16`, `f32`, `f64`, `f80`, `f128` | IEEE-754 floating point numbers             |
| **Boolean**              | `bool`                             | Logical values: `true` or `false`           |
| **Special Purpose**      | `None`                             | Represents the absence of a value           |
|                          | `noreturn`                         | Indicates a function never returns normally |
| **String Types**         | `str`                              | Read-only UTF-8 string                      |
|                          | `strb`                             | String builder for efficient manipulation   |

### Special Types

| Type             | Description                                           | Example Usage                       |
| :--------------- | :---------------------------------------------------- | :---------------------------------- |
| `comptime_int`   | Compile-time integer with extended precision          | `const COUNT: comptime_int = 1000`  |
| `comptime_float` | Compile-time floating point with extended precision   | `const PI: comptime_float = 3.1415` |
| `usize`          | Platform-specific sized unsigned integer (32/64 bits) | `size_of :: fn(slice: []u8) usize {}`  |
| `isize`          | Platform-specific sized signed integer (32/64 bits)   | `let diff: isize = end - start`     |
| `?T`             | Optional type - either a value of type `T` or `None`  | `let maybe: ?i32 = None`            |
| `E!T`            | Error union - either a value of type `T` or an error  | `read :: fn() !str {}`                 |

### Generic-like behavior

Howl uses compile-time parameters to achieve generic-like behavior without explicit generic syntax:

```rust
std :: @import("std")

// A generic-like container type
Container :: struct {
    // Create a specialized container for any type
    of :: (comptime T: type) type {
        return struct {
            data: ?T,

            init :: () Self {
                return .{.data = None}
            }

            set :: (self: *Self, value: T) void {
                self.data = value
            }

            get :: (self: Self) ?T {
                return self.data
            }
        }
    }
}

// Usage example
example_container :: fn() void {
    // Create specialized container types
    IntContainer :: Container.of(i32)
    StrContainer :: Container.of(str)

    // Create and use instances
    int_box := IntContainer.init()
    int_box.set(42)

    str_box := StrContainer.init()
    str_box.set("Hello")

    // Generic function that works with any Container
    printContainer :: fn(container: anytype) void {
        match container.get() {
        | Some => |val| std.debug.print("Container value: {any}\n", .{ val }) // works because print can infer the type of val
        | None => std.debug.print("Empty Container: :(", .{});
        }
    }

    printContainer(int_box) // Works with IntContainer
    printContainer(str_box) // Works with StrContainer

}
```
