## Types in HowlLang

HowlLang provides a comprehensive type system designed for safety, performance, and expressiveness. The type system includes primitive types, composite types, and advanced features like compile-time generics and memory allocation strategies.

### `struct`

To create a standard type you can use the `struct` keyword.

Example:

```rust
myType2 :: struct {
    name : str
    job : str
}

myType :: struct {
    field: f32
    field2: str
    field3: myType2
}
```

#### Using struct as a return Type

Struct can also be used as the `return` of a function

Example:

```rust
createLinkedListType :: fn(T: Type) Type {
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

Howl includes a comprehensive set of primitive types organized by category:

| Category                 | Types                              | Description                                          |
| :----------------------- | :--------------------------------- | :--------------------------------------------------- |
| **Integer Types**        | `i8`, `i16`, `i32`, `i64`, `i128`  | Signed integers of various bit widths                |
|                          | `u8`, `u16`, `u32`, `u64`, `u128`  | Unsigned integers of various bit widths              |
| **Floating Point Types** | `f16`, `f32`, `f64`, `f80`, `f128` | IEEE-754 floating point numbers                      |
| **Character Types**      | `u8`                               | ASCII character (8-bit)                              |
| **Boolean**              | `bool`                             | Logical values: `true` or `false`                    |
| **Unit Type**            | `void`                             | Represents no value (function return)                |
| **Special Purpose**      | `None`                             | Represents the absence of a value                    |
|                          | `Some`                             | Represents a value that is not None and not an Error |
|                          | `noreturn`                         | Indicates a function never returns normally          |
| **String Types**         | `str`                              | Read-only UTF-8 string slice                         |
|                          | `strb`                             | String builder for efficient manipulation            |
| **Raw Types**            | `*T`                               | Raw pointer to type T                                |
|                          | `[*]T`                             | Many-item pointer (C-style array)                    |

### Special Types

| Type             | Description                                           | Example Usage                            |
| :--------------- | :---------------------------------------------------- | :--------------------------------------- |
| `comptime_int`   | Compile-time integer with extended precision          | `COUNT: comptime_int = 1000`             |
| `comptime_float` | Compile-time floating point with extended precision   | `PI: comptime_float = 3.1415`            |
| `usize`          | Platform-specific sized unsigned integer (32/64 bits) | `size_of :: fn(slice: []u8) usize {...}` |
| `isize`          | Platform-specific sized signed integer (32/64 bits)   | `diff: isize = end - start`              |
| `Type`           | Type of types (metatype)                              | `T: Type = i32`                          |
| `AnyError`       | Set of all possible error values                      | `err: anyerror = MyError.InvalidInput`   |

### Composite Types

| Type            | Description                                          | Example Usage                                         |
| :-------------- | :--------------------------------------------------- | :---------------------------------------------------- |
| `?T`            | Optional type - either a value of type `T` or `None` | `maybe: ?i32 = None`                                  |
| `E!T`           | Error union - either a value of type `T` or an error | `read :: fn() !str {...}`                             |
| `[N]T`          | Fixed-size array of N elements of type T             | `buffer: [1024]u8 = undefined`                        |
| `[]T`           | Slice - pointer and length to elements of type T     | `slice: []const u8 = "hello"`                         |
| `@Vector(N, T)` | SIMD vector of N elements of type T                  | `vec: @Vector(4, f32) = .{1,2,3,4}`                   |
| `enum`          | Enumeration type with named values                   | `Color :: enum { Red, Green, Blue }`                  |
| `union`         | Tagged union type                                    | `Value :: union(enum) { int: i32, str: []const u8 }`  |
| `struct`        | Structure with named fields                          | `Point :: struct { x: f32, y: f32 }`                  |
| `trait`         | Interface defining required methods and types        | `Drawable :: trait { draw :: fn(Self, Canvas) void }` |
| `impl`          | Implementation of traits for types                   | `Point :: impl Display { ... }`                       |
| `*dyn Trait`    | Trait object for dynamic dispatch                    | `drawable: *dyn Drawable = &circle`                   |

### Generic-like Behavior

Howl achieves generics through compile-time parameters and type functions, providing zero-cost abstractions:

#### Compile-time Type Parameters

```rust
// Generic function with compile-time type parameter
max :: fn(comptime T: Type, a: T, b: T) T {
    return if (a > b) a else b
}

// Usage - type is inferred
result_int :: max(i32, 10, 20)      // Returns i32
result_float :: max(f64, 3.14, 2.71) // Returns f64
```

#### Type-Generating Functions

```rust
// Container factory function
Container :: fn(comptime T: type) type {
    return struct {
        data: ?T,

        init :: fn() Self {
            return .{ .data = None }
        }

        set :: fn(self: *Self, value: T) void {
            self.data = value
        }

        get :: fn(self: Self) ?T {
            return self.data
        }

        map :: fn(self: Self, comptime U: type, func: fn(T) U) Container(U) {
            if (self.data) |value| {
                return Container(U){ .data = func(value) }
            }
            return Container(U).init()
        }
    }
}

// Create specialized types
IntContainer :: Container(i32)
StringContainer :: Container([]const u8)

// Usage
int_box := IntContainer.init()
int_box.set(42)

// Define a Container trait
Container :: trait {
    type Element,

    get :: fn(Self) ?Element,
    set :: fn(*Self, Element) void,
    is_empty :: fn(Self) bool,
}

// Generic function using trait constraints - clean syntax
process_container :: fn(container: C :: Container) void {
    if (container.get()) |value| {
        std.debug.print("Container value: {any}\n", .{value})
    } else {
        std.debug.print("Empty container\n", .{})
    }
}
```

#### Trait System

```rust
// Define a trait
Display :: trait {
    display :: fn(Self) str,
}

// Implement for existing types
i32 :: impl Display {
    display :: fn(self: i32) str {
        return std.fmt.format("{d}", .{self})
    }
}

// Struct with trait implementation
Point :: struct impl Display {
    x: f32,
    y: f32,

    display :: fn(self: Self) str {
        return std.fmt.format("({d}, {d})", .{self.x, self.y})
    }
}

// Generic function with trait constraint
print_value :: fn(value: T :: Display) void {
    std.debug.print("{s}\n", .{T.display(value)})
}

// Usage
point := Point{ .x = 1.0, .y = 2.0 }
print_value(point)  // Works because Point implements Display
print_value(42)     // Works because i32 implements Display
```

### Trait Features

#### Multiple Traits

```rust
// Multiple traits on one type
Person :: struct impl Display + Clone {
    name: str,
    age: u32,

    display :: fn(self: Self) str {
        return std.fmt.format("{s} ({})", .{self.name, self.age})
    }

    clone :: fn(self: Self) Self {
        return Self{ .name = self.name, .age = self.age }
    }
}

// Function requiring multiple traits
process :: fn(item: T :: Display + Clone) T {
    std.debug.print("Processing: {s}\n", .{T.display(item)})
    return T.clone(item)
}
```

#### Conditional Implementation

```rust
// Implement trait only if condition is met
T :: impl Display if T :: Debug {
    display :: fn(self: T) str {
        return T.debug(self)
    }
}
```

#### Trait Objects (Dynamic Dispatch)

```rust
Drawable :: trait {
    draw :: fn(Self) void,
}

Circle :: struct impl Drawable {
    radius: f32,
    draw :: fn(self: Self) void { ... }
}

Square :: struct impl Drawable {
    size: f32,
    draw :: fn(self: Self) void { ... }
}

// Can hold any type that implements Drawable
shapes := [_]*dyn Drawable{ &Circle{.radius = 5}, &Square{.size = 10} }
for (shapes) |shape| {
    shape.draw()  // Dynamic dispatch
}
```

### Why Traits?

Traits provide better autocompletions and easier type-checking

```rust
process :: fn(value: AnyType) void {
    comptime {
        switch (@typeInfo(@TypeOf(value))) {
            .Struct => if (@hasDecl(@TypeOf(value), "process")) value.process(),
            else => @compileError("Expected processable type"),
        }
    }
}

Processable :: trait {
    process :: fn(Self) void,
}

process :: fn(value: Processable) void {
    T.process(value)  // Guaranteed to exist
}
```
