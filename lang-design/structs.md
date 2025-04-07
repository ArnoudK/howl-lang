# Structures and Object-Oriented Concepts

Howl provides robust structural types without traditional inheritance, focusing instead on composition and explicit interfaces.

## Structure Definition

```rust
const Person = struct {
    // Fields
    name: str,
    age: u32,

    // Method with implicitly bound 'self' parameter
    fn greet(self: Self) void {
        std.debug.print("Hello, my name is {s}", .{self.name})
    }

    // Constructor (returns Self type)
    fn init(name: str, age: u32) $Self {
        return $Self{
            .name = name,
            .age = age,
        }
    }
}
```

## Using Structures

```rust
// Create an instance using the constructor
let alice = Person.init("Alice", 30)

// Or directly using field initialization (stack-allocated)
let bob = .Person{.name = "Bob", .age = 25}

// Call methods
alice.greet() // Prints: Hello, my name is Alice
```

## Generic-like behavior

Howl uses compile-time parameters to achieve generic-like behavior without explicit generic syntax:

```rust
const std = @import("std")

// A generic-like container type
const Container = struct {
    // Create a specialized container for any type
    pub fn of(comptime T: type) type {
        return struct {
            data: ?T,

            pub fn init() Self {
                return .{.data = None}
            }

            pub fn set(self: *Self, value: T) void {
                self.data = value
            }

            pub fn get(self: Self) ?T {
                return self.data
            }
        }
    }
}

// Usage example
fn example_container() void {
    // Create specialized container types
    const IntContainer = Container.of(i32)
    const StrContainer = Container.of(str)

    // Create and use instances
    var int_box = IntContainer.init()
    int_box.set(42)

    var str_box = StrContainer.init()
    str_box.set("Hello")

    // Generic function that works with any Container
    fn printContainer(container: anytype) void {
        match container.get() {
        | Some => |val| std.debug.print("Container value: {any}\n", .{val })
        | None => std.debug.print("Empty Container: :(", .{});
        }
    }

    printContainer(int_box) // Works with IntContainer
    printContainer(str_box) // Works with StrContainer
}
```
