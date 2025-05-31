# Structures and Object-Oriented Concepts

Howl provides robust structural types without traditional inheritance, focusing instead on composition and explicit interfaces.

## Structure Definition

```rust
Person :: struct {
    // Fields
    name: str,
    age: u32,
}

// Method with implicitly bound 'self' parameter
greet :: fn(self: Person) void {
    std.debug.print("Hello, my name is {s}", .{self.name})
}

// Constructor (returns Person type)
init :: fn(name: str, age: u32) Person {
    return Person{
        .name = name,
        .age = age,
    }
}
```

## Using Structures

```rust
// Create an instance using the constructor
alice :: Person.init("Alice", 30)

// Or directly using field initialization
bob :: Person{.name = "Bob", .age = 25}

// Mutable instance
charlie := Person{.name = "Charlie", .age = 28}
charlie.age = 29  // OK: can modify fields of mutable variable

// Call methods
alice.greet() // Prints: Hello, my name is Alice
```

## Generic-like behavior

Howl uses compile-time parameters to achieve generic-like behavior without explicit generic syntax:

```rust
std :: @import("std")

// A generic-like container type
Container :: struct {
    // Create a specialized container for any type
    pub of :: fn(comptime T: AnyType) Type {
        return struct {
            data: ?T,

            pub init :: fn() Self {
                return .{.data = None}
            }

            pub set :: fn(self: *Self, value: T) void {
                self.data = value
            }

            pub get :: fn(self: Self) ?T {
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
    printContainer :: fn(container: AnyType) void {
        match container.get() {
        | Some => |val| std.debug.print("Container value: {any}\n", .{val })
        | None => std.debug.print("Empty Container: :(", .{});
        }
    }

    printContainer(int_box) // Works with IntContainer
    printContainer(str_box) // Works with StrContainer

}
```
