# Functions

Functions are first-class citizens in Howl with powerful capabilities for error handling.

## Basic Function Syntax

```rust
fn my_function(parameter: paramtype) returntype {
    // function body
}
```

## Methods with Self Parameter

When the first parameter is named `self`, the function becomes a method that can be called using dot notation:

```rust
const Point = struct {
    x: isize,
    y: isize,

    // Method with self parameter
    fn distance(self: Self, other: Self) isize {
        const dx = self.x - other.x
        const dy = self.y - other.y
        return std.math.sqrt(dx*dx + dy*dy)
    }
}

// Usage:
let p1 = .Point{.x = 0, .y = 0}
let p2 = .Point{.x = 3, .y = 4}
let dist = p1.distance(p2) // 5
```

## Pipe Operator

The pipe operator (`|>`) allows for functional-style chaining of operations:

```rust
const std = @import("std")
const sqrt = std.math.sqrt

const Point = struct {
    x: isize,
    y: isize,

    // Constructor
    fn init(x: isize, y: isize) Self {
        return .{.x = x, .y = y}
    }

    // Calculate difference between points
    fn diff(self: Self, other: Self) Self {
        return .{
            .x = other.x - self.x,
            .y = other.y - self.y,
        }
    }

    // Calculate length from origin
    fn length(self: Self) isize {
        return sqrt(self.x*self.x + self.y*self.y)
    }

    // Calculate distance using the pipe operator
    fn distance(self: Self, other: Self) isize {
        return self.diff(other) |> length
        // Equivalent to: length(self.diff(other))
    }
}
```

## Error Handling in Functions

Howl makes error handling explicit with the `!` syntax:

```rust
// Function that may return any error type
fn read_file(path: str) !str {
    if (path.len == 0) return error.EmptyPath
    // Implementation...
}

// Function returning either a specific error or a string
fn validate_input(input: str) ValidationError!str {
    if (input.len < 3) return ValidationError.TooShort
    return input
}
```

## Function Parameters

### Default Parameters

```rust
fn greet(name: str, greeting: str = "Hello") void {
    std.debug.print("{s}, {s}!", .{greeting, name})
}

// Usage
greet("Alice") // "Hello, Alice!"
greet("Bob", "Welcome") // "Welcome, Bob!"
```

### Named Parameters

```rust
fn configure(options: struct {
    width: u32 = 800,
    height: u32 = 600,
    fullscreen: bool = false,
}) void {
    // Implementation using options.width, etc.
}

// Usage
configure(.{}) // Use all defaults
configure(.{.width = 1024}) // Override just width
configure(.{.fullscreen = true, .height = 768}) // Multiple overrides
```

## Higher-Order Functions

Functions can be passed as parameters and returned from other functions:

```rust
// Function that takes a function as a parameter
fn map(items: []i32, transform: fn(i32) i32) []i32 {
    var result = std.ArrayList(i32).init()
    defer result.deinit()
    
    for (items) |item| {
        result.append(transform(item))
    }
    
    return result.toOwnedSlice()
}

// Usage
fn double(x: i32) i32 {
    return x * 2
}

// Map a list of numbers to their doubled values
let numbers = [_]i32{1, 2, 3, 4}
let doubled = map(&numbers, double) // [2, 4, 6, 8]
```
