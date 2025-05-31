# Functions

Functions are first-class citizens in Howl with powerful capabilities for error handling.

## Basic Function Syntax

```rust
my_function :: fn(parameter: paramtype) returntype {
    // function body
}
```

## Methods with Self Parameter

When the first parameter is named `self`, the function becomes a method that can be called using dot notation:

```rust
Point :: struct {
    x, y: isize,
}

// Method with self parameter  
distance :: fn(self, other: Point) isize {
    dx := self.x - other.x
    dy := self.y - other.y
    return std.math.sqrt(dx*dx + dy*dy)
}

// Usage:
p1 := Point{.x = 0, .y = 0}
p2 := Point{.x = 3, .y = 4}
dist := p1.distance(p2) // 5
```

## Pipe Operator

The pipe operator (`|>`) allows for functional-style chaining of operations:

```rust
std := @import("std")
sqrt := std.math.sqrt

Point :: struct {
    x, y: isize,
}

// Constructor
init :: fn(x, y: isize) Point {
    return Point{.x = x, .y = y}
}

// Calculate difference between points
diff :: fn(self, other: Point) Point {
    return Point{
        .x = other.x - self.x,
        .y = other.y - self.y,
    }
}

// Calculate length from origin
length :: fn(self: Point) isize {
    return sqrt(self.x*self.x + self.y*self.y)
}

// Calculate distance using the pipe operator
distance :: fn(self, other: Point) isize {
    return self.diff(other) |> length
    // Equivalent to: length(self.diff(other))
}
```

## Error Handling in Functions

Howl makes error handling explicit with the `!` syntax:

```rust
// Function that may return any error type
read_file :: fn(path: str) !str {
    if path.len == 0 return error.EmptyPath
    // Implementation...
}

// Function returning either a specific error or a string
validate_input :: fn(input: str) ValidationError!str {
    if input.len < 3 return ValidationError.TooShort
    return input
}
```

## Function Parameters

### Default Parameters

```rust
greet :: fn(name: str, greeting := "Hello") void {
    std.debug.print("{s}, {s}!", .{greeting, name})
}

// Usage
greet("Alice") // "Hello, Alice!"
greet("Bob", "Welcome") // "Welcome, Bob!"
```

### Named Parameters

```rust
configure :: fn(options: struct {
    width := 800,
    height := 600,
    fullscreen := false,
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
map :: fn(items: []i32, transform: fn(i32) i32) []i32 {
    result := std.ArrayList(i32).init()
    defer result.deinit()
    
    for item in items {
        result.append(transform(item))
    }
    
    return result.toOwnedSlice()
}

// Usage
double :: fn(x: i32) i32 {
    return x * 2
}

// Map a list of numbers to their doubled values
numbers := [_]i32{1, 2, 3, 4}
doubled := map(&numbers, double) // [2, 4, 6, 8]
```

## Function Values

Functions are normal values that can be assigned to variables, stored in data structures, and passed around like any other value:

### Function Types

Function types use the `fn` keyword followed by parameter and return types:

```rust
// Function type declarations
BinaryOp :: fn(i32, i32) i32
Predicate :: fn(i32) bool
ErrorHandler :: fn(str) !void
```

### Function Assignment

Functions are just values - assign them like any other variable:

```rust
// Import functions from other modules
pow := @import("std").math.pow
sqrt := @import("std").math.sqrt

// Reference existing functions
add_impl :: fn(a, b: i32) i32 {
    return a + b
}
add := add_impl  // Just assign the function

// Anonymous functions when needed
multiply := fn(x, y: i32) i32 {
    return x * y
}

// Single expression functions
square := fn(x: i32) i32 => x * x
is_even := fn(n: i32) bool => n % 2 == 0
```

### Functions in Data Structures

Functions are normal values, so they work naturally in arrays and structs:

```rust
// Array of functions
operations := [_]BinaryOp{ add, subtract, multiply }

// Struct with function fields
Calculator :: struct {
    add: fn(i32, i32) i32,
    subtract: fn(i32, i32) i32,
    multiply: fn(i32, i32) i32,
}

// Simple assignment
calc := Calculator{
    .add = add,
    .subtract = subtract, 
    .multiply = multiply,
}
```

### Closures and Capture

Functions can capture variables from their enclosing scope:

```rust
make_adder :: fn(base: i32) fn(i32) i32 {
    return fn(x: i32) i32 => base + x  // Captures 'base'
}

// Usage
add_ten := make_adder(10)
result := add_ten(5) // 15
```

### Function Composition

Since functions are just values, composition is straightforward:

```rust
// Higher-order functions
compose :: fn(f: fn(i32) i32, g: fn(i32) i32) fn(i32) i32 {
    return fn(x: i32) i32 => f(g(x))
}

// Usage with existing functions
add_one := fn(x: i32) i32 => x + 1
multiply_two := fn(x: i32) i32 => x * 2
add_then_multiply := compose(multiply_two, add_one)

// Import and compose
sin := @import("std").math.sin
cos := @import("std").math.cos
sin_of_cos := compose(sin, cos)
```

### Method References

Reference methods as function values using simple assignment:

```rust
Point :: struct {
    x, y: i32,
}
    
distance_from_origin :: fn(self: Point) f64 {
    return std.math.sqrt(self.x * self.x + self.y * self.y)
}

// Reference methods as functions
distance_fn := Point.distance_from_origin  // fn(Point) f64

// Usage
p := Point{.x = 3, .y = 4}
dist := distance_fn(p)  // 5.0

// Method calls are just syntactic sugar for function calls
dist2 := p.distance_from_origin()  // Same as distance_fn(p)
```
