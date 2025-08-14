# Standard Library

## Batteries Included

The target is that the standard library has all the features you need for development. Howl's standard library is designed to be comprehensive, well-organized, and easy to use.

## Module Structure

The standard library is organized into logical modules for easy discovery and use:

```
std/
├── debug/       - Debug printing and assertions
├── mem/         - Memory management and allocators  
├── math/        - Mathematical functions and constants
├── io/          - Input/output operations
├── string/      - String manipulation utilities
├── collections/ - Data structures (arrays, maps, lists)
├── fmt/         - Formatting and serialization
├── os/          - Operating system interfaces
├── net/         - Network operations
├── crypto/      - Cryptographic functions
├── json/        - JSON parsing and generation
├── xml/         - XML parsing and generation
└── testing/     - Unit testing framework
```

## Module Import System

Modules can be imported using the `@import()` builtin function:

```rust
// Import entire std module
std :: @import("std")

// Import specific submodules
debug :: @import("std/debug")
math :: @import("std/math")
collections :: @import("std/collections")

// Import from external files
my_module :: @import("./my_module.howl")
utils :: @import("../utils/helpers.howl")
```

## Core Modules

### std/debug

Debug utilities for development and testing:

```rust
debug :: @import("std/debug")

main :: fn() !void {
    // Print with formatting
    debug.print("Hello, {}!", .{"World"})
    
    // Assert conditions
    debug.assert(true, "This should never fail")
    
    // Debug-only code (removed in release builds)
    debug.warn("This is a warning message")
}
```

### std/mem

Memory management utilities:

```rust
mem :: @import("std/mem")

main :: fn() !void {
    // Allocator interface
    allocator := mem.page_allocator
    
    // Allocate memory
    buffer := try allocator.alloc(u8, 1024)
    defer allocator.free(buffer)
    
    // Memory utilities
    mem.set(u8, buffer, 0)  // Zero the buffer
    mem.copy(u8, dest, src) // Copy memory
}
```

### std/math

Mathematical functions and constants:

```rust
math :: @import("std/math")

calculate :: fn() f64 {
    // Mathematical constants
    pi := math.pi
    e := math.e
    
    // Basic functions
    result := math.sqrt(16.0)     // 4.0
    angle := math.sin(math.pi/2)  // 1.0
    power := math.pow(2.0, 8.0)   // 256.0
    
    return result + angle + power
}
```

### std/collections

Data structures for common use cases:

```rust
collections :: @import("std/collections")

main :: fn() !void {
    // Dynamic array
    list := collections.ArrayList(i32).init(allocator)
    defer list.deinit()
    
    try list.append(42)
    try list.append(24)
    
    // Hash map
    map := collections.HashMap([]const u8, i32).init(allocator)
    defer map.deinit()
    
    try map.put("answer", 42)
    value := map.get("answer") // Optional(42)
}
```

### std/fmt

Formatting and string manipulation:

```rust
fmt :: @import("std/fmt")

main :: fn() !void {
    // Format to string
    result := try fmt.allocPrint(allocator, "Answer: {}", .{42})
    defer allocator.free(result)
    
    // Parse from string
    number := try fmt.parseInt(i32, "123", 10) // 123
    
    // String utilities
    uppercase := try fmt.toUpper(allocator, "hello") // "HELLO"
    defer allocator.free(uppercase)
}
```

## Builtin Function Integration

The standard library works seamlessly with builtin functions:

```rust
std :: @import("std")

type_info :: fn() void {
    value : i32 = 42
    
    // Get type information
    T :: @TypeOf(value)           // i32
    size :: @sizeOf(T)            // 4
    
    // Use with standard library
    std.debug.print("Type: {}, Size: {} bytes", .{T, size})
}

safe_operations :: fn() !void {
    a : u8 = 200
    b : u8 = 100
    
    // Safe arithmetic with error handling
    result := @add_s(a, b) catch |err| switch (err) {
        error.Overflow => {
            std.debug.print("Overflow detected!", .{})
            return err
        }
    }
    
    std.debug.print("Result: {}", .{result})
}
```

## Module Definition

Modules are regular Howl files that export public symbols:

```rust
// math_utils.howl
pub PI : f64 : 3.14159265359

pub square :: fn(x: f64) f64 {
    return x * x
}

pub circle_area :: fn(radius: f64) f64 {
    return PI * square(radius)
}

// Private helper (not exported)
validate_positive :: fn(x: f64) bool {
    return x > 0.0
}
```

```rust
// main.howl
math_utils :: @import("./math_utils.howl")

main :: fn() !void {
    area := math_utils.circle_area(5.0)
    std.debug.print("Circle area: {}", .{area})
}
```

## Standard Library Goals

1. **Comprehensive**: Cover all common programming needs
2. **Consistent**: Uniform APIs and naming conventions
3. **Performance**: Optimized implementations for critical paths
4. **Safety**: Memory-safe and error-aware by default
5. **Cross-platform**: Work across all supported targets
6. **Well-documented**: Clear examples and documentation
7. **Modular**: Import only what you need

## Implementation Status

- ✅ **Module system design** - Architecture defined
- 🚧 **Core builtins** - `@import`, type introspection functions
- 🚧 **std/debug** - Basic printing functionality
- ⏳ **std/mem** - Memory management utilities
- ⏳ **std/math** - Mathematical functions
- ⏳ **std/collections** - Data structures
- ⏳ **std/fmt** - Formatting utilities