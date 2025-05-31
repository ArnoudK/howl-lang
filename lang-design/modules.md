# Module System

Howl organizes code into modules for better maintainability and reusability.

## Exporting Declarations

The `pub` keyword makes declarations visible outside the current module:

```rust
// Exported and accessible from other modules
pub VERSION :: "1.0.0"

// Private to this module
INTERNAL_ID :: "xyz123"

// Exported function
add :: (a: i32, b: i32) i32 {
    return a + b
}
```

## Importing Modules

Use `@import` to access other modules:

```rust
std :: @import("std") // Standard library
math :: @import("./math.howl") // Local module

calculate :: fn() {
    result :: math.cos(std.math.PI)
    std.debug.print("Result: {d}", .{result})
}
```

## Module Organization

Howl modules follow the file system structure:

```
project/
├── src/
│   ├── main.howl   // Entry point
│   ├── utils.howl  // Utility module
│   └── math/       // Math module directory
│       ├── core.howl
│       └── advanced.howl
├── tests/
│   └── test_math.howl
└── build.howl
```

### Importing from Subdirectories

You can import from subdirectories using path notation:

```rust
// In main.howl
math_core :: @import("./math/core.howl")
math_advanced :: @import("./math/advanced.howl")

// Or import the directory to get a namespace
math :: @import("./math")
// This works if math/index.howl or math.howl exists
```

## Default Exports

You can create a default export by naming a declaration `main`:

```rust
// In utils.howl
main :: () void {
    // This becomes the default export
}

// In main.howl
utils :: @import("./utils.howl")
utils() // Calls the default export
```
