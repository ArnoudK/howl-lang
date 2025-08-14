# The Howl Programming Language

## Overview

Howl is a modern programming language designed for expressiveness, safety, and developer productivity. Drawing inspiration from Zig's pragmatism and OCaml's powerful type system, Howl provides a balanced approach to functional and imperative programming paradigms.

### Key Features

- **Strong static typing** with comprehensive compile-time checking and type inference
- **Expressive pattern matching** for clear and maintainable control flow
- **First-class error handling** with ergonomic propagation and recovery mechanisms
- **Flexible memory management** combining garbage collection with deterministic cleanup
- **Powerful compile-time features** for meta-programming without sacrificing readability
- **Clear separation** between runtime and compile-time computation phases

---

## Quick Start Examples

The following examples demonstrate Howl's clean and expressive syntax.

### Hello World Program

```howl
// Import the standard library
std :: @import("std")

// Main function with error handling capability
main :: () !void {
    // Constants are declared with '::'
    hello :: "Hello World!"

    // Print to debug output with formatting
    std.debug.print("{s}!!", .{hello})
    // Output: Hello World!!!
}
```

### Pattern Matching Example

Pattern matching provides elegant solutions for complex branching logic:

```rust
// Convert numeric values to descriptive strings based on ranges
quantity_string :: (a: u64) !str {
    // Match expressions evaluate conditions in order and return the first match
    return match a {
    | < 5 => "low"      // For values 0-4
    | < 10 => "medium"  // For values 5-9
    | < 15 => "high"    // For values 10-14
    | >= 15 => "plenty" // For values 15 and above
    }
}

branchingLogic :: (a: bool) void {

    match a {
    | true => func1()
    | false => func2()
    }

}
```

---

## Language Reference

For detailed documentation on language features, please refer to these specific documents:

- [Types](./type.md) - Type system, struct declarations, and generics
- [Memory Management](./memory-management.md) - Allocation strategies and memory regions
- [Structs](./structs.md) - Structure definitions and object-oriented patterns
- [Arrays and Slices](./arrays-slices.md) - Working with fixed and dynamic collections
- [Variables](./variables.md) - Variable declarations and scoping rules
- [Modules](./modules.md) - Module system and imports
- [Built-ins](./builtins.md) - Built-in functions and special operations
- [Functions](./functions.md) - Function declarations, methods, and functional patterns
- [Enums and Unions](./enums-unions.md) - Enumeration types and tagged unions
- [Control Flow](./control-flow.md) - Loops, conditionals, and pattern matching
- [Error Handling](./error-handling.md) - Error handling mechanisms
- [Operators](./operators.md) - Arithmetic, comparison, and logical operators
- [SIMD and Vectors](./simd-vectors.md) - SIMD operations and vector types
- [Style Guide](./style-guide.md) - Comments and formatting conventions
- [Keywords](./keywords.md) - Reserved keywords and their usage
