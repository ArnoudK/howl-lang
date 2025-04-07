# Memory Management

Howl provides flexible memory management with distinct allocation strategies, giving developers explicit control over memory while maintaining safety.

## Allocation Strategies Overview

| Strategy                      | Syntax     | Lifetime                   | Use Case                             |
| :---------------------------- | :--------- | :------------------------- | :----------------------------------- |
| Automatic                     | `.` prefix | Compiler decides           | You don't care                       |
| Garbage Collection            | `$` prefix | Managed by GC              | Complex data structures, shared data |
| Stack Allocation              | `~` prefix | Automatic at scope exit    | Small, temporary values              |
| Scoped Manual Heap Allocation | `#` prefix | Deterministic at scope end | Resource management, large objects   |

## Automatic

You use the automatic `.` prefix for standard allocations.

## Garbage-Collected Heap Allocation

Use the `$` prefix for objects that should be managed by the garbage collector. This is ideal for data with complex ownership patterns or unknown lifetimes.

```rust
// Create a heap-allocated Person using garbage collection
let person = $Person{.name = "Alice", .age = 30}

// Strings are heap-allocated by default
let message = "Hello, world!"

// Explicitly heap-allocate a mutable string builder
let builder = str.init$()
builder.append("Dynamic content")
```

### Assignment Between Memory Regions

When assigning values between different memory regions, these rules apply:

```rust
// When a stack-allocated value is assigned to a GC object field,
// the value is copied to the heap - not referenced
let point = .Point{.x = 10, .y = 20}  // Stack-allocated
let game = $Game{.player_position = point}  // Value is copied to heap

// Modifying the original doesn't affect the copy
point.x = 15  // The game.player_position.x is still 10

// Complex objects with heap-allocated fields maintain those references
let user = .User{.name = "Alice"}  // .name is already heap-allocated
let registry = $Registry{.current_user = user}  // name reference is preserved

// Manual allocations (#) assigned to GC objects ($) remain independently managed
let file = #File.open("data.txt", .{.read = true})
let processor = $Processor{.data_source = file}  // file still has deterministic cleanup
// file is freed at scope end even though referenced by processor
```

## Stack Allocation

Use the `.` prefix for efficient stack allocation of temporary values with well-defined lifetimes.

```rust
// Create a stack-allocated Point structure
let point = .Point{.x = 10, .y = 20}

// Arrays use stack allocation by default
let numbers = [5]i32{1, 2, 3, 4, 5}

// Anonymous struct literals also use stack allocation
let config = .{.width = 800, .height = 600}
```

## Scoped Manual Heap Allocation

Use the `#` prefix for resources that need deterministic cleanup regardless of garbage collection timing.

```rust
fn process_file(path: str) !void {
    // File is heap-allocated but automatically freed at end of scope
    let file = #File.open(path, .{.read = true}) catch |err| {
        std.debug.print("Failed to open file: {e}\n", .{err})
        return err
    }

    defer file.close() // Optional but recommended for clarity

    // Process file contents...
} // File is automatically freed here
```

## Memory Management Decision Guide

- **Use stack allocation** (`~` prefix) when:

  - The object has a simple lifetime limited to the current scope
  - The object is small and frequently accessed
  - You need the best performance for local operations

- **Use garbage collection** (`$` prefix) when:

  - The object's ownership is complex or shared
  - The object might outlive its creating scope
  - Simplicity of memory management is more important than deterministic cleanup

- **Use scoped manual allocation** (`#` prefix) when:

  - You need deterministic resource cleanup
  - The object represents a system resource (file, network connection)
  - You want heap allocation but with predictable cleanup timing

  **Use an arena allocator** when:

  - Performance matters a lot.
  - The lifetime and scope of usage is clear. Because you will need to deinit the arena yourself.
    Usage:

  ```rust
  const std = @import("std");
  // ArrayList of 1kb chunks
  const Chunks = std.ArrayList([1024] u8);
  const BigObject = struct {
    chunks : Chunks,
    fn initArena(arena : std.heap.Arena) Self{
        let chunks = arena.create(Chunks)
        let bigObj = arena.create(BigObject)
        bigObj.chunks = chunks
        return bigObj
    }
  }
  fn arenaExample() !void {
    let arena = std.heap.Arena.init()
    const bigObj = BigObject.initArena(arena)
    doSomeThingWithBigObject(bigObj)
    // frees all memory allocated by the arena
     arena.deinit()
  }
  ```

## Default Allocation By Type

Some types in Howl have default allocation strategies:

- `str` and `strb`: Default to garbage collection
- Fixed-size arrays: Default to stack allocation
- Dynamic collections: Default to garbage collection

You can override these defaults with explicit allocation prefixes when needed.
