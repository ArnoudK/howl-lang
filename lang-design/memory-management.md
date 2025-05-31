# Memory Management

Howl provides flexible memory management with distinct allocation strategies for safety and performance.

## Allocation Strategies

| Strategy      | Syntax                     | Lifetime               | Use Case                       | Performance |
| :------------ | :------------------------- | :--------------------- | :----------------------------- | :---------- |
| **Automatic** | `.` prefix                 | Compiler decides       | Default choice                 | Variable    |
| **Stack**     | `~` prefix                 | Scope exit             | Small, temporary values        | Excellent   |
| **GC**        | `$` prefix                 | Managed by GC          | Shared data, complex ownership | Good        |
| **Manual**    | `#` prefix                 | Scope exit             | Function-based allocation      | Good        |
| **RefCount**  | `&` prefix                 | Last reference dropped | Shared ownership, no cycles    | Good        |
| **Atomic RC** | `` ` `` prefix             | Thread-safe ref count  | Multithreaded shared ownership | Fair        |
| **Arena**     | `.init(.arena{my_arena})`  | Until arena freed      | Batch allocations              | Excellent   |
|           | `my_arena.create(AnyType)` |                        |

## Basic Usage

```rust
// Automatic (recommended default)
user :: .User{.name = "Alice", .age = 30}

// Stack for small, local data
point :: ~Point{.x = 10, .y = 20}

// GC for shared/dynamic data
cache :: $HashMap(str, Data).init()

// Manual with scope cleanup
file :: #File.open("data.txt")

// Reference counting
config :: &Config{.timeout = 30}

// Thread-safe reference counting
shared :: `SharedState.init()

// Arena for batch operations
arena :: std.heap.ArenaAllocator.init()
defer arena.deinit()
nodes :: ArrayList(Node).init(.arena{arena})
```

## Decision Guide

**Choose by use case:**

- **Automatic (`.`)**: Default, let compiler optimize
- **Stack (`~`)**: Small data, known size, local scope
- **GC (`$`)**: Shared data, complex ownership, collections
- **Manual (`#`)**: Function-based allocation with scope cleanup
- **RefCount (`&`)**: Shared ownership without cycles (single-threaded)
- **Atomic RC (`` ` ``)**: Shared ownership (multithreaded)
- **Arena (`.init(.arena{arena})`)**: Many allocations, batch cleanup

## Advanced Specification

### Explicit Strategy Selection

```rust
list :: ArrayList(i32).init(.gc)         // GC allocation
map :: HashMap(str, Data).init(.rc)      // Reference counted
buffer :: Buffer.init(.manual, 1024)     // Manual allocation
arena_list :: ArrayList(Node).init(.arena{my_arena})  // Arena allocation
```

### Custom Type Support

```rust
MyStruct :: struct {
    init :: (strategy: @MemStrat, size: usize) !MyStruct {
        allocator :: switch (strategy) {
            .gc => std.heap.gc_allocator,
            .manual => std.heap.c_allocator,
            .arena => |a| a.allocator,
            else => @compileError("Unsupported strategy"),
        };
        // ...
    }
};

main :: () !void {
  myArena :: std.heap.arena()
  defer myArena.free()
  s :: MyStruct.init(.arena{myArena})
}

```

## Best Practices

```rust
// ✅ Good patterns
config :: .Config{.timeout = 30}           // Let compiler decide
temp :: ~Point{.x = 1, .y = 2}            // Stack for temp data
shared :: $HashMap.init()                  // GC for shared structures
file :: #File.open("data.txt")             // Manual with scope cleanup

// ❌ Avoid these patterns
huge :: ~[1_000_000]u8{0}                  // Too large for stack
bad :: fn() ~Point { return ~Point{} }           // Can't return stack data
cycle :: &Node{.parent = cycle}            // Reference cycle
```
