# Arrays and Slices

Howl distinguishes between fixed-size arrays and dynamic slices.

## Fixed-Size Arrays

Arrays have a fixed size known at compile time and are typically stack-allocated.
Creation of arrays use the same style as creating structures.

```rust
// Array with explicit size and type
const fixed_array: [5]u32 = .[5]u32{1, 2, 3, 4, 5}

// Array with inferred size using underscore
const inferred_array = .[_]u32{1, 2, 3, 4, 5} // Type is [5]u32
```

## Slices

Slices are references to contiguous sequences with runtime-known length.

```rust
// Function accepting a slice of u32 values
fn process(numbers: []u32) void {
    // Access the length of the slice
    std.debug.print("Processing {d} numbers", .{numbers.len})

    // Iterate through the slice with item and index
    for (numbers, 0..) |num, i| {
        std.debug.print("Element {d}: {d}", .{i, num})
    }
}

// Create a slice from an array
let array = [_]u32{1, 2, 3, 4, 5}
let slice = array[1..<4] // Contains {2, 3, 4}

// Pass the slice to a function
process(slice)
```

## Range Operators for Slicing

| Operator | Description         | Example        | Result                |
| :------- | :------------------ | :------------- | :-------------------- |
| `..`     | Full range to end   | `array[2..]`   | Elements from index 2 |
| `..<`    | Non-inclusive range | `array[1..<4]` | Elements 1, 2, and 3  |
| `..=`    | Inclusive range     | `array[1..=3]` | Elements 1, 2, and 3  |
