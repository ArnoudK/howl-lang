# SIMD and Vectors

Howl provides vector types for efficient SIMD (Single Instruction, Multiple Data) operations.

## Creating Vector Types

Create a vector type using `@Vector`:

```rust
// Create a vector type of 4 f32 values
Vec4f32 :: @Vector(4, f32)

// Create a vector type of 8 u8 values
Vec8u8 :: @Vector(8, u8)
```

## Vector Operations

Vector arithmetic operations apply to each element separately:

| Operator | Description                      | Note                                        | Example (using @Vector(3, i32))                 |
| :------- | :------------------------------- | :------------------------------------------ | :---------------------------------------------- |
| `+`      | Add the values element wise      | -                                           | `[-1, 1, 0] + [2, 1, 0] = [1, 2, 0]`           |
| `-`      | Subtract the values element wise | -                                           | `[2, 3, 4] - [2, 4, -1] = [0, -1, 5]`           |
| `*`      | Multiply element wise            | -                                           | `[2, 3, 4] * [3, 2, 1] = [6, 6, 4]`             |
| `/`      | Divide                           | Divide by zero can have unexpected behavior | `[6, 8, 9] / [2, 2, 3] = [3, 4, 3]`             |
| `%`      | Modulo                           | Only valid on integer types                 | `[5, 9, 10] % [2, 4, 3] = [1, 1, 1]`            |
| `==`     | Equal to                         | Returns a boolean @Vector of the length     | `[1, 2, 3] == [1, 2, 3] = [true, true, true]`   |
| `!=`     | Not equal                        | Return a boolean @Vector                    | `[1, 2, 3] != [0, 2, 3] = [true, false, false]` |
| `<`      | Less than                        | -                                           | `[1, 2, 3] < [2, 2, 2] = [true, false, false]`  |
| `>`      | Greater than                     | -                                           | -                                               |
| `<=`     | Less or equal                    | -                                           | -                                               |
| `>=`     | Greater than or equal            | -                                           | -                                               |
| `bOr`    | Bitwise or                       | Only valid on integer types                | `[1, 2, 3] bOr [2, 1, 4] = [3, 3, 7]`          |
| `bXor`   | Bitwise xor                      | Only valid on integer types                | `[1, 3, 5] bXor [3, 3, 4] = [2, 0, 1]`         |
| `bAnd`   | Bitwise and                      | Only valid on integer types                | `[7, 2, 5] bAnd [3, 3, 4] = [3, 2, 4]`         |
| `bNot`   | Bitwise not                      | Only valid on integer types                | `bNot [0, 1, -1] = [-1, -2, 0]`                |
| `<<`     | Bitwise shift left               | Only valid on integer types                | `[1, 2, 3] << [2, 1, 0] = [4, 4, 3]`           |
| `>>`     | Bitwise shift right              | Only valid on integer types                | `[8, 4, 3] >> [1, 2, 0] = [4, 1, 3]`           |

## Vector Examples

### Basic Vector Operations

```rust
std :: @import("std")
Vec4f32 :: @Vector(4, f32)

vector_basics :: fn() void {
    // Initialize vectors
    v1 :: Vec4f32{1.0, 2.0, 3.0, 4.0}
    v2 :: Vec4f32{5.0, 6.0, 7.0, 8.0}
    
    // Element-wise operations
    sum :: v1 + v2        // [6.0, 8.0, 10.0, 12.0]
    product :: v1 * v2    // [5.0, 12.0, 21.0, 32.0]
    
    // Element access (via array indexing)
    first :: v1[0]        // 1.0
    
    // Conditionals return boolean vectors
    greater :: v2 > v1    // [true, true, true, true]
    
    // Check if any element meets a condition
    any_greater_than_6 :: @reduce(.Or, v2 > Vec4f32{6.0, 6.0, 6.0, 6.0})
    // any_greater_than_6 = true
}
```

### Computing Dot Product

```rust
Vec4f32 :: @Vector(4, f32)

dotProduct :: fn(a: Vec4f32, b: Vec4f32) f32 {
    // Element-wise multiplication
    multiplied :: a * b // [a[0]*b[0], a[1]*b[1], a[2]*b[2], a[3]*b[3]]
    
    // Sum all elements
    sum : f32 = 0
    for (0..4) |i| {
        sum += multiplied[i]
    }
    
    // Alternative using vector reduction (more efficient)
    // let sum = @reduce(.Add, multiplied)
    
    return sum
}

example :: fn() void {
    vec1 :: Vec4f32{1.0, 2.0, 3.0, 4.0}
    vec2 :: Vec4f32{5.0, 6.0, 7.0, 8.0}

    result :: dotProduct(vec1, vec2) // 70.0
}
```

### Image Processing Example

```rust
Vec4u8 :: @Vector(4, u8)

applyBrightness :: fn(pixel: Vec4u8, brightness: i8) Vec4u8 {
    // Convert to signed for safe arithmetic
    pixel_i8 :: @bitCast(@Vector(4, i8), pixel)
    
    // Create vector filled with brightness value
    brightness_vec :: @splat(4, brightness)
    
    // Add brightness and clamp to valid u8 range
    result_i8 :: @min(@max(pixel_i8 + brightness_vec, 0), 255)
    
    // Convert back to unsigned
    return @bitCast(Vec4u8, result_i8)
}

processImageSIMD :: fn(image: []Vec4u8, brightness: i8) void {
    for (image) |*pixel| {
        pixel.* = applyBrightness(pixel.*, brightness)
    }
}
```

## Performance Considerations

- Vector operations map directly to CPU SIMD instructions when available
- Vector width should match hardware capabilities for best performance
- Align vector data properly to avoid performance penalties
- Use `@prefetch` for memory access optimization when appropriate
- Consider using vectors for numeric algorithms, graphics, and signal processing

## Hardware-Specific Optimizations

```rust
// Choose vector size based on target architecture
std :: @import("std")
optimal_vector_size :: if (std.Target.current.cpu.arch == .x86_64) 
                              32 // AVX-256 (32 bytes)
                           else if (std.Target.current.cpu.arch == .arm)
                              16 // NEON (16 bytes)
                           else
                              16;

// Create a float vector with the optimal size
OptimalFloatVec :: @Vector(optimal_vector_size / @sizeOf(f32), f32);
```
