# Enum and Tagged Unions

Howl provides powerful enum types and tagged unions for type-safe data modeling.

## Enums

```rust
// Basic enum
const Color = enum {
    Red,
    Green,
    Blue,
}

// Enum with explicit values
const HttpStatus = enum(u16) {
    OK = 200,
    NotFound = 404,
    ServerError = 500,
}

// Using enums
fn process_color(color: Color) void {
    match color {
    | .Red => std.debug.print("Processing red", .{})
    | .Green => std.debug.print("Processing green", .{})
    | .Blue => std.debug.print("Processing blue", .{})
    }
}

// Getting the raw value of an enum
fn get_status_code(status: HttpStatus) u16 {
    return @enumToInt(status)
}

// Creating an enum from a raw value
fn status_from_code(code: u16) !HttpStatus {
    return std.meta.intToEnum(HttpStatus, code) catch error.InvalidStatusCode
}
```

## Tagged Unions

Tagged unions combine an enum with associated values for each variant:

```rust
// Tagged union using an existing enum
const Value = tag(enum) {
    Integer: i64,
    Float: f64,
    String: str,
    Boolean: bool,
}

// Creating and using tagged union values
fn example() void {
    let int_val = Value{.Integer = 42}
    let str_val = Value{.String = "hello"}

    // Pattern matching on tagged unions
    match int_val {
    | .Integer(i) => std.debug.print("Got integer: {d}", .{i})
    | .Float(f) => std.debug.print("Got float: {f}", .{f})
    | .String(s) => std.debug.print("Got string: {s}", .{s})
    | .Boolean(b) => std.debug.print("Got boolean: {}", .{b})
    }
}

// If you know the type for sure you can enforce it
// not recommended
pub fn enforce(val : Value) void {
    std.debug.assert(val == .String)

    // forcefully coerce the value of val as a string
    const strVal = val.String
}
```

## Using Tagged Unions for Error Handling

Tagged unions are particularly useful for error handling:

```rust
const ParseResult = tag(enum) {
    Success: i64,
    InvalidFormat: str,
    OutOfRange: struct {
        value: i64,
        min: i64,
        max: i64,
    },
}

fn parse_number(input: str) ParseResult {
    // Check for invalid format
    if (!is_numeric(input)) {
        return ParseResult{.InvalidFormat = "Input must contain only digits"}
    }
    
    // Convert to integer
    const value = std.fmt.parseInt(i64, input, 10) catch |err| {
        return ParseResult{.InvalidFormat = "Failed to parse: {err}"}
    }
    
    // Check range
    const min: i64 = -1000
    const max: i64 = 1000
    if (value < min or value > max) {
        return ParseResult{.OutOfRange = .{
            .value = value,
            .min = min,
            .max = max,
        }}
    }
    
    // Success case
    return ParseResult{.Success = value}
}

// Usage
fn handle_user_input(input: str) void {
    const result = parse_number(input)
    
    match result {
    | .Success(value) => {
        std.debug.print("Successfully parsed: {d}", .{value})
    }
    | .InvalidFormat(msg) => {
        std.debug.print("Invalid format: {s}", .{msg})
    }
    | .OutOfRange(info) => {
        std.debug.print("Value {d} is out of range [{d}, {d}]", 
            .{info.value, info.min, info.max})
    }
    }
}
```
