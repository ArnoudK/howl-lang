# Error Handling Mechanisms

Howl provides several ergonomic error handling mechanisms:

## `catch` Expression

Handles errors inline and can provide fallback values:

```rust
const std = @import("std")
const print_d = std.debug.print
fn catch_example() void {
    // Simple error handling
    throwingFunction() catch |e| {
        print_d("Error occurred: {e}", .{e})
        }

    // Providing a fallback value
    let val: str = throwingFunction2() catch |e| {
        std.debug.print("Failed with error: {e}", .{e})
        return "fallback"
    }

    // or match the error
    let val: str = throwingFunction() catch |e| {
        return match e {
        | .error2 =>{ return "Error 2"}
        | .error3 =>{ return "Error 3"}
        }
    }


    // Abbreviated form for common cases
    let result = getValue() catch "default"

    // else where
    const ValueError = error {
        MySpecificError,
        OtherError1,
        OtherError2,
    }
    pub fn valueErrorOrNull() ValueError!?Value

    // Use match to handle errors
    match valueErrorOrNull() {
    | Value => |val| {} // success value it's captured with |val|
    | None => {} // no value
    | error.MySpecificError => {} // special error value
    | anyerror => {} // all other errors
    }
}
```

## `try` Expression

Shorthand for propagating errors to the caller:

```rust
fn try_example() !void {
    // If this function fails, propagate its error to our caller
    try throwingFunction()

    // Capture successful return value, propagate error on failure
    let result = try getValueFunction()
    std.debug.print("Got: {any}", .{result})
}
```

## `defer` Statement

Ensures cleanup code runs when leaving scope, regardless of how the scope is exited:

```rust
fn defer_example() !void {
    var resource = try acquireResource()
    defer releaseResource(resource)

    // Multiple defers execute in reverse order (LIFO)
    var resource2 = try acquireAnotherResource()
    defer releaseAnotherResource(resource2) // This runs first

    // Work with resources...
}
```

## `errdefer` Statement

Like `defer`, but only executes if the scope is exited with an error:

```rust
const File = std.File
fn create_file(path: str) !void {
    const file_option : File.OptionFlags = .create
    let file = try File.open(path, file_option)
    errdefer File.deleteFile(path) // Only runs if an error occurs later
    defer file.close()

    try writeToFile(file, "content")
    // On success: file remains and is closed
    // On error: file is deleted after being closed
}
```

## `orelse` Expression

Provides a fallback value for `None` values in optional types:

```rust
// If getTOrNull() returns None, use the default value instead
let myValue: T = getTOrNull() orelse T.default()

// Can be chained with other error handling
let result = (try getValue()) orelse default_value
```

## Defining Error Sets

```rust
// Define a custom error set
const FileError = error {
    NotFound,
    AccessDenied,
    DiskFull,
    InvalidFormat,
}

// Function that returns a specific error set
fn readConfig(path: str) FileError!Config {
    if (!fileExists(path)) {
        return FileError.NotFound
    }
    
    // Read file contents
    // Process configuration
    
    return config
}

// Using the function
fn loadSettings() !void {
    const config = readConfig("settings.conf") catch |err| {
        match err {
        | FileError.NotFound => {
            std.debug.print("Config file not found, using defaults", .{})
            return Config.default()
        }
        | FileError.AccessDenied => {
            std.debug.print("Access denied to config file", .{})
            return err // Re-propagate this error
        }
        | else => {
            std.debug.print("Unexpected error: {e}", .{err})
            return err
        }
        }
    }
    
    // Use the config...
}
```

## Error Handling Best Practices

1. **Be explicit about errors** - Use specific error sets to document what can go wrong
2. **Handle errors at the appropriate level** - Don't catch errors too early or too late
3. **Provide meaningful error information** - Include context about what went wrong
4. **Use `defer` for cleanup** - Ensure resources are released even when errors occur
5. **Prefer `match` for complex error handling** - It makes control flow more explicit
6. **Consider using `errdefer` for cleanup on error paths** - Keeps success and error handling clean
