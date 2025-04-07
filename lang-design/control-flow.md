# Control Flow

## Loops

Howl provides flexible looping constructs:
Common patterns include capture item(s). These are captured with `|` `|`.

```rust
// For loop with range (non-inclusive upper bound)
for (0 ..< 10) |i| {
    std.debug.print("{} ", .{i})
} // Prints: 0 1 2 3 4 5 6 7 8 9

// For loop with inclusive range
for (0 ..= 10) |i| {
    std.debug.print("{} ", .{i})
} // Prints: 0 1 2 3 4 5 6 7 8 9 10

// Loop over items in a collection
let items = [_]i32{1, 2, 3}
for (items) |item| {
    process(item)
}

// Loop with index
for (items) |item, index| {
    std.debug.print("Item {d} at position {d}", .{item, index})
}

// While loop
while (condition) {
    // loop body
}

// Infinite loop with conditional break
while (true) {
    if (should_break) break
}
```

### Iterators

Iterators typically made like this:

```rust
    const Node = struct {
        nextNode: Self,
        myValue: u32,

        pub fn iterator(self: Node) {

            return struct {
                current: ?Node,
                pub fn next() ?Self{
                    // could be done more safely if you want
                    current = current.nextNode
                    return current
                }
            }
        }

    }

    pub fn main() !void {
        let n = Node.{
            .myValue = 1,
            .nextNode = Node.{
                .myValue = 2,
                .nextNode = None,
            }
        }

        var iter = n.iterator()
        // we capture the value if it's `Some`
        // if it's none treat it as false, so we stop the loop
        for (iter.next()) |node| {
            std.debug.print("nodeval: {d}\n", .{node.myValue})
        }


    }


```

## Pattern Matching

Pattern matching is a powerful feature for expressing complex branching logic concisely:

```rust
fn match_examples() {
    let a = false

    // Simple boolean matching
    match a {
        true => function1()
        false => function2()
    }

    // Numeric range matching
    let number = 42
    match number {
        < 0 => handleNegative()
        0 => handleZero()
        > 0 and < 100 => handleSmallPositive() // multiple conditions
        100..<200 => handleLargePositive() // using range
        > 200 => handleVeryLargePositive()
    }

    // Matching on enums or variants
    let result = getResult()
    match result {
    | .success => handleSuccess()
    | .failure => handleFailure()
    | .partial => |value| handlePartial(value) // capture the result
    }
}
```

### Conditionals

Conditionals are also handled with the match statement. Only the first matching condition is handled.

```rust
let my_condition = false
let other_condition = true
match my_condition {
    true => {} // code when condition is true
    false => {
        match other_condition {
            true => {}// code when other_condition is true
            _ => {} // code when both conditions are false
        }
    }
}
```

### Matching on Anonymous Types

Matching is possible on compile-time known anonymous types.

```rust
let conditions = .{true, false}
match conditions {
    .{true, false} => {}
    .{false, false} => {}
    .{false, true} => {}
    .{true, true} => {}
}
```

### Matching Arrays and Slices

You can match on partial arrays/slices with spread patterns:

```rust
let nums = [1, 2, 3, 4, 5]
match nums {
    [0, 1, ...] => {} // Array starts with 0, 1
    [1, 2, ...] => {} // Array starts with 1, 2
    [...] => {}      // Any other array
}
```

### Matching Optional Values

```rust
fn process_optional(maybe_value: ?i32) void {
    match maybe_value {
        Some => |value| {
            std.debug.print("Got value: {d}", .{value})
        }
        None => {
            std.debug.print("No value present", .{})
        }
    }
}
```

### Matching Tagged Unions

```rust
const Message = tag(enum) {
    Text: str,
    Number: i64,
    Empty,
}

fn process_message(msg: Message) void {
    match msg {
    | .Text => |text| {
        if (text.len > 0) {
            std.debug.print("Text message: {s}", .{text})
        } else {
            std.debug.print("Empty text message", .{})
        }
    }
    | .Number => |n| {
        match n {
            0 => std.debug.print("Zero", .{})
            > 0 => std.debug.print("Positive: {d}", .{n})
            < 0 => std.debug.print("Negative: {d}", .{n})
        }
    }
    | .Empty => {
        std.debug.print("Empty message", .{})
    }
    }
}
```

## Expression-Based Conditionals

Conditionals can also be used as expressions:

```rust
let max = if (a > b) a else b

let description = match status {
    .Success => "Operation succeeded"
    .Failure => "Operation failed"
    .Pending => "Operation in progress"
}


let order_status = match status {
    .Success => "Done"
    .Failure => "Failed"
    .Pending => {
        let get_last_known = get_progess(order)
        match get_last_known() {
            error => break: "Unkown pending status"
            None => break: "Not found???"
            Some => |progress| break: progress_to_str(progress)
        }
    }
}

```
