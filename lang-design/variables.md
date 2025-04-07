# Variable Declarations

Howl offers several declaration keywords to express intent about mutability and compile-time evaluation.

## Declaration Keywords

| Keyword | Description                                             | Use Case                                                     |
| :------ | :------------------------------------------------------ | :----------------------------------------------------------- |
| `let`   | Non-reassignable variable declaration with mutable contents | For most variables where reassignment isn't needed           |
| `var`   | Mutable variable declaration (reassignable)             | For variables that need to be reassigned                     |
| `const` | Immutable compile-time known value (not reassignable)   | For truly immutable values known at compile time             |
| `comp`  | Compile-time variable that remains mutable              | For values calculated at compile time that need modification |

## Examples

```rust
// Non-reassignable variable (contents may be modified)
let user = #Person{.name = "Alice", .age = 30}
user.age = 31 // OK: changing a field
// user = Person{.name = "Bob", .age = 25} // Error: can't reassign 'let'

// Compile-time constant (nothing can be modified)
const PI = 3.14159
// PI = 3 // Error: can't modify 'const'

// Compile-time mutable
comp fibonacci = [_]u32{0, 1, 1, 2, 3, 5, 8, 13}
fibonacci[7] = 21 // OK: can modify compile-time value

// Mutable variable
var counter = 0
counter = 1 // OK: can reassign 'var'
```
