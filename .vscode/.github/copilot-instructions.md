## Design documents

- Keep the design document concise and focused on the problem at hand.
- Give only a few examples to illustrate the problem.
- Use clear and simple language to describe the problem and the solution.
- Use mermaid diagrams to illustrate complex concepts.
- Avoid creating overly complex diagrams; keep them simple and easy to understand.
- Use headings and subheadings to organize the document.
- Use bullet points and lists to break down information into digestible parts.
- Don't inlclude decision tables in design documents; they are not needed.
- Use code snippets to illustrate examples, but keep them short and relevant.
- Avoid using too many comments in code snippets; they should be self-explanatory.
- Use consistent formatting for code snippets, such as indentation and spacing.
- Use tables to show available options and their characteristics.

## Documentation of the compiler itself.

- Documents are stored in `docs/compiler`.

### Compiler Development Guidelines

The compiler is written in the newest version of Zig.
Use best practices for Zig development, such as:

- Use `const` and `var` appropriately.
- Use `comptime` for compile-time evaluation.
- Try to keep files under 1000 lines of code.
- Use explicit `errror` types and not inferred errors.

### Lexer

The lexer has a `.addFile` method. This method takes control of the memory of the arguments.
So make sure the filename and contents are stack allocated strings that can be freed.

### Temporary tests

- Temporary tests are stored in `src/tmp_test` -
- Temporary test can be run with `cd PROJECT_ROOT && zig run src/tmp_test/TEST_NAME.zig`.

### Implementing the compiler

- look at the `lang-design` folder for the design documents.

  These are the main design documents for the language and compiler:

- `lang-design/arrays-slices.md`
- `lang-design/control-flow.md`
- `lang-design/error-handling.md`
- `lang-design/keywords.md`
- `lang-design/modules.md`
- `lang-design/simd-vectors.md`
- `lang-design/std.md`
- `lang-design/target-platforms.md`
- `lang-design/variables.md`
- `lang-design/builtins.md`
- `lang-design/enums-unions.md`
- `lang-design/functions.md`
- `lang-design/memory-management.md`
- `lang-design/operators.md`
- `lang-design/spec.md`
- `lang-design/structs.md`
- `lang-design/type.md`
