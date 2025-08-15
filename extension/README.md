# Howl Language Support for VS Code

This extension provides comprehensive language support for the Howl programming language, including enhanced syntax highlighting, integrated language server with advanced autocomplete, and built-in compiler commands.

## Features

- **Enhanced Syntax Highlighting**: Full syntax highlighting for Howl language constructs
- **Advanced Language Server Protocol (LSP)**: Integrated language server with intelligent autocomplete
- **Advanced Code Formatting**: Intelligent code formatter with trailing comma support
- **Format on Save**: Automatic code formatting when saving files
- **Smart Auto-completion**: 
  - Context-aware completions for `std.debug.print` with format specifier documentation
  - Anonymous struct syntax support (`.{arg1, arg2}`)
  - Standard library module completions
  - Format specifier help (`{d}`, `{s}`, `{f:.2}`, etc.)
- **Enhanced Debug.Print Support**: Rich autocomplete for format strings and precision specifiers
- **Integrated Build/Run Commands**: Built-in commands to compile and execute Howl files
- **Error Diagnostics**: Real-time error reporting and syntax validation
- **Code Navigation**: Support for go-to-definition and hover information
- **Bracket Matching**: Auto-closing brackets and intelligent indentation
- **Comment Support**: Line and block comment toggling

## Installation

### Method 1: Install from VS Code Extension Directory

1. Copy the entire `extension` folder to your VS Code extensions directory:
   - **Windows**: `%USERPROFILE%\.vscode\extensions\howl-language-support-0.2.0`
   - **macOS**: `~/.vscode/extensions/howl-language-support-0.2.0`
   - **Linux**: `~/.vscode/extensions/howl-language-support-0.2.0`

2. Restart VS Code

### Method 2: Package and Install

1. Install the VS Code Extension Manager CLI:
   ```bash
   npm install -g @vscode/vsce
   ```

2. Package the extension:
   ```bash
   cd extension
   npm run package
   ```

3. Install the generated `.vsix` file:
   ```bash
   code --install-extension howl-language-support-0.2.0.vsix
   ```

### Method 3: Development Mode

1. Open the `extension` folder in VS Code
2. Press `F5` to launch a new Extension Development Host window
3. The extension will be active in the new window

## Requirements

### Howl Compiler

The extension requires the Howl compiler (`howl`) to be available. Make sure you have:

1. Built the Howl compiler:
   ```bash
   cd /path/to/howl-lang
   zig build
   ```

2. The `howl` executable should be in your PATH or in the `zig-out/bin/` directory of your Howl project.

The extension now uses the integrated `howl lsp` subcommand instead of a separate `howl_lsp` executable.

## Configuration

The extension can be configured through VS Code settings:

```json
{
  "howl.lsp.serverPath": "howl",
  "howl.lsp.enabled": true,
  "howl.lsp.enableEnhancedCompletion": true,
  "howl.lsp.completionTriggers": [".", "@", "{", "}"],
  "howl.lsp.trace.server": "off",
  "howl.formatter.enable": true,
  "howl.formatter.formatOnSave": false,
  "howl.formatter.indentSize": 4,
  "howl.formatter.useTabs": false,
  "howl.formatter.maxLineLength": 100,
  "howl.formatter.trailingCommaThreshold": 3,
  "howl.formatter.alwaysMultilineTrailingComma": true
}
```

### Settings

- `howl.lsp.serverPath`: Path to the Howl compiler executable (uses `howl lsp` subcommand)
  - Default: `"howl"`
  - Can be an absolute path or relative to workspace
  - The extension will automatically look for `zig-out/bin/howl` in your workspace

- `howl.lsp.enabled`: Enable/disable the language server with enhanced features
  - Default: `true`

- `howl.lsp.enableEnhancedCompletion`: Enable enhanced completion with format specifier documentation
  - Default: `true`
  - Provides rich autocomplete for `std.debug.print` format specifiers
  - Includes anonymous struct syntax support

- `howl.lsp.completionTriggers`: Characters that trigger autocomplete
  - Default: `[".", "@", "{", "}"]`
  - Supports std library access and struct literals

- `howl.lsp.trace.server`: Trace communication with the language server
  - Options: `"off"`, `"messages"`, `"verbose"`
  - Default: `"off"`

## Commands

The extension provides the following commands:

- `Howl: Restart Language Server` - Restart the language server
- `Howl: Show Output Channel` - Show the language server output for debugging
- `Howl: Build Current File` - Compile the current Howl file (JavaScript target)
- `Howl: Run Current File` - Compile and run the current Howl file
- `Howl: Format Document` - Format the entire document (`Ctrl+Shift+I`)
- `Howl: Format Selection` - Format selected text (`Ctrl+K Ctrl+F`)

Access these commands through the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`).

## Code Formatting

The extension includes an advanced code formatter with intelligent trailing comma support.

### Formatting Commands

| Command | Shortcut | Description |
|---------|----------|-------------|
| Format Document | `Ctrl+Shift+I` (`Cmd+Shift+I`) | Format the entire document |
| Format Selection | `Ctrl+K Ctrl+F` (`Cmd+K Cmd+F`) | Format selected text |

### Format on Save

Enable automatic formatting when saving files:

```json
{
  "howl.formatter.formatOnSave": true,
  // OR use the general editor setting:
  "editor.formatOnSave": true,
  "editor.defaultFormatter": "howl-lang.howl-language-support"
}
```

### Trailing Comma Intelligence

The formatter intelligently handles trailing commas with a special rule:

> **🎯 Trailing Comma Rule**: When a trailing comma is present, all items are formatted on separate lines.

#### Examples

**Arrays:**
```howl
// Without trailing comma (stays compact):
let colors = ["red", "green", "blue"];

// With trailing comma (formats multi-line):
let colors = [
    "red",
    "green",
    "blue",
];
```

**Struct Literals:**
```howl
// Without trailing comma (stays compact):
let point = Point { x: 10, y: 20 };

// With trailing comma (formats multi-line):
let point = Point {
    x: 10,
    y: 20,
    z: 30,
};
```

**Function Parameters:**
```howl
// Without trailing comma (stays compact):
fn add(a: i32, b: i32) -> i32 { return a + b; }

// With trailing comma (formats multi-line):
fn complex_calc(
    first: i32,
    second: f64,
    third: string,
    fourth: bool,
) -> i32 {
    return 42;
}
```

### Formatter Configuration

Customize the formatter behavior with these settings:

- `howl.formatter.enable` (boolean, default: `true`): Enable/disable the formatter
- `howl.formatter.formatOnSave` (boolean, default: `false`): Auto-format on save
- `howl.formatter.indentSize` (number, default: `4`): Spaces for indentation
- `howl.formatter.useTabs` (boolean, default: `false`): Use tabs instead of spaces
- `howl.formatter.maxLineLength` (number, default: `100`): Maximum line length
- `howl.formatter.trailingCommaThreshold` (number, default: `3`): Item count to trigger multi-line
- `howl.formatter.alwaysMultilineTrailingComma` (boolean, default: `true`): Force multi-line with trailing commas

## Enhanced Autocomplete Features

### Debug.Print Format Specifiers

When typing `std.debug.print`, the extension provides rich documentation for format specifiers:

- `{}` - Default formatting
- `{d}` - Decimal integer  
- `{s}` - String
- `{f}` - Float
- `{f:.2}` - Float with 2 decimal places
- `{f:.N}` - Float with N decimal places

### Anonymous Struct Support

Smart completion for anonymous struct arguments:
- Type `.{` to get anonymous struct completion
- Shows examples like `.{arg1, arg2, arg3}`

### Standard Library Completions

Context-aware completions when typing `std.`:
- `debug` - Debug utilities with enhanced print function
- `math` - Mathematical functions
- `mem` - Memory utilities
- `fmt` - Formatting utilities
- And more...

## Usage

1. Open a `.howl` file in VS Code
2. The extension will automatically activate and start the language server
3. You should see syntax highlighting immediately
4. Type `std.debug.print` to see enhanced autocomplete with format documentation
5. Use `Ctrl+Space` for general code completion
6. Use Command Palette commands to build/run files

## Troubleshooting

### Language Server Not Starting

1. Check that `howl` is in your PATH:
   ```bash
   which howl  # On macOS/Linux
   where howl  # On Windows
   ```

2. Verify the server path in settings:
   - Open VS Code settings (`Ctrl+,`)
   - Search for "howl"
   - Check the `howl.lsp.serverPath` setting

3. Check the output channel:
   - Run command: `Howl: Show Output Channel`
   - Look for error messages

4. Test the LSP manually:
   ```bash
   howl lsp  # Should start the language server
   ```

### Extension Not Activating

1. Ensure you have a `.howl` file open
2. Check the VS Code extension view to confirm the extension is installed and enabled
3. Try restarting VS Code

### Missing Enhanced Completions

1. Ensure `howl.lsp.enableEnhancedCompletion` is set to `true`
2. Check that you're using the latest version of the Howl compiler
3. Restart the language server with `Howl: Restart Language Server`

### Formatter Issues

1. Check that `howl.formatter.enable` is set to `true`
2. Verify the Howl compiler supports formatting: `howl fmt --help`
3. Check that the file is saved as a `.howl` file
4. For format-on-save issues, verify the default formatter is set correctly:
   ```json
   {
     "editor.defaultFormatter": "howl-lang.howl-language-support"
   }
   ```
5. Try manual formatting first (`Ctrl+Shift+I`) to verify the formatter works

## Development

### Building from Source

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd howl-lang/extension
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Compile the extension:
   ```bash
   npm run compile
   ```

4. Run in development mode:
   ```bash
   code .
   # Press F5 to launch Extension Development Host
   ```

### Contributing

1. Make your changes
2. Test in the Extension Development Host
3. Run linting: `npm run lint`
4. Submit a pull request

## File Association

The extension automatically associates with `.howl` files. If you need to manually set the language mode:

1. Open a Howl file
2. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
3. Type "Change Language Mode"
4. Select "Howl"

## Known Issues

- Icon files are placeholders and will be replaced with proper graphics

## Release Notes

### 0.2.0

- **Major Update**: Integrated with new `howl lsp` command structure
- **Enhanced Autocomplete**: Smart completions for `std.debug.print` with format specifier documentation
- **Anonymous Struct Support**: Rich completion for `.{arg1, arg2}` syntax
- **Advanced Code Formatting**: Intelligent formatter with trailing comma support
- **Format on Save**: Automatic formatting when saving files
- **Build/Run Commands**: Integrated commands to compile and execute Howl files
- **Improved LSP Integration**: Better error handling and configuration options
- **Standard Library Completions**: Context-aware completions for std modules

### 0.1.0

- Initial release
- Basic syntax highlighting
- Language server integration
- Auto-completion for keywords
- Error diagnostics support
- VS Code integration with proper file association

## License

This extension is part of the Howl programming language project. Please refer to the main project for licensing information.