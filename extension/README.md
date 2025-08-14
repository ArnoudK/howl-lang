# Howl Language Support for VS Code

This extension provides comprehensive language support for the Howl programming language, including syntax highlighting, language server integration, and intelligent code features.

## Features

- **Syntax Highlighting**: Full syntax highlighting for Howl language constructs
- **Language Server Protocol (LSP)**: Integrated language server for real-time error reporting and code intelligence
- **Auto-completion**: Intelligent code completion for keywords and language constructs
- **Error Diagnostics**: Real-time error reporting and syntax validation
- **Code Navigation**: Support for go-to-definition and hover information
- **Bracket Matching**: Auto-closing brackets and intelligent indentation
- **Comment Support**: Line and block comment toggling

## Installation

### Method 1: Install from VS Code Extension Directory

1. Copy the entire `extension` folder to your VS Code extensions directory:
   - **Windows**: `%USERPROFILE%\.vscode\extensions\howl-language-support-0.1.0`
   - **macOS**: `~/.vscode/extensions/howl-language-support-0.1.0`
   - **Linux**: `~/.vscode/extensions/howl-language-support-0.1.0`

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
   code --install-extension howl-language-support-0.1.0.vsix
   ```

### Method 3: Development Mode

1. Open the `extension` folder in VS Code
2. Press `F5` to launch a new Extension Development Host window
3. The extension will be active in the new window

## Requirements

### Howl Language Server

The extension requires the Howl language server (`howl_lsp`) to be available. Make sure you have:

1. Built the Howl compiler with LSP support:
   ```bash
   cd /path/to/howl-lang
   zig build
   ```

2. The `howl_lsp` executable should be in your PATH or in the `zig-out/bin/` directory of your Howl project.

## Configuration

The extension can be configured through VS Code settings:

```json
{
  "howl.lsp.serverPath": "howl_lsp",
  "howl.lsp.enabled": true,
  "howl.lsp.trace.server": "off"
}
```

### Settings

- `howl.lsp.serverPath`: Path to the Howl language server executable
  - Default: `"howl_lsp"`
  - Can be an absolute path or relative to workspace
  - The extension will automatically look for `zig-out/bin/howl_lsp` in your workspace

- `howl.lsp.enabled`: Enable/disable the language server
  - Default: `true`

- `howl.lsp.trace.server`: Trace communication with the language server
  - Options: `"off"`, `"messages"`, `"verbose"`
  - Default: `"off"`

## Commands

The extension provides the following commands:

- `Howl: Restart Language Server` - Restart the language server
- `Howl: Show Output Channel` - Show the language server output for debugging

Access these commands through the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`).

## Usage

1. Open a `.howl` file in VS Code
2. The extension will automatically activate and start the language server
3. You should see syntax highlighting immediately
4. Error diagnostics will appear as you type (if the language server is running)
5. Use `Ctrl+Space` for code completion

## Troubleshooting

### Language Server Not Starting

1. Check that `howl_lsp` is in your PATH:
   ```bash
   which howl_lsp  # On macOS/Linux
   where howl_lsp  # On Windows
   ```

2. Verify the server path in settings:
   - Open VS Code settings (`Ctrl+,`)
   - Search for "howl"
   - Check the `howl.lsp.serverPath` setting

3. Check the output channel:
   - Run command: `Howl: Show Output Channel`
   - Look for error messages

### Extension Not Activating

1. Ensure you have a `.howl` file open
2. Check the VS Code extension view to confirm the extension is installed and enabled
3. Try restarting VS Code

### Syntax Highlighting Issues

1. Verify the file has a `.howl` extension
2. Try manually setting the language: `Ctrl+Shift+P` → "Change Language Mode" → "Howl"

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

- Language server features are currently basic and will be enhanced over time
- Icon files are placeholders and will be replaced with proper graphics

## Release Notes

### 0.1.0

- Initial release
- Basic syntax highlighting
- Language server integration
- Auto-completion for keywords
- Error diagnostics support
- VS Code integration with proper file association

## License

This extension is part of the Howl programming language project. Please refer to the main project for licensing information.