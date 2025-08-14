# 🐺 Howl VS Code Extension - Complete Installation Guide

## Quick Start (Recommended)

### 1. Automatic Installation
```bash
cd /home/du/Code/howl-lang/extension
./install.sh
```

### 2. Manual Installation
1. Copy the `extension` folder to your VS Code extensions directory:
   - **Linux/macOS**: `~/.vscode/extensions/howl-language-support-0.1.0/`
   - **Windows**: `%USERPROFILE%\.vscode\extensions\howl-language-support-0.1.0\`

2. Restart VS Code

### 3. Verification
1. Open the example file: `extension/example.howl`
2. You should see syntax highlighting immediately
3. Check the status bar for "Howl" language mode

## Extension Features

✅ **Syntax Highlighting**: Complete highlighting for Howl language constructs
✅ **LSP Integration**: Connects to the Howl language server for real-time features
✅ **Auto-completion**: Keyword completion (fn, let, const, if, while, etc.)
✅ **Error Diagnostics**: Real-time error reporting (when LSP server is running)
✅ **Bracket Matching**: Auto-closing brackets and smart indentation
✅ **Comment Support**: Line (`//`) and block (`/* */`) comments
✅ **File Association**: Automatic `.howl` file recognition

## Configuration

Open VS Code Settings and search for "howl":

```json
{
  "howl.lsp.serverPath": "howl_lsp",
  "howl.lsp.enabled": true,
  "howl.lsp.trace.server": "off"
}
```

## Connecting to the Language Server

The extension will automatically look for the `howl_lsp` executable in:
1. Your system PATH
2. `zig-out/bin/howl_lsp` in the current workspace
3. The path specified in `howl.lsp.serverPath` setting

Make sure you've built the language server:
```bash
cd /home/du/Code/howl-lang
./zig-install/zig build
```

## Troubleshooting

### Language Server Issues
1. **Check if server is running**: 
   - Command Palette → "Howl: Show Output Channel"
   - Look for connection messages

2. **Server not found**:
   - Set absolute path: `"howl.lsp.serverPath": "/full/path/to/howl_lsp"`
   - Verify executable exists: `which howl_lsp`

3. **Restart server**:
   - Command Palette → "Howl: Restart Language Server"

### Extension Issues
1. **No syntax highlighting**:
   - Verify file has `.howl` extension
   - Manually set language: Command Palette → "Change Language Mode" → "Howl"

2. **Extension not loading**:
   - Check installed extensions: View → Extensions → Search "howl"
   - Restart VS Code

## Commands Available

Access via Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`):
- `Howl: Restart Language Server`
- `Howl: Show Output Channel`

## Development Mode

For development and testing:
1. Open the `extension` folder in VS Code
2. Press `F5` to launch Extension Development Host
3. Open a `.howl` file in the new window
4. Test the extension features

## File Structure

```
extension/
├── package.json              # Extension manifest
├── src/extension.ts          # Main extension code
├── syntaxes/                 # Syntax highlighting
│   └── howl.tmGrammar.json
├── language-configuration.json # Language settings
├── README.md                 # Documentation
├── example.howl              # Test file
├── install.sh               # Unix installer
└── install.bat              # Windows installer
```

## What's Working

- ✅ Extension loads and activates for `.howl` files
- ✅ Syntax highlighting works immediately
- ✅ LSP client connects to server (when available)
- ✅ Basic error reporting pipeline established
- ✅ Auto-completion framework ready
- ✅ Command palette integration
- ✅ Configuration system working

## Next Steps

1. **Test the extension**: Open `extension/example.howl`
2. **Configure LSP path** if needed in VS Code settings
3. **Enhance LSP server** features as the language develops
4. **Add more language features** like snippets, themes, etc.

The extension is ready to use and provides a solid foundation for Howl language development in VS Code! 🎉