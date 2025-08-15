# VS Code Extension Installation Guide

## Quick Setup for Howl Formatter in VS Code

### 1. Install Extension Dependencies

```bash
cd extension
npm install
npm run compile
```

### 2. Install the Extension

#### Option A: Development Mode
```bash
cd extension
code .
# Press F5 to launch Extension Development Host
```

#### Option B: Package and Install
```bash
cd extension
npm run package
code --install-extension howl-language-support-0.2.0.vsix
```

### 3. Configure VS Code Settings

Create or update your VS Code settings (`Ctrl+,` then click "Open Settings JSON"):

```json
{
    "howl.formatter.enable": true,
    "howl.formatter.formatOnSave": true,
    "howl.formatter.indentSize": 4,
    "howl.formatter.useTabs": false,
    "howl.formatter.trailingCommaThreshold": 3,
    
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "howl-lang.howl-language-support",
    
    "[howl]": {
        "editor.formatOnSave": true,
        "editor.defaultFormatter": "howl-lang.howl-language-support"
    }
}
```

### 4. Test the Formatter

1. Open `extension/example.howl` in VS Code
2. Try these commands:
   - **Format Document**: `Ctrl+Shift+I` (Windows/Linux) or `Cmd+Shift+I` (macOS)
   - **Format Selection**: Select text, then `Ctrl+K Ctrl+F` 
   - **Command Palette**: `Ctrl+Shift+P` → "Howl: Format Document"

### 5. Test Trailing Comma Formatting

Edit the example file:

```howl
// Add trailing comma to trigger multi-line format:
let array = [1, 2, 3,];  

// Remove trailing comma for compact format:  
let array = [1, 2, 3];
```

Then format the document to see the intelligent formatting in action!

### 6. Enable Format on Save

Save a `.howl` file and watch it automatically format based on trailing comma rules.

## Troubleshooting

### Formatter Not Working
1. Check Howl compiler is available: `howl --version`
2. Verify extension settings: `Ctrl+,` → search "howl"
3. Check output panel: `View` → `Output` → select "Howl Language Server"

### LSP Issues  
1. Ensure Howl is built: `zig build` in the project root
2. Restart language server: `Ctrl+Shift+P` → "Howl: Restart Language Server"

Enjoy intelligent Howl code formatting with trailing comma support!