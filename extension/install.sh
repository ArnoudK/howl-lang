#!/bin/bash

# Howl VS Code Extension Installer
# This script installs the Howl language extension for VS Code

set -e

echo "🐺 Installing Howl Language Support for VS Code..."

# Detect VS Code extensions directory
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    VSCODE_EXTENSIONS_DIR="$HOME/.vscode/extensions"
elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    # Windows
    VSCODE_EXTENSIONS_DIR="$USERPROFILE/.vscode/extensions"
else
    # Linux and others
    VSCODE_EXTENSIONS_DIR="$HOME/.vscode/extensions"
fi

EXTENSION_DIR="$VSCODE_EXTENSIONS_DIR/howl-language-support-0.2.0"

echo "📁 Installing to: $EXTENSION_DIR"

# Create the extensions directory if it doesn't exist
mkdir -p "$VSCODE_EXTENSIONS_DIR"

# Remove existing installation
if [ -d "$EXTENSION_DIR" ]; then
    echo "🗑️  Removing existing installation..."
    rm -rf "$EXTENSION_DIR"
fi

# Copy extension files
echo "📋 Copying extension files..."
cp -r "$(dirname "$0")" "$EXTENSION_DIR"

# Clean up development files that shouldn't be in the installed extension
cd "$EXTENSION_DIR"
rm -f install.sh
rm -rf node_modules/.cache
rm -rf .git

echo "✅ Installation complete!"
echo ""
echo "📖 Next steps:"
echo "1. Restart VS Code"
echo "2. Open a .howl file"
echo "3. The extension should activate automatically"
echo ""
echo "🔧 Configuration:"
echo "- Set 'howl.lsp.serverPath' to point to your howl executable (uses 'howl lsp')"
echo "- Enable 'howl.lsp.enableEnhancedCompletion' for smart autocomplete"
echo "- Try typing 'std.debug.print' to see format specifier help"
echo "- Use Command Palette → 'Howl: Build Current File' to compile"
echo "- Check 'Howl: Show Output Channel' if you encounter issues"
echo ""
echo "🎉 Happy coding with Howl!"