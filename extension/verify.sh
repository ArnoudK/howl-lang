#!/bin/bash

echo "🐺 Howl Language Extension - Verification Script"
echo "=================================================="

# Check if LSP server is built
echo -n "1. Checking if LSP server is built... "
if [ -f "/home/du/Code/howl-lang/zig-out/bin/howl_lsp" ]; then
    echo "✅ Found"
else
    echo "❌ Not found - run 'zig build' first"
    exit 1
fi

# Test LSP server responds correctly  
echo -n "2. Testing LSP server initialize... "
cd /home/du/Code/howl-lang
MESSAGE='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{"textDocument":{}}}}'
RESPONSE=$(printf "Content-Length: ${#MESSAGE}\r\n\r\n${MESSAGE}" | timeout 3s ./zig-out/bin/howl_lsp 2>/dev/null)

if [[ "$RESPONSE" == *"capabilities"* ]]; then
    echo "✅ Working"
else
    echo "❌ Failed"
    echo "Response: $RESPONSE"
    exit 1
fi

# Test completion
echo -n "3. Testing LSP server completion... "
MESSAGE2='{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///test.howl"},"position":{"line":0,"character":0}}}'
COMPLETION=$(printf "Content-Length: ${#MESSAGE2}\r\n\r\n${MESSAGE2}" | timeout 3s ./zig-out/bin/howl_lsp 2>/dev/null)

if [[ "$COMPLETION" == *"fn"* ]]; then
    echo "✅ Working"
else
    echo "❌ Failed"
    echo "Response: $COMPLETION"
fi

# Check extension files
echo -n "4. Checking extension files... "
cd /home/du/Code/howl-lang/extension
if [ -f "out/extension.js" ] && [ -f "package.json" ] && [ -f "syntaxes/howl.tmGrammar.json" ]; then
    echo "✅ Complete"
else
    echo "❌ Missing files - run 'npm run compile'"
    exit 1
fi

# Check example file
echo -n "5. Checking example file... "
if [ -f "example.howl" ]; then
    echo "✅ Ready for testing"
else
    echo "❌ Missing example.howl"
fi

echo ""
echo "🎉 All checks passed! Ready to install extension:"
echo ""
echo "   cd /home/du/Code/howl-lang/extension"
echo "   ./install.sh"
echo ""
echo "Then restart VS Code and open extension/example.howl"