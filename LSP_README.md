# LSP Support for Howl Language

This project now includes Language Server Protocol (LSP) support for the Howl programming language.

## Building

To build both the compiler and LSP server:

```bash
zig build
```

## Running the LSP Server

To start the LSP server:

```bash
zig build lsp
```

Or run the executable directly:

```bash
./zig-out/bin/howl_lsp
```

## Features

The Howl LSP server currently supports:

- **Text Synchronization**: Real-time document updates
- **Diagnostics**: Error reporting with syntax and semantic analysis
- **Hover Information**: Basic hover support (placeholder implementation)
- **Code Completion**: Keyword completion for Howl language constructs
- **Go to Definition**: Basic implementation (placeholder)

## VS Code Extension

To use the LSP with VS Code, you can create a simple extension or use a generic LSP client.

### Option 1: Generic LSP Extension

1. Install the "Generic LSP Client" extension from the VS Code marketplace
2. Add this configuration to your VS Code settings:

```json
{
  "genericLspClient.languageServers": [
    {
      "language": "howl",
      "command": "/path/to/howl-lang/zig-out/bin/howl_lsp",
      "args": [],
      "fileExtensions": [".howl"]
    }
  ]
}
```

### Option 2: Custom VS Code Extension

A basic VS Code extension configuration is provided in the `.vscode/` directory. To use it:

1. Copy the `.vscode/extension/` folder to your VS Code extensions directory
2. Restart VS Code
3. The extension will automatically activate for `.howl` files

## Supported LSP Methods

### Requests
- `initialize` - Server initialization
- `shutdown` - Clean server shutdown
- `textDocument/hover` - Hover information
- `textDocument/definition` - Go to definition
- `textDocument/completion` - Code completion

### Notifications
- `initialized` - Initialization complete
- `exit` - Server exit
- `textDocument/didOpen` - Document opened
- `textDocument/didChange` - Document changed
- `textDocument/didClose` - Document closed
- `textDocument/publishDiagnostics` - Send diagnostics to client

## Development

The LSP implementation consists of:

- `src/lib/lsp.zig` - LSP protocol types and structures
- `src/lib/lsp_server.zig` - Main LSP server implementation
- `src/lsp_main.zig` - LSP server entry point

To extend the LSP functionality:

1. Add new request/notification handlers in `lsp_server.zig`
2. Implement the corresponding LSP protocol types in `lsp.zig`
3. Update the server capabilities in `createServerCapabilities()`

## Testing

To test the LSP server manually:

1. Start the server: `zig build lsp`
2. Send LSP messages via stdin in JSON-RPC format
3. The server will respond via stdout

Example initialize request:
```json
Content-Length: 246

{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"clientInfo":{"name":"test-client"},"rootUri":"file:///path/to/project","capabilities":{"textDocument":{"completion":{},"hover":{}}}}}
```