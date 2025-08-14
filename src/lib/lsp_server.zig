const std = @import("std");
const lsp = @import("lsp.zig");
const CompileProcess = @import("compile_process.zig");
const ErrorSystem = @import("error_system.zig");
const ast = @import("ast.zig");

pub const DocumentStore = struct {
    documents: std.StringHashMap(Document),
    allocator: std.mem.Allocator,

    const Document = struct {
        uri: []const u8,
        text: []const u8,
        version: i32,

        pub fn deinit(self: *Document, allocator: std.mem.Allocator) void {
            allocator.free(self.uri);
            allocator.free(self.text);
        }
    };

    pub fn init(allocator: std.mem.Allocator) DocumentStore {
        return DocumentStore{
            .documents = std.StringHashMap(Document).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DocumentStore) void {
        var iterator = self.documents.iterator();
        while (iterator.next()) |entry| {
            var doc = entry.value_ptr;
            doc.deinit(self.allocator);
        }
        self.documents.deinit();
    }

    pub fn openDocument(self: *DocumentStore, uri: []const u8, text: []const u8, version: i32) !void {
        const owned_uri = try self.allocator.dupe(u8, uri);
        errdefer self.allocator.free(owned_uri);

        const owned_text = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(owned_text);

        const doc = Document{
            .uri = owned_uri,
            .text = owned_text,
            .version = version,
        };

        try self.documents.put(owned_uri, doc);
    }

    pub fn updateDocument(self: *DocumentStore, uri: []const u8, text: []const u8, version: i32) !void {
        if (self.documents.getPtr(uri)) |doc| {
            self.allocator.free(doc.text);
            doc.text = try self.allocator.dupe(u8, text);
            doc.version = version;
        }
    }

    pub fn closeDocument(self: *DocumentStore, uri: []const u8) void {
        if (self.documents.fetchRemove(uri)) |kv| {
            var doc = kv.value;
            doc.deinit(self.allocator);
        }
    }

    pub fn getDocument(self: *DocumentStore, uri: []const u8) ?*Document {
        return self.documents.getPtr(uri);
    }
};

pub const LspServer = struct {
    allocator: std.mem.Allocator,
    documents: DocumentStore,
    initialized: bool,
    shutdown_requested: bool,
    capabilities: lsp.ServerCapabilities,

    pub fn init(allocator: std.mem.Allocator) LspServer {
        return LspServer{
            .allocator = allocator,
            .documents = DocumentStore.init(allocator),
            .initialized = false,
            .shutdown_requested = false,
            .capabilities = createServerCapabilities(),
        };
    }

    pub fn deinit(self: *LspServer) void {
        self.documents.deinit();
    }

    fn createServerCapabilities() lsp.ServerCapabilities {
        return lsp.ServerCapabilities{
            .textDocumentSync = lsp.TextDocumentSyncOptions{
                .openClose = true,
                .change = .Full,
                .willSave = false,
                .willSaveWaitUntil = false,
                .save = lsp.SaveOptions{ .includeText = false },
            },
            .completionProvider = lsp.CompletionOptions{
                .triggerCharacters = null, // Simplified for now
                .resolveProvider = false,
            },
            .hoverProvider = true,
            .definitionProvider = true,
            .referencesProvider = false,
            .documentSymbolProvider = false,
            .codeActionProvider = false,
            .documentFormattingProvider = false,
            .documentRangeFormattingProvider = false,
            .renameProvider = false,
            .foldingRangeProvider = false,
            .workspaceSymbolProvider = false,
        };
    }

    pub fn handleRequest(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        if (std.mem.eql(u8, request.method, "initialize")) {
            try self.handleInitialize(request, writer);
        } else if (std.mem.eql(u8, request.method, "shutdown")) {
            try self.handleShutdown(request, writer);
        } else if (std.mem.eql(u8, request.method, "textDocument/hover")) {
            try self.handleHover(request, writer);
        } else if (std.mem.eql(u8, request.method, "textDocument/definition")) {
            try self.handleDefinition(request, writer);
        } else if (std.mem.eql(u8, request.method, "textDocument/completion")) {
            try self.handleCompletion(request, writer);
        } else {
            try self.sendMethodNotFound(request, writer);
        }
    }

    pub fn handleNotification(self: *LspServer, notification: lsp.LspNotification, writer: anytype) !void {
        if (std.mem.eql(u8, notification.method, "initialized")) {
            self.initialized = true;
        } else if (std.mem.eql(u8, notification.method, "exit")) {
            std.process.exit(if (self.shutdown_requested) 0 else 1);
        } else if (std.mem.eql(u8, notification.method, "textDocument/didOpen")) {
            try self.handleDidOpen(notification);
        } else if (std.mem.eql(u8, notification.method, "textDocument/didChange")) {
            try self.handleDidChange(notification, writer);
        } else if (std.mem.eql(u8, notification.method, "textDocument/didClose")) {
            try self.handleDidClose(notification);
        }
    }

    fn sendJsonResponse(self: *LspServer, request_id: ?std.json.Value, result_json: []const u8, writer: anytype) !void {
        _ = self;

        // Build the complete JSON response first
        var response_buffer: [8192]u8 = undefined;
        var response_stream = std.io.fixedBufferStream(&response_buffer);
        const response_writer = response_stream.writer();

        // Write the JSON response to buffer first
        try response_writer.print("{{\"jsonrpc\":\"2.0\",\"id\":", .{});
        
        if (request_id) |id| {
            try std.json.stringify(id, .{}, response_writer);
        } else {
            try response_writer.print("null", .{});
        }
        
        try response_writer.print(",\"result\":{s}}}", .{result_json});
        
        const response_json = response_stream.getWritten();
        
        // Send with correct Content-Length
        try writer.print("Content-Length: {d}\r\n\r\n{s}", .{ response_json.len, response_json });
    }

    fn handleInitialize(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        // Create a compact capabilities response (no newlines or extra spaces)
        const capabilities_json = 
            \\{"capabilities":{"textDocumentSync":{"openClose":true,"change":1},"completionProvider":{"triggerCharacters":["."]},"hoverProvider":true,"definitionProvider":true},"serverInfo":{"name":"Howl Language Server","version":"0.1.0"}}
        ;

        try self.sendJsonResponse(request.id, capabilities_json, writer);
    }

    fn handleShutdown(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        self.shutdown_requested = true;
        try self.sendJsonResponse(request.id, "null", writer);
    }

    fn handleDidOpen(self: *LspServer, notification: lsp.LspNotification) !void {
        _ = self;
        _ = notification;
        // Simplified - just acknowledge the notification for now
    }

    fn handleDidChange(self: *LspServer, notification: lsp.LspNotification, writer: anytype) !void {
        // Try to compile the document and publish diagnostics
        // For now, we'll just publish a simple diagnostic update
        if (notification.params) |params| {
            if (params.object.get("textDocument")) |text_doc| {
                if (text_doc.object.get("uri")) |uri_value| {
                    const uri = uri_value.string;
                    try self.publishDiagnostics(uri, writer);
                }
            }
        }
    }

    fn handleDidClose(self: *LspServer, notification: lsp.LspNotification) !void {
        _ = self;
        _ = notification;
        // Simplified - just acknowledge the notification for now
    }

    fn handleHover(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        // For now, provide basic hover information
        // In a real implementation, this would analyze the symbol at the position
        const hover_json =
            \\{
            \\  "contents": {
            \\    "kind": "markdown",
            \\    "value": "Howl Language Symbol\n\nHover information will be available once semantic analysis is integrated."
            \\  }
            \\}
        ;

        try self.sendJsonResponse(request.id, hover_json, writer);
    }

    fn handleDefinition(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        // Basic go-to-definition implementation
        // In a real implementation, this would analyze the symbol at the position
        // and return its definition location
        
        // For demonstration, return a simple location
        const definition_json = 
            \\{{
            \\  "uri": "file:///path/to/definition.howl",
            \\  "range": {{
            \\    "start": {{ "line": 0, "character": 0 }},
            \\    "end": {{ "line": 0, "character": 10 }}
            \\  }}
            \\}}
        ;
        
        try self.sendJsonResponse(request.id, definition_json, writer);
    }

    fn handleCompletion(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        // Enhanced completion list with Howl-specific keywords and constructs
        const completion_json =
            \\[
            \\  {"label": "fn", "kind": 14, "detail": "function declaration", "documentation": "Define a function"},
            \\  {"label": "let", "kind": 14, "detail": "mutable variable", "documentation": "Declare a mutable variable"},
            \\  {"label": "const", "kind": 14, "detail": "constant declaration", "documentation": "Declare a constant"},
            \\  {"label": "if", "kind": 14, "detail": "conditional statement", "documentation": "Conditional execution"},
            \\  {"label": "else", "kind": 14, "detail": "else clause", "documentation": "Alternative execution path"},
            \\  {"label": "while", "kind": 14, "detail": "while loop", "documentation": "While loop construct"},
            \\  {"label": "for", "kind": 14, "detail": "for loop", "documentation": "For loop construct"},
            \\  {"label": "in", "kind": 14, "detail": "in operator", "documentation": "Used in for..in loops"},
            \\  {"label": "match", "kind": 14, "detail": "pattern matching", "documentation": "Pattern matching construct"},
            \\  {"label": "return", "kind": 14, "detail": "return statement", "documentation": "Return from function"},
            \\  {"label": "break", "kind": 14, "detail": "break statement", "documentation": "Break from loop"},
            \\  {"label": "continue", "kind": 14, "detail": "continue statement", "documentation": "Continue to next iteration"},
            \\  {"label": "struct", "kind": 22, "detail": "struct type", "documentation": "Define a struct type"},
            \\  {"label": "enum", "kind": 22, "detail": "enum type", "documentation": "Define an enum type"},
            \\  {"label": "type", "kind": 22, "detail": "type alias", "documentation": "Define a type alias"},
            \\  {"label": "@import", "kind": 3, "detail": "import directive", "documentation": "Import a module"},
            \\  {"label": "string", "kind": 22, "detail": "string type", "documentation": "String type"},
            \\  {"label": "u32", "kind": 22, "detail": "unsigned 32-bit integer", "documentation": "32-bit unsigned integer type"},
            \\  {"label": "i32", "kind": 22, "detail": "signed 32-bit integer", "documentation": "32-bit signed integer type"},
            \\  {"label": "f64", "kind": 22, "detail": "64-bit float", "documentation": "64-bit floating point type"},
            \\  {"label": "bool", "kind": 22, "detail": "boolean type", "documentation": "Boolean type"},
            \\  {"label": "true", "kind": 12, "detail": "boolean literal", "documentation": "Boolean true value"},
            \\  {"label": "false", "kind": 12, "detail": "boolean literal", "documentation": "Boolean false value"},
            \\  {"label": "..", "kind": 24, "detail": "range operator", "documentation": "Inclusive range operator"},
            \\  {"label": "..<", "kind": 24, "detail": "exclusive range", "documentation": "Exclusive end range operator"},
            \\  {"label": "..=", "kind": 24, "detail": "inclusive range", "documentation": "Inclusive end range operator"},
            \\  {"label": "|>", "kind": 24, "detail": "pipe operator", "documentation": "Function composition pipe"},
            \\  {"label": "=>", "kind": 24, "detail": "arrow operator", "documentation": "Pattern match arrow"}
            \\]
        ;

        try self.sendJsonResponse(request.id, completion_json, writer);
    }

    fn publishDiagnostics(self: *LspServer, uri: []const u8, writer: anytype) !void {
        _ = self;

        // Create a simple diagnostic notification (compact JSON)
        var diagnostic_buffer: [1024]u8 = undefined;
        const diagnostic_json = try std.fmt.bufPrint(&diagnostic_buffer, 
            "{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{{\"uri\":\"{s}\",\"diagnostics\":[]}}}}", 
            .{uri});

        // Send as notification with proper Content-Length
        try writer.print("Content-Length: {d}\r\n\r\n{s}", .{ diagnostic_json.len, diagnostic_json });
    }

    fn sendMethodNotFound(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        _ = self;
        const error_json =
            \\{"code": -32601, "message": "Method not found"}
        ;

        // Calculate content length for error response
        var content_length: usize = 50; // Base JSON structure for error
        content_length += error_json.len;

        if (request.id) |id| {
            switch (id) {
                .integer => |i| {
                    var buf: [32]u8 = undefined;
                    const id_str = std.fmt.bufPrint(&buf, "{d}", .{i}) catch "0";
                    content_length += id_str.len;
                },
                .string => |s| content_length += s.len + 2, // +2 for quotes
                else => content_length += 4, // "null"
            }
        } else {
            content_length += 4; // "null"
        }

        try writer.print("Content-Length: {d}\r\n\r\n", .{content_length});
        try writer.print("{{\"jsonrpc\":\"2.0\",\"id\":", .{});

        if (request.id) |id| {
            try std.json.stringify(id, .{}, writer);
        } else {
            try writer.print("null", .{});
        }

        try writer.print(",\"error\":{s}}}", .{error_json});
    }
};

fn uriToFilePath(allocator: std.mem.Allocator, uri: []const u8) ![]u8 {
    if (std.mem.startsWith(u8, uri, "file://")) {
        const path = uri[7..];
        return allocator.dupe(u8, path);
    }
    return allocator.dupe(u8, uri);
}
