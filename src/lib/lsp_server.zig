const std = @import("std");
const lsp = @import("lsp.zig");
const CompileProcess = @import("compile_process.zig");
const ErrorSystem = @import("error_system.zig");
const ast = @import("ast.zig");
const formatter = @import("formatter.zig");

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
            // Allocate new text first, only free old text on success
            const new_text = try self.allocator.dupe(u8, text);
            self.allocator.free(doc.text);
            doc.text = new_text;
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
                .triggerCharacters = null,
                .resolveProvider = false,
            },
            .hoverProvider = true,
            .definitionProvider = true,
            .referencesProvider = false,
            .documentSymbolProvider = false,
            .codeActionProvider = false,
            .documentFormattingProvider = true,
            .documentRangeFormattingProvider = true,
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
        } else if (std.mem.eql(u8, request.method, "textDocument/formatting")) {
            try self.handleDocumentFormatting(request, writer);
        } else if (std.mem.eql(u8, request.method, "textDocument/rangeFormatting")) {
            try self.handleDocumentRangeFormatting(request, writer);
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

    fn sendJsonResponse(self: *LspServer, request_id: ?std.json.Value, result: anytype, writer: anytype) !void {

        // Serialize the result to JSON first
        var result_buffer: [16384]u8 = undefined;
        var result_stream = std.io.fixedBufferStream(&result_buffer);
        try std.json.stringify(result, .{}, result_stream.writer());
        const result_json = result_stream.getWritten();

        // Create the full response with proper JSON structure
        // const response = lsp.LspResponse{
        //     .id = request_id,
        //     .result = std.json.Value{ .string = result_json },
        // };

        // Serialize the full response
        const response_buffer = self.allocator.alloc(u8, 1024 * 1024 * 32) catch return error.OutOfMemory;
        var response_stream = std.io.fixedBufferStream(response_buffer);

        // Manually construct the response to avoid double-JSON encoding
        const response_writer = response_stream.writer();
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
        const initialize_result = lsp.InitializeResult{
            .capabilities = lsp.ServerCapabilities{
                .textDocumentSync = lsp.TextDocumentSyncOptions{
                    .openClose = true,
                    .change = .Full,
                    .willSave = false,
                    .willSaveWaitUntil = false,
                    .save = lsp.SaveOptions{ .includeText = false },
                },
                .completionProvider = lsp.CompletionOptions{
                    .triggerCharacters = @constCast(&[_][]const u8{
                        @constCast("."),
                        @constCast("@"),
                        @constCast("{"),
                        @constCast("}"),
                    }),
                    .resolveProvider = false,
                },
                .hoverProvider = true,
                .definitionProvider = true,
                .documentFormattingProvider = true,
                .documentRangeFormattingProvider = true,
                .referencesProvider = false,
                .documentSymbolProvider = false,
                .codeActionProvider = false,
                .renameProvider = false,
                .foldingRangeProvider = false,
                .workspaceSymbolProvider = false,
            },
            .serverInfo = .{
                .name = "Howl Language Server",
                .version = "0.1.0",
            },
        };

        try self.sendJsonResponse(request.id, initialize_result, writer);
    }

    fn handleShutdown(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        self.shutdown_requested = true;
        // For shutdown, we return null as the result
        var response_buffer: [256]u8 = undefined;
        var response_stream = std.io.fixedBufferStream(&response_buffer);
        const response_writer = response_stream.writer();

        try response_writer.print("{{\"jsonrpc\":\"2.0\",\"id\":", .{});
        if (request.id) |id| {
            try std.json.stringify(id, .{}, response_writer);
        } else {
            try response_writer.print("null", .{});
        }
        try response_writer.print(",\"result\":null}}", .{});

        const response_json = response_stream.getWritten();
        try writer.print("Content-Length: {d}\r\n\r\n{s}", .{ response_json.len, response_json });
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
        // Get context information from the request
        var context_info = CompletionContext{};
        if (request.params) |params| {
            if (params.object.get("context")) |ctx| {
                if (ctx.object.get("triggerCharacter")) |trigger| {
                    context_info.trigger_character = trigger.string;
                }
            }

            // Try to get the document text to analyze context
            if (params.object.get("textDocument")) |text_doc| {
                if (text_doc.object.get("uri")) |uri_value| {
                    context_info.uri = uri_value.string;
                }
            }

            if (params.object.get("position")) |pos| {
                if (pos.object.get("line")) |line| {
                    context_info.line = @intCast(line.integer);
                }
                if (pos.object.get("character")) |char| {
                    context_info.character = @intCast(char.integer);
                }
            }
        }

        // Generate completions based on context
        const completion_json = try self.generateCompletions(context_info);
        try self.sendJsonResponse(request.id, completion_json, writer);
    }

    const CompletionContext = struct {
        trigger_character: ?[]const u8 = null,
        uri: ?[]const u8 = null,
        line: u32 = 0,
        character: u32 = 0,
    };

    fn generateCompletions(self: *LspServer, context: CompletionContext) ![]const u8 {
        _ = self;

        // Check if we're completing after a dot (member access)
        if (context.trigger_character) |trigger| {
            if (std.mem.eql(u8, trigger, ".")) {
                // Return std library completions and struct member access
                return 
                \\[
                \\  {"label": "debug", "kind": 9, "detail": "std.debug module", "documentation": "Standard debug utilities", "insertText": "debug"},
                \\  {"label": "math", "kind": 9, "detail": "std.math module", "documentation": "Mathematical functions", "insertText": "math"},
                \\  {"label": "mem", "kind": 9, "detail": "std.mem module", "documentation": "Memory utilities", "insertText": "mem"},
                \\  {"label": "fs", "kind": 9, "detail": "std.fs module", "documentation": "File system utilities", "insertText": "fs"},
                \\  {"label": "fmt", "kind": 9, "detail": "std.fmt module", "documentation": "Formatting utilities", "insertText": "fmt"},
                \\  {"label": "json", "kind": 9, "detail": "std.json module", "documentation": "JSON parsing and serialization", "insertText": "json"},
                \\  {"label": "net", "kind": 9, "detail": "std.net module", "documentation": "Networking utilities", "insertText": "net"},
                \\  {"label": "print", "kind": 3, "detail": "debug.print function", "documentation": "Enhanced debug print with format specifiers\\n\\nSupported format specifiers:\\n- {} - default formatting\\n- {d} - decimal integer\\n- {s} - string\\n- {f} - float\\n- {f:.2} - float with 2 decimal places\\n\\nExample: std.debug.print(\\\"{s}: {f:.2}\\\\n\\\", .{name, value})", "insertText": "print(\\\"$1\\\", .$2)$0", "insertTextFormat": 2}
                \\]
                ;
            }
        }

        // Default completions for general context
        return 
        \\[
        \\  {"label": "fn", "kind": 14, "detail": "function declaration", "documentation": "Define a function\\n\\nSyntax: fn name(params) -> return_type { body }", "insertText": "fn $1($2) -> $3 {\\n    $0\\n}"},
        \\  {"label": "let", "kind": 14, "detail": "mutable variable", "documentation": "Declare a mutable variable\\n\\nSyntax: let name = value;", "insertText": "let $1 = $0;"},
        \\  {"label": "const", "kind": 14, "detail": "constant declaration", "documentation": "Declare a constant\\n\\nSyntax: const NAME = value;", "insertText": "const $1 = $0;"},
        \\  {"label": "if", "kind": 14, "detail": "conditional statement", "documentation": "Conditional execution", "insertText": "if $1 {\\n    $0\\n}"},
        \\  {"label": "else", "kind": 14, "detail": "else clause", "documentation": "Alternative execution path"},
        \\  {"label": "while", "kind": 14, "detail": "while loop", "documentation": "While loop construct", "insertText": "while $1 {\\n    $0\\n}"},
        \\  {"label": "for", "kind": 14, "detail": "for loop", "documentation": "For loop construct\\n\\nSyntax: for item in collection { body }", "insertText": "for $1 in $2 {\\n    $0\\n}"},
        \\  {"label": "in", "kind": 14, "detail": "in operator", "documentation": "Used in for..in loops"},
        \\  {"label": "match", "kind": 14, "detail": "pattern matching", "documentation": "Pattern matching construct", "insertText": "match $1 {\\n    $2 => $0\\n}"},
        \\  {"label": "return", "kind": 14, "detail": "return statement", "documentation": "Return from function", "insertText": "return $0;"},
        \\  {"label": "break", "kind": 14, "detail": "break statement", "documentation": "Break from loop"},
        \\  {"label": "continue", "kind": 14, "detail": "continue statement", "documentation": "Continue to next iteration"},
        \\  {"label": "struct", "kind": 22, "detail": "struct type", "documentation": "Define a struct type\\n\\nSyntax: struct Name { fields }", "insertText": "struct $1 {\\n    $0\\n}"},
        \\  {"label": "enum", "kind": 22, "detail": "enum type", "documentation": "Define an enum type", "insertText": "enum $1 {\\n    $0\\n}"},
        \\  {"label": "type", "kind": 22, "detail": "type alias", "documentation": "Define a type alias", "insertText": "type $1 = $0;"},
        \\  {"label": "@import", "kind": 3, "detail": "import directive", "documentation": "Import a module", "insertText": "@import(\\\"$1\\\")"},
        \\  {"label": "std", "kind": 9, "detail": "standard library", "documentation": "The Howl standard library\\n\\nContains modules like debug, math, mem, fmt, etc.", "insertText": "std"},
        \\  {"label": "std.debug.print", "kind": 3, "detail": "Enhanced debug print", "documentation": "Enhanced debug print with format specifiers and anonymous struct arguments\\n\\nFormat specifiers:\\n- {} - default formatting\\n- {d} - decimal integer\\n- {s} - string\\n- {f} - float\\n- {f:.2} - float with precision\\n\\nExample: std.debug.print(\\\"{s}: {f:.2}\\\\n\\\", .{name, value})", "insertText": "std.debug.print(\\\"$1\\\", .$2)$0", "insertTextFormat": 2},
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
        \\  {"label": "=>", "kind": 24, "detail": "arrow operator", "documentation": "Pattern match arrow"},
        \\  {"label": ".{}", "kind": 15, "detail": "anonymous struct", "documentation": "Anonymous struct literal for function arguments\\n\\nExample: .{arg1, arg2, arg3}", "insertText": ".{$0}"}
        \\]
        ;
    }

    fn publishDiagnostics(self: *LspServer, uri: []const u8, writer: anytype) !void {
        _ = self;

        // Create a simple diagnostic notification (compact JSON)
        var diagnostic_buffer: [1024]u8 = undefined;
        const diagnostic_json = try std.fmt.bufPrint(&diagnostic_buffer, "{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{{\"uri\":\"{s}\",\"diagnostics\":[]}}}}", .{uri});

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

    fn handleDocumentFormatting(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        // Extract document URI from request
        var uri: []const u8 = "";
        var document_text: []const u8 = "";
        var should_free_document_text = false;
        var file_path: ?[]u8 = null;

        if (request.params) |params| {
            if (params.object.get("textDocument")) |text_doc| {
                if (text_doc.object.get("uri")) |uri_value| {
                    uri = uri_value.string;
                }
            }
        }

        // Try to get document text from our document store
        if (self.documents.getDocument(uri)) |doc| {
            document_text = doc.text;
        } else {
            // If document not found in store, try to read from file system
            file_path = uriToFilePath(self.allocator, uri) catch {
                try self.sendJsonResponse(request.id, "null", writer);
                return;
            };

            document_text = std.fs.cwd().readFileAlloc(self.allocator, file_path.?, 10 * 1024 * 1024) catch {
                self.allocator.free(file_path.?);
                try self.sendJsonResponse(request.id, "null", writer);
                return;
            };
            should_free_document_text = true;
        }

        // Ensure cleanup happens at function scope, not branch scope
        defer {
            if (file_path) |path| {
                self.allocator.free(path);
            }
            if (should_free_document_text) {
                self.allocator.free(document_text);
            }
        }

        // Parse formatting options from request
        var options = formatter.FormatterOptions{};
        if (request.params) |params| {
            if (params.object.get("options")) |fmt_options| {
                if (fmt_options.object.get("tabSize")) |tab_size| {
                    options.indent_size = @intCast(tab_size.integer);
                }
                if (fmt_options.object.get("insertSpaces")) |insert_spaces| {
                    options.use_tabs = !insert_spaces.bool;
                }
            }
        }

        // Format the document
        const format_result = formatter.formatCode(self.allocator, document_text, options) catch {
            try self.sendJsonResponse(request.id, "null", writer);
            return;
        };
        defer format_result.deinit(self.allocator);

        if (!format_result.success) {
            try self.sendJsonResponse(request.id, "null", writer);
            return;
        }

        // Create text edit for the entire document
        var response_buffer: [8192]u8 = undefined;
        const response_json = std.fmt.bufPrint(&response_buffer,
            \\[{{"range":{{"start":{{"line":0,"character":0}},"end":{{"line":999999,"character":999999}}}},"newText":"{s}"}}]
        , .{format_result.formatted_code}) catch {
            try self.sendJsonResponse(request.id, "null", writer);
            return;
        };

        try self.sendJsonResponse(request.id, response_json, writer);
    }

    fn handleDocumentRangeFormatting(self: *LspServer, request: lsp.LspRequest, writer: anytype) !void {
        // For now, range formatting will format the entire document
        // In a more sophisticated implementation, this would format only the specified range
        try self.handleDocumentFormatting(request, writer);
    }
};

fn uriToFilePath(allocator: std.mem.Allocator, uri: []const u8) ![]u8 {
    if (std.mem.startsWith(u8, uri, "file://")) {
        const path = uri[7..];
        return allocator.dupe(u8, path);
    }
    return allocator.dupe(u8, uri);
}
