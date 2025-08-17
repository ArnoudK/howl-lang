const std = @import("std");
const lsp = @import("lsp.zig");
const lsp_logger = @import("lsp_logger.zig");

/// Safe error handling wrapper for LSP operations
pub const SafeResult = struct {
    success: bool,
    error_message: ?[]const u8 = null,
    
    pub fn ok() SafeResult {
        return SafeResult{ .success = true };
    }
    
    pub fn fail(message: []const u8) SafeResult {
        return SafeResult{ .success = false, .error_message = message };
    }
    
    pub fn fromError(err: anyerror, context: []const u8, allocator: std.mem.Allocator) SafeResult {
        const message = std.fmt.allocPrint(allocator, "Error in {s}: {}", .{ context, err }) catch "Unknown error";
        return SafeResult{ .success = false, .error_message = message };
    }
};

/// Enhanced error codes for LSP protocol
pub const LspErrorCode = enum(i32) {
    // Standard LSP error codes
    ParseError = -32700,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
    InvalidParams = -32602,
    InternalError = -32603,
    
    // Custom error codes
    DocumentNotFound = -32001,
    CompilationFailed = -32002,
    FormattingFailed = -32003,
    OutOfMemory = -32004,
    TimeoutError = -32005,
    
    pub fn toString(self: LspErrorCode) []const u8 {
        return switch (self) {
            .ParseError => "Parse error",
            .InvalidRequest => "Invalid request",
            .MethodNotFound => "Method not found",
            .InvalidParams => "Invalid params",
            .InternalError => "Internal error",
            .DocumentNotFound => "Document not found",
            .CompilationFailed => "Compilation failed",
            .FormattingFailed => "Formatting failed",
            .OutOfMemory => "Out of memory",
            .TimeoutError => "Operation timed out",
        };
    }
};

/// Safe JSON parsing with error recovery
pub fn safeParseJson(allocator: std.mem.Allocator, json_text: []const u8) SafeResult {
    _ = allocator;
    
    if (json_text.len == 0) {
        lsp_logger.warn("Attempted to parse empty JSON");
        return SafeResult.fail("Empty JSON input");
    }
    
    // Basic validation - check for balanced braces
    var brace_count: i32 = 0;
    var bracket_count: i32 = 0;
    var in_string = false;
    var escape_next = false;
    
        for (json_text) |char| {
            if (escape_next) {
                escape_next = false;
                continue;
            }
            
            switch (char) {
                '"' => in_string = !in_string,
                '\\' => {
                    if (in_string) {
                        escape_next = true;
                    }
                },
                '{' => {
                    if (!in_string) {
                        brace_count += 1;
                    }
                },
                '}' => {
                    if (!in_string) {
                        brace_count -= 1;
                    }
                },
                '[' => {
                    if (!in_string) {
                        bracket_count += 1;
                    }
                },
                ']' => {
                    if (!in_string) {
                        bracket_count -= 1;
                    }
                },
                else => {},
            }
        }
    
    if (brace_count != 0) {
        lsp_logger.warn("JSON has unbalanced braces: {}", .{brace_count});
        return SafeResult.fail("Unbalanced braces in JSON");
    }
    
    if (bracket_count != 0) {
        lsp_logger.warn("JSON has unbalanced brackets: {}", .{bracket_count});
        return SafeResult.fail("Unbalanced brackets in JSON");
    }
    
    return SafeResult.ok();
}

/// Safe JSON value extraction
pub fn safeGetString(json_value: std.json.Value, key: []const u8) ?[]const u8 {
    if (json_value != .object) {
        lsp_logger.warn("Expected object, got {}", .{json_value});
        return null;
    }
    
    const value = json_value.object.get(key) orelse {
        lsp_logger.debug("Key '{s}' not found in JSON object", .{key});
        return null;
    };
    
    return switch (value) {
        .string => |s| s,
        else => {
            lsp_logger.warn("Expected string for key '{s}', got {}", .{ key, value });
            return null;
        },
    };
}

pub fn safeGetInteger(json_value: std.json.Value, key: []const u8) ?i64 {
    if (json_value != .object) {
        lsp_logger.warn("Expected object, got {}", .{json_value});
        return null;
    }
    
    const value = json_value.object.get(key) orelse {
        lsp_logger.debug("Key '{s}' not found in JSON object", .{key});
        return null;
    };
    
    return switch (value) {
        .integer => |i| i,
        .float => |f| @intFromFloat(f),
        else => {
            lsp_logger.warn("Expected number for key '{s}', got {}", .{ key, value });
            return null;
        },
    };
}

pub fn safeGetBool(json_value: std.json.Value, key: []const u8) ?bool {
    if (json_value != .object) {
        lsp_logger.warn("Expected object, got {}", .{json_value});
        return null;
    }
    
    const value = json_value.object.get(key) orelse {
        lsp_logger.debug("Key '{s}' not found in JSON object", .{key});
        return null;
    };
    
    return switch (value) {
        .bool => |b| b,
        else => {
            lsp_logger.warn("Expected boolean for key '{s}', got {}", .{ key, value });
            null;
        },
    };
}

/// Safe memory allocation wrapper
pub fn safeAlloc(allocator: std.mem.Allocator, comptime T: type, count: usize) ?[]T {
    return allocator.alloc(T, count) catch |alloc_err| {
        lsp_logger.logError(alloc_err, "memory allocation");
        return null;
    };
}

pub fn safeDupe(allocator: std.mem.Allocator, comptime T: type, slice: []const T) ?[]T {
    return allocator.dupe(T, slice) catch |dupe_err| {
        lsp_logger.logError(dupe_err, "memory duplication");
        return null;
    };
}

/// Safe file operations
pub fn safeReadFile(allocator: std.mem.Allocator, path: []const u8, max_size: usize) ?[]u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |file_err| {
        lsp_logger.logError(file_err, "file opening");
        return null;
    };
    defer file.close();
    
    return file.readToEndAlloc(allocator, max_size) catch |read_err| {
        lsp_logger.logError(read_err, "file reading");
        return null;
    };
}

/// Crash prevention wrapper for LSP message handlers
pub fn safeLspHandler(
    comptime handler_fn: anytype,
    args: anytype,
    context_name: []const u8,
    writer: anytype,
    request_id: ?std.json.Value,
) void {
    const result = @call(.auto, handler_fn, args) catch |err| {
        lsp_logger.err("Handler '{}' crashed with error: {}", .{ context_name, err });
        sendInternalError(writer, request_id, err, context_name);
        return;
    };
    
    _ = result; // Handler might return void or a result
    lsp_logger.debug("Handler '{s}' completed successfully", .{context_name});
}

/// Send standardized error responses
pub fn sendInternalError(writer: anytype, request_id: ?std.json.Value, err: anyerror, context: []const u8) void {
    const error_code = switch (err) {
        error.OutOfMemory => LspErrorCode.OutOfMemory,
        error.FileNotFound => LspErrorCode.DocumentNotFound,
        else => LspErrorCode.InternalError,
    };
    
    const error_json = std.fmt.allocPrint(
        std.heap.page_allocator,
        "{{\"code\": {}, \"message\": \"{s}: {}\"}}", 
        .{ @intFromEnum(error_code), error_code.toString(), err }
    ) catch "Internal error formatting failed";
    defer std.heap.page_allocator.free(error_json);
    
    sendErrorResponse(writer, request_id, error_json) catch {
        lsp_logger.err("Failed to send error response for context: {s}", .{context});
    };
}

pub fn sendErrorResponse(writer: anytype, request_id: ?std.json.Value, error_json: []const u8) !void {
    var response_buffer: [2048]u8 = undefined;
    
    // Build error response
    var stream = std.io.fixedBufferStream(&response_buffer);
    const response_writer = stream.writer();
    
    try response_writer.print("{{\"jsonrpc\":\"2.0\",\"id\":", .{});
    
    if (request_id) |id| {
        try std.json.stringify(id, .{}, response_writer);
    } else {
        try response_writer.print("null", .{});
    }
    
    try response_writer.print(",\"error\":{s}}}", .{error_json});
    
    const response = stream.getWritten();
    
    // Send with proper Content-Length header
    try writer.print("Content-Length: {d}\r\n\r\n{s}", .{ response.len, response });
}

/// Safe string operations
pub fn safeStringEql(a: ?[]const u8, b: []const u8) bool {
    return if (a) |str| std.mem.eql(u8, str, b) else false;
}

pub fn safeStringContains(haystack: ?[]const u8, needle: []const u8) bool {
    return if (haystack) |str| std.mem.indexOf(u8, str, needle) != null else false;
}

/// Timeout wrapper for potentially long operations
pub const TimeoutContext = struct {
    start_time: i64,
    timeout_ms: i64,
    
    pub fn init(timeout_ms: i64) TimeoutContext {
        return TimeoutContext{
            .start_time = std.time.milliTimestamp(),
            .timeout_ms = timeout_ms,
        };
    }
    
    pub fn checkTimeout(self: *const TimeoutContext) bool {
        const elapsed = std.time.milliTimestamp() - self.start_time;
        return elapsed > self.timeout_ms;
    }
    
    pub fn remainingMs(self: *const TimeoutContext) i64 {
        const elapsed = std.time.milliTimestamp() - self.start_time;
        return @max(0, self.timeout_ms - elapsed);
    }
};

/// Enhanced panic handler for LSP server
pub fn lspPanicHandler(msg: []const u8, stack_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    _ = ret_addr;
    _ = stack_trace;
    
    lsp_logger.err("LSP SERVER PANIC: {s}", .{msg});
    lsp_logger.err("LSP server is shutting down due to panic");
    
    // Try to flush logs before exit
    if (lsp_logger.getLogger()) |logger| {
        if (logger.file) |file| {
            file.sync() catch {};
        }
    }
    
    std.process.exit(1);
}

/// Memory usage monitoring
pub const MemoryMonitor = struct {
    allocator: std.mem.Allocator,
    allocated: std.atomic.Value(usize),
    peak_usage: std.atomic.Value(usize),
    warning_threshold: usize,
    
    pub fn init(allocator: std.mem.Allocator, warning_threshold_mb: usize) MemoryMonitor {
        return MemoryMonitor{
            .allocator = allocator,
            .allocated = std.atomic.Value(usize).init(0),
            .peak_usage = std.atomic.Value(usize).init(0),
            .warning_threshold = warning_threshold_mb * 1024 * 1024,
        };
    }
    
    pub fn trackAllocation(self: *MemoryMonitor, size: usize) void {
        const new_total = self.allocated.fetchAdd(size, .AcqRel) + size;
        
        // Update peak if necessary
        var current_peak = self.peak_usage.load(.Acquire);
        while (new_total > current_peak) {
            const old_peak = self.peak_usage.compareAndSwap(current_peak, new_total, .AcqRel, .Acquire);
            if (old_peak == current_peak) break;
            current_peak = old_peak orelse new_total;
        }
        
        // Check warning threshold
        if (new_total > self.warning_threshold) {
            lsp_logger.warn("Memory usage warning: {} MB (peak: {} MB)", .{
                new_total / (1024 * 1024),
                current_peak / (1024 * 1024)
            });
        }
    }
    
    pub fn trackDeallocation(self: *MemoryMonitor, size: usize) void {
        _ = self.allocated.fetchSub(size, .AcqRel);
    }
    
    pub fn getCurrentUsage(self: *const MemoryMonitor) usize {
        return self.allocated.load(.Acquire);
    }
    
    pub fn getPeakUsage(self: *const MemoryMonitor) usize {
        return self.peak_usage.load(.Acquire);
    }
};