const std = @import("std");

/// Position information for errors, including file location and span
pub const SourceSpan = struct {
    file_path: []const u8,
    start_pos: usize,
    end_pos: usize,
    line: usize,
    column: usize,
    
    pub fn single(file_path: []const u8, pos: usize, line: usize, column: usize) SourceSpan {
        return SourceSpan{
            .file_path = file_path,
            .start_pos = pos,
            .end_pos = pos,
            .line = line,
            .column = column,
        };
    }
    
    pub fn range(file_path: []const u8, start_pos: usize, end_pos: usize, start_line: usize, start_column: usize) SourceSpan {
        return SourceSpan{
            .file_path = file_path,
            .start_pos = start_pos,
            .end_pos = end_pos,
            .line = start_line,
            .column = start_column,
        };
    }
};

/// Error severity levels
pub const ErrorSeverity = enum {
    note,
    warning,
    error_,
    fatal,
    
    pub fn toString(self: ErrorSeverity) []const u8 {
        return switch (self) {
            .note => "note",
            .warning => "warning", 
            .error_ => "error",
            .fatal => "fatal",
        };
    }
    
    pub fn color(self: ErrorSeverity) []const u8 {
        return switch (self) {
            .note => "\x1b[36m",      // cyan
            .warning => "\x1b[33m",   // yellow
            .error_ => "\x1b[31m",    // red
            .fatal => "\x1b[35m",     // magenta
        };
    }
};

/// Error categories for better organization
pub const ErrorCategory = enum {
    lexer,
    parser,
    semantic,
    codegen,
    io,
    internal,
    
    pub fn toString(self: ErrorCategory) []const u8 {
        return switch (self) {
            .lexer => "lexer",
            .parser => "parser",
            .semantic => "semantic",
            .codegen => "codegen",
            .io => "io",
            .internal => "internal",
        };
    }
};

/// Error codes for specific error types
pub const ErrorCode = enum {
    // Lexer errors
    invalid_character,
    unterminated_string,
    invalid_number_format,
    invalid_escape_sequence,
    
    // Parser errors
    unexpected_token,
    missing_semicolon,
    missing_closing_brace,
    missing_closing_paren,
    invalid_expression,
    invalid_statement,
    invalid_declaration,
    expected_identifier,
    
    // Whitespace enforcement errors
    missing_space_around_operator,
    missing_space_before_brace,
    missing_space_after_keyword,
    multiple_statements_per_line,
    missing_newline_after_statement,
    
    // Semantic errors
    undefined_variable,
    type_mismatch,
    duplicate_declaration,
    invalid_member_access,
    invalid_function_call,
    wrong_argument_count,
    return_type_mismatch,
    missing_return_value,
    binary_operation_type_error,
    invalid_enum_value,  // Enum values must be in ascending order and compile-time constants
    compile_error,  // Triggered by @compileError builtin
    
    // Codegen errors
    unsupported_operation,
    target_specific_error,
    optimization_failed,
    
    // IO errors
    file_not_found,
    permission_denied,
    
    // Internal errors
    out_of_memory,
    assertion_failed,
    
    pub fn toString(self: ErrorCode) []const u8 {
        return switch (self) {
            .invalid_character => "E001",
            .unterminated_string => "E002", 
            .invalid_number_format => "E003",
            .invalid_escape_sequence => "E004",
            .unexpected_token => "E100",
            .missing_semicolon => "E101",
            .missing_closing_brace => "E102",
            .missing_closing_paren => "E103",
            .invalid_expression => "E104",
            .invalid_statement => "E105",
            .invalid_declaration => "E106",
            .expected_identifier => "E107",
            .missing_space_around_operator => "E108",
            .missing_space_before_brace => "E109", 
            .missing_space_after_keyword => "E110",
            .multiple_statements_per_line => "E111",
            .missing_newline_after_statement => "E112",
            .undefined_variable => "E200",
            .type_mismatch => "E201",
            .duplicate_declaration => "E202",
            .invalid_member_access => "E203",
            .invalid_function_call => "E204",
            .wrong_argument_count => "E205",
            .return_type_mismatch => "E206",
            .missing_return_value => "E207",
            .binary_operation_type_error => "E208",
            .invalid_enum_value => "E209",
            .compile_error => "E210",
            .unsupported_operation => "E300",
            .target_specific_error => "E301",
            .optimization_failed => "E302",
            .file_not_found => "E400",
            .permission_denied => "E401",
            .out_of_memory => "E500",
            .assertion_failed => "E501",
        };
    }
    
    pub fn defaultMessage(self: ErrorCode) []const u8 {
        return switch (self) {
            .invalid_character => "Invalid character",
            .unterminated_string => "Unterminated string literal",
            .invalid_number_format => "Invalid number format",
            .invalid_escape_sequence => "Invalid escape sequence",
            .unexpected_token => "Unexpected token",
            .missing_semicolon => "Missing semicolon",
            .missing_closing_brace => "Missing closing brace '}'",
            .missing_closing_paren => "Missing closing parenthesis ')'",
            .invalid_expression => "Invalid expression",
            .invalid_statement => "Invalid statement",
            .invalid_declaration => "Invalid declaration",
            .expected_identifier => "Expected identifier",
            .missing_space_around_operator => "Missing space around operator",
            .missing_space_before_brace => "Missing space before '{'",
            .missing_space_after_keyword => "Missing space after keyword",
            .multiple_statements_per_line => "Multiple statements on one line",
            .missing_newline_after_statement => "Missing newline after statement",
            .undefined_variable => "Undefined variable",
            .type_mismatch => "Type mismatch",
            .duplicate_declaration => "Duplicate declaration",
            .invalid_member_access => "Invalid member access",
            .invalid_function_call => "Invalid function call",
            .wrong_argument_count => "Wrong number of arguments",
            .return_type_mismatch => "Return type mismatch",
            .missing_return_value => "Missing return value",
            .binary_operation_type_error => "Binary operation type error",
            .invalid_enum_value => "Invalid enum value",
            .compile_error => "Compile-time error",
            .unsupported_operation => "Unsupported operation",
            .target_specific_error => "Target-specific error",
            .optimization_failed => "Optimization failed",
            .file_not_found => "File not found",
            .permission_denied => "Permission denied",
            .out_of_memory => "Out of memory",
            .assertion_failed => "Internal assertion failed",
        };
    }
};

/// A single error with context and suggestions
pub const CompilerError = struct {
    code: ErrorCode,
    category: ErrorCategory,
    severity: ErrorSeverity,
    message: []const u8,
    span: SourceSpan,
    suggestions: []const []const u8,
    related_errors: []const *CompilerError,
    
    pub fn create(
        allocator: std.mem.Allocator,
        code: ErrorCode,
        category: ErrorCategory,
        severity: ErrorSeverity,
        message: []const u8,
        span: SourceSpan,
    ) !*CompilerError {
        const error_ptr = try allocator.create(CompilerError);
        error_ptr.* = CompilerError{
            .code = code,
            .category = category,
            .severity = severity,
            .message = try allocator.dupe(u8, message),
            .span = span,
            .suggestions = &[_][]const u8{},
            .related_errors = &[_]*CompilerError{},
        };
        return error_ptr;
    }
    
    pub fn withSuggestions(self: *CompilerError, allocator: std.mem.Allocator, suggestions: []const []const u8) !void {
        // Free existing suggestions if any
        for (self.suggestions) |suggestion| {
            allocator.free(suggestion);
        }
        if (self.suggestions.len > 0) {
            allocator.free(self.suggestions);
        }
        
        // Allocate new suggestions
        var new_suggestions = try allocator.alloc([]const u8, suggestions.len);
        for (suggestions, 0..) |suggestion, i| {
            new_suggestions[i] = try allocator.dupe(u8, suggestion);
        }
        self.suggestions = new_suggestions;
    }
    
    pub fn deinit(self: *CompilerError, allocator: std.mem.Allocator) void {
        allocator.free(self.message);
        for (self.suggestions) |suggestion| {
            allocator.free(suggestion);
        }
        allocator.free(self.suggestions);
        allocator.destroy(self);
    }
};

/// Error collector that accumulates errors across compilation stages
pub const ErrorCollector = struct {
    allocator: std.mem.Allocator,
    errors: std.ArrayList(*CompilerError),
    has_fatal_error: bool,
    max_errors: usize,
    
    pub fn init(allocator: std.mem.Allocator) ErrorCollector {
        return ErrorCollector{
            .allocator = allocator,
            .errors = std.ArrayList(*CompilerError).init(allocator),
            .has_fatal_error = false,
            .max_errors = 100, // Reasonable default
        };
    }
    
    pub fn deinit(self: *ErrorCollector) void {
        for (self.errors.items) |err| {
            err.deinit(self.allocator);
        }
        self.errors.deinit();
    }
    
    pub fn addError(self: *ErrorCollector, err: *CompilerError) !void {
        if (self.errors.items.len >= self.max_errors) {
            return; // Stop collecting errors if we hit the limit
        }
        
        try self.errors.append(err);
        
        if (err.severity == .fatal) {
            self.has_fatal_error = true;
        }
    }
    
    pub fn createAndAddError(
        self: *ErrorCollector,
        code: ErrorCode,
        category: ErrorCategory,
        severity: ErrorSeverity,
        message: []const u8,
        span: SourceSpan,
    ) !*CompilerError {
        const err = try CompilerError.create(self.allocator, code, category, severity, message, span);
        try self.addError(err);
        return err;
    }
    
    pub fn hasErrors(self: *const ErrorCollector) bool {
        for (self.errors.items) |err| {
            if (err.severity == .error_ or err.severity == .fatal) {
                return true;
            }
        }
        return false;
    }
    
    pub fn hasWarnings(self: *const ErrorCollector) bool {
        for (self.errors.items) |err| {
            if (err.severity == .warning) {
                return true;
            }
        }
        return false;
    }
    
    pub fn errorCount(self: *const ErrorCollector) usize {
        var count: usize = 0;
        for (self.errors.items) |err| {
            if (err.severity == .error_ or err.severity == .fatal) {
                count += 1;
            }
        }
        return count;
    }
    
    pub fn warningCount(self: *const ErrorCollector) usize {
        var count: usize = 0;
        for (self.errors.items) |err| {
            if (err.severity == .warning) {
                count += 1;
            }
        }
        return count;
    }
    
    /// Sort errors by file and position for better output
    pub fn sortErrors(self: *ErrorCollector) void {
        std.sort.block(*CompilerError, self.errors.items, {}, compareErrors);
    }
    
    fn compareErrors(_: void, a: *CompilerError, b: *CompilerError) bool {
        // First sort by file path
        const file_cmp = std.mem.order(u8, a.span.file_path, b.span.file_path);
        if (file_cmp != .eq) {
            return file_cmp == .lt;
        }
        
        // Then by position
        if (a.span.start_pos != b.span.start_pos) {
            return a.span.start_pos < b.span.start_pos;
        }
        
        // Finally by severity (errors before warnings)
        const a_priority = switch (a.severity) {
            .fatal => 0,
            .error_ => 1,
            .warning => 2,
            .note => 3,
        };
        const b_priority = switch (b.severity) {
            .fatal => 0,
            .error_ => 1,
            .warning => 2,
            .note => 3,
        };
        
        return a_priority < b_priority;
    }
};

/// Utility to track file positions and calculate line/column numbers
pub const SourceMap = struct {
    file_content: []const u8,
    line_starts: std.ArrayList(usize),
    
    pub fn init(allocator: std.mem.Allocator, file_content: []const u8) !SourceMap {
        var line_starts = std.ArrayList(usize).init(allocator);
        try line_starts.append(0); // First line starts at position 0
        
        for (file_content, 0..) |char, i| {
            if (char == '\n') {
                try line_starts.append(i + 1);
            }
        }
        
        return SourceMap{
            .file_content = file_content,
            .line_starts = line_starts,
        };
    }
    
    pub fn deinit(self: *SourceMap) void {
        self.line_starts.deinit();
    }
    
    pub fn getLineColumn(self: *const SourceMap, pos: usize) struct { line: usize, column: usize } {
        // Binary search to find the line
        var left: usize = 0;
        var right: usize = self.line_starts.items.len;
        
        while (left < right) {
            const mid = (left + right) / 2;
            if (self.line_starts.items[mid] <= pos) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }
        
        const line = if (left > 0) left - 1 else 0;
        const line_start = self.line_starts.items[line];
        const column = pos - line_start;
        
        return .{ .line = line + 1, .column = column + 1 }; // 1-indexed
    }
    
    pub fn getLineText(self: *const SourceMap, line: usize) []const u8 {
        if (line == 0 or line > self.line_starts.items.len) {
            return "";
        }
        
        const line_start = self.line_starts.items[line - 1];
        const line_end = if (line < self.line_starts.items.len) 
            self.line_starts.items[line] - 1 // Exclude the newline
        else 
            self.file_content.len;
            
        return self.file_content[line_start..line_end];
    }
};

/// Error recovery tokens for parser to continue after errors
pub const RecoveryToken = enum {
    semicolon,
    closing_brace,
    closing_paren,
    newline,
    eof,
    
    pub fn fromTokenKind(token_kind: anytype) ?RecoveryToken {
        // This would need to be implemented based on your token types
        // For now, returning null to avoid compilation errors
        _ = token_kind;
        return null;
    }
};

/// Context for error recovery during parsing
pub const ErrorRecoveryContext = struct {
    expected_tokens: []const RecoveryToken,
    recovery_point: ?usize,
    
    pub fn init(expected_tokens: []const RecoveryToken) ErrorRecoveryContext {
        return ErrorRecoveryContext{
            .expected_tokens = expected_tokens,
            .recovery_point = null,
        };
    }
    
    pub fn canRecover(self: *const ErrorRecoveryContext, token_kind: anytype) bool {
        const recovery_token = RecoveryToken.fromTokenKind(token_kind) orelse return false;
        
        for (self.expected_tokens) |expected| {
            if (expected == recovery_token) {
                return true;
            }
        }
        return false;
    }
};