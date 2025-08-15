const std = @import("std");
const ErrorSystem = @import("./error_system.zig");
const CompilerError = ErrorSystem.CompilerError;
const ErrorCollector = ErrorSystem.ErrorCollector;
const SourceMap = ErrorSystem.SourceMap;
const ErrorSeverity = ErrorSystem.ErrorSeverity;

/// Formatter for displaying errors in a user-friendly way
pub const ErrorFormatter = struct {
    allocator: std.mem.Allocator,
    use_colors: bool,
    show_line_numbers: bool,
    max_context_lines: usize,
    
    const RESET = "\x1b[0m";
    const BOLD = "\x1b[1m";
    const DIM = "\x1b[2m";
    
    pub fn init(allocator: std.mem.Allocator) ErrorFormatter {
        return ErrorFormatter{
            .allocator = allocator,
            .use_colors = true,
            .show_line_numbers = true,
            .max_context_lines = 5,  // Increased from 3 to 5 for more context
        };
    }
    
    /// Format all errors in the collector and write to writer
    pub fn formatErrors(
        self: *const ErrorFormatter,
        collector: *const ErrorCollector,
        source_maps: *const std.StringHashMap(SourceMap),
        writer: anytype,
    ) !void {
        if (collector.errors.items.len == 0) {
            return;
        }
        
        // Print summary
        const error_count = collector.errorCount();
        const warning_count = collector.warningCount();
        
        if (error_count > 0) {
            try self.writeColored(writer, .error_, "error");
            try writer.print(": {d} error{s}", .{ error_count, if (error_count == 1) "" else "s" });
            
            if (warning_count > 0) {
                try writer.print(", ", .{});
                try self.writeColored(writer, .warning, "warning");
                try writer.print(": {d} warning{s}", .{ warning_count, if (warning_count == 1) "" else "s" });
            }
            try writer.print("\n\n", .{});
        } else if (warning_count > 0) {
            try self.writeColored(writer, .warning, "warning");
            try writer.print(": {d} warning{s}\n\n", .{ warning_count, if (warning_count == 1) "" else "s" });
        }
        
        // Format each error
        for (collector.errors.items) |err| {
            try self.formatSingleError(err, source_maps, writer);
            try writer.print("\n", .{});
        }
    }
    
    /// Format a single error with full context
    pub fn formatSingleError(
        self: *const ErrorFormatter,
        err: *const CompilerError,
        source_maps: *const std.StringHashMap(SourceMap),
        writer: anytype,
    ) !void {
        // Error header: severity + code + message
        try self.writeColored(writer, err.severity, err.severity.toString());
        try writer.print("[{s}]: {s}\n", .{ err.code.toString(), err.message });
        
        // File location
        try self.writeColored(writer, .note, "  -->");
        try writer.print(" {s}:{d}:{d}\n", .{ err.span.file_path, err.span.line, err.span.column });
        
        // Source context if available
        if (source_maps.get(err.span.file_path)) |source_map| {
            try self.formatSourceContext(err, &source_map, writer);
        }
        
        // Suggestions
        if (err.suggestions.len > 0) {
            try writer.print("\n", .{});
            for (err.suggestions) |suggestion| {
                try self.writeColored(writer, .note, "help");
                try writer.print(": {s}\n", .{suggestion});
            }
        }
        
        // Related errors
        if (err.related_errors.len > 0) {
            try writer.print("\n", .{});
            for (err.related_errors) |related| {
                try self.writeColored(writer, .note, "note");
                try writer.print(": related error at {s}:{d}:{d}: {s}\n", .{
                    related.span.file_path,
                    related.span.line,
                    related.span.column,
                    related.message,
                });
            }
        }
    }
    
    /// Format source code context around the error
    fn formatSourceContext(
        self: *const ErrorFormatter,
        err: *const CompilerError,
        source_map: *const SourceMap,
        writer: anytype,
    ) !void {
        const line_num = err.span.line;
        const start_line = if (line_num > self.max_context_lines) line_num - self.max_context_lines else 1;
        const end_line = line_num + self.max_context_lines;
        
        // Calculate the width needed for line numbers
        const max_line_width = std.fmt.count("{d}", .{end_line});
        
        // Add a separator line above the context for clarity
        try writer.print("   |\n", .{});
        
        // Show context lines before the error
        var current_line = start_line;
        while (current_line < line_num) : (current_line += 1) {
            const line_text = source_map.getLineText(current_line);
            if (line_text.len > 0) { // Only show non-empty lines
                try self.formatContextLine(writer, current_line, line_text, max_line_width, false);
            }
        }
        
        // Show the error line with highlighting
        const error_line_text = source_map.getLineText(line_num);
        try self.formatContextLine(writer, line_num, error_line_text, max_line_width, true);
        
        // Show the error pointer
        try self.formatErrorPointer(writer, err, max_line_width);
        
        // Show context lines after the error
        current_line = line_num + 1;
        while (current_line <= end_line) : (current_line += 1) {
            const line_text = source_map.getLineText(current_line);
            if (line_text.len == 0) break; // No more lines
            try self.formatContextLine(writer, current_line, line_text, max_line_width, false);
        }
        
        // Add a separator line below the context for clarity
        try writer.print("   |\n", .{});
    }
    
    fn formatContextLine(
        self: *const ErrorFormatter,
        writer: anytype,
        line_num: usize,
        line_text: []const u8,
        _: usize,
        is_error_line: bool,
    ) !void {
        if (self.show_line_numbers) {
            if (is_error_line and self.use_colors) {
                try writer.print("{s}{d:>8}{s} | ", .{ BOLD, line_num, RESET });
            } else {
                try self.writeColored(writer, .note, "");
                try writer.print("{d:>8} | ", .{line_num});
                if (self.use_colors) try writer.print(RESET, .{});
            }
        } else {
            try writer.print("   | ", .{});
        }
        
        // Highlight the entire error line
        if (is_error_line and self.use_colors) {
            try writer.print("{s}{s}{s}\n", .{ BOLD, line_text, RESET });
        } else {
            try writer.print("{s}\n", .{line_text});
        }
    }
    
    fn formatErrorPointer(
        self: *const ErrorFormatter,
        writer: anytype,
        err: *const CompilerError,
        _: usize,
    ) !void {
        // Create the pointer line
        const pointer_prefix = if (self.show_line_numbers) 
            try std.fmt.allocPrint(self.allocator, "{s:>8} | ", .{""})
        else 
            try self.allocator.dupe(u8, "   | ");
        defer self.allocator.free(pointer_prefix);
        
        // Calculate spaces before the caret
        const spaces_before = if (err.span.column > 0) err.span.column - 1 else 0;
        const span_length = if (err.span.end_pos > err.span.start_pos) 
            err.span.end_pos - err.span.start_pos 
        else 
            1;
        
        // Cap span length to reasonable size to avoid super long underlines
        const max_span_length = 50;
        const actual_span_length = @min(span_length, max_span_length);
        
        try writer.print("{s}", .{pointer_prefix});
        
        // Write spaces before the pointer
        for (0..spaces_before) |_| {
            try writer.print(" ", .{});
        }
        
        // Write the pointer(s) with color
        try self.writeColored(writer, err.severity, "");
        if (actual_span_length == 1) {
            try writer.print("^", .{});
        } else {
            try writer.print("^", .{});
            for (1..actual_span_length) |_| {
                try writer.print("~", .{});
            }
        }
        if (self.use_colors) try writer.print(RESET, .{});
        
        // Add the error message on the same line if it's short
        if (err.message.len < 50) {
            try writer.print(" {s}", .{err.message});
        }
        
        // If the error span was truncated, indicate it
        if (span_length > max_span_length) {
            try writer.print(" (span truncated)", .{});
        }
        
        try writer.print("\n", .{});
    }
    
    fn writeColored(
        self: *const ErrorFormatter,
        writer: anytype,
        severity: ErrorSeverity,
        text: []const u8,
    ) !void {
        if (self.use_colors) {
            try writer.print("{s}{s}{s}", .{ severity.color(), text, RESET });
        } else {
            try writer.print("{s}", .{text});
        }
    }
    
    /// Create a quick error summary for IDEs or other tools
    pub fn formatErrorsSummary(
        _: *const ErrorFormatter,
        collector: *const ErrorCollector,
        writer: anytype,
    ) !void {
        for (collector.errors.items) |err| {
            try writer.print("{s}:{d}:{d}: {s}: {s}\n", .{
                err.span.file_path,
                err.span.line,
                err.span.column,
                err.severity.toString(),
                err.message,
            });
        }
    }
    
    /// Format errors in JSON format for tooling integration
    pub fn formatErrorsJson(
        _: *const ErrorFormatter,
        collector: *const ErrorCollector,
        writer: anytype,
    ) !void {
        try writer.print("{{\n  \"errors\": [\n", .{});
        
        for (collector.errors.items, 0..) |err, i| {
            if (i > 0) try writer.print(",\n", .{});
            
            try writer.print("    {{\n", .{});
            try writer.print("      \"code\": \"{s}\",\n", .{err.code.toString()});
            try writer.print("      \"severity\": \"{s}\",\n", .{err.severity.toString()});
            try writer.print("      \"category\": \"{s}\",\n", .{err.category.toString()});
            try writer.print("      \"message\": \"{s}\",\n", .{err.message});
            try writer.print("      \"file\": \"{s}\",\n", .{err.span.file_path});
            try writer.print("      \"line\": {d},\n", .{err.span.line});
            try writer.print("      \"column\": {d},\n", .{err.span.column});
            try writer.print("      \"start_pos\": {d},\n", .{err.span.start_pos});
            try writer.print("      \"end_pos\": {d}", .{err.span.end_pos});
            
            if (err.suggestions.len > 0) {
                try writer.print(",\n      \"suggestions\": [", .{});
                for (err.suggestions, 0..) |suggestion, j| {
                    if (j > 0) try writer.print(", ", .{});
                    try writer.print("\"{s}\"", .{suggestion});
                }
                try writer.print("]", .{});
            }
            
            try writer.print("\n    }}", .{});
        }
        
        try writer.print("\n  ],\n", .{});
        try writer.print("  \"error_count\": {d},\n", .{collector.errorCount()});
        try writer.print("  \"warning_count\": {d}\n", .{collector.warningCount()});
        try writer.print("}}\n", .{});
    }
};