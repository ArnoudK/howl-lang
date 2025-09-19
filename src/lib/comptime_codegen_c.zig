const std = @import("std");
const ast = @import("ast.zig");
const semantic = @import("semantic_analyzer.zig");
pub const ComptimeEvaluator = @import("comptime_evaluator.zig");

// ============================================================================
// Compile-time C Code Generation
// Generates C code on-demand for types like List<i32>, Optional<str>, etc.
// This system replaces traditional generics with Zig-like compile-time functions
// ============================================================================

pub const ComptimeTypeCodegen = struct {
    allocator: std.mem.Allocator,
    generated_types: std.StringHashMap([]const u8), // Cache of generated C types
    semantic_analyzer: *semantic.SemanticAnalyzer,

    pub fn init(
        allocator: std.mem.Allocator,
        semantic_analyzer: *semantic.SemanticAnalyzer,
    ) ComptimeTypeCodegen {
        return ComptimeTypeCodegen{
            .allocator = allocator,
            .generated_types = std.StringHashMap([]const u8).init(allocator),
            .semantic_analyzer = semantic_analyzer,
        };
    }

    pub fn deinit(self: *ComptimeTypeCodegen) void {
        var iter = self.generated_types.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.value_ptr.*);
        }
        self.generated_types.deinit();
    }

    /// Generate C type definition for a compile-time type call
    pub fn generateTypeDefinition(self: *ComptimeTypeCodegen, type_name: []const u8, args: []const ComptimeEvaluator.ComptimeArg) ![]const u8 {
        // Check if already generated
        if (self.generated_types.get(type_name)) |existing| {
            return existing;
        }

        // Generate based on type name
        var generated_code: []const u8 = undefined;

        if (std.mem.eql(u8, type_name, "List")) {
            generated_code = try self.generateListType(args);
        } else if (std.mem.eql(u8, type_name, "Optional")) {
            generated_code = try self.generateOptionalType(args);
        } else {
            // Unknown type - return placeholder
            generated_code = try std.fmt.allocPrint(self.allocator, "/* Unknown type: {s} */\nvoid", .{type_name});
        }

        // Cache the result
        try self.generated_types.put(type_name, generated_code);
        return generated_code;
    }

    fn generateListType(self: *ComptimeTypeCodegen, args: []const ComptimeEvaluator.ComptimeArg) ![]const u8 {
        if (args.len != 1) {
            return try std.fmt.allocPrint(self.allocator, "/* Error: List requires 1 argument, got {d} */\nvoid", .{args.len});
        }

        // For now, generate a simple dynamic array structure
        // In a real implementation, we'd analyze the element type
        return try std.fmt.allocPrint(self.allocator,
            \\typedef struct {{
            \\    void* data;
            \\    size_t length;
            \\    size_t capacity;
            \\}} List_generic;
            \\
        , .{});
    }

    fn generateOptionalType(self: *ComptimeTypeCodegen, args: []const ComptimeEvaluator.ComptimeArg) ![]const u8 {
        if (args.len != 1) {
            return try std.fmt.allocPrint(self.allocator, "/* Error: Optional requires 1 argument, got {d} */\nvoid", .{args.len});
        }

        // Generate optional wrapper structure
        return try std.fmt.allocPrint(self.allocator,
            \\typedef struct {{
            \\    int has_value;
            \\    void* value;
            \\}} Optional_generic;
            \\
        , .{});
    }

    /// Generate all type declarations that have been collected
    pub fn generateAllDeclarations(self: *ComptimeTypeCodegen) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        // Write all generated types to the result
        var iter = self.generated_types.iterator();
        while (iter.next()) |entry| {
            try result.appendSlice(entry.value_ptr.*);
            try result.appendSlice("\n\n");
        }

        return result.toOwnedSlice();
    }
};
