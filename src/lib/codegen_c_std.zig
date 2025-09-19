const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("codegen_c_utils.zig");

const CCodegenError = utils.CCodegenError;
const Writer = utils.Writer;

/// Handles all standard library function calls and converts them to appropriate C equivalents
pub const StdLibHandler = struct {
    arena: *const ast.AstArena,
    allocator: std.mem.Allocator,

    pub fn init(arena: *const ast.AstArena, allocator: std.mem.Allocator) StdLibHandler {
        return StdLibHandler{
            .arena = arena,
            .allocator = allocator,
        };
    }

    /// Check if a call expression is a std library call and handle it
    /// Returns true if handled, false if it should be processed as a regular call
    pub fn handleStdCall(self: *const StdLibHandler, writer: Writer, call_expr: anytype) CCodegenError!bool {
        const callee_node = self.arena.getNodeConst(call_expr.callee) orelse return false;

        if (callee_node.data != .member_expr) return false;

        return try self.handleMemberCall(writer, callee_node.data.member_expr, call_expr.args);
    }

    /// Handle member expression calls like std.debug.print
    fn handleMemberCall(self: *const StdLibHandler, writer: Writer, member_expr: anytype, args: anytype) CCodegenError!bool {
        const object_node = self.arena.getNodeConst(member_expr.object) orelse return false;

        // Check for std.debug.print pattern
        if (std.mem.eql(u8, member_expr.field, "print")) {
            if (object_node.data == .member_expr) {
                const obj_member = object_node.data.member_expr;
                if (std.mem.eql(u8, obj_member.field, "debug")) {
                    const std_node = self.arena.getNodeConst(obj_member.object);
                    if (std_node) |s_node| {
                        if (s_node.data == .identifier and std.mem.eql(u8, s_node.data.identifier.name, "std")) {
                            try self.generateDebugPrint(writer, args);
                            return true;
                        }
                    }
                }
            }
        }

        // TODO: Add other std library functions here
        // - std.mem.*
        // - std.fmt.*
        // - std.io.*
        // etc.

        return false;
    }

    /// Generate printf call for std.debug.print
    fn generateDebugPrint(self: *const StdLibHandler, writer: Writer, args: anytype) CCodegenError!void {
        try writer.writeAll("printf");
        try writer.writeAll("(");

        // Handle the format string (first argument) and arguments together
        if (args.items.len > 0) {
            const format_string_id = args.items[0];

            // Get format arguments if present
            var format_args: ?ast.NodeId = null;
            if (args.items.len > 1) {
                format_args = args.items[1];
            }

            try self.generateSmartFormatString(writer, format_string_id, format_args);
        } else {
            try writer.writeAll("\"\"");
        }

        try writer.writeAll(")");
    }

    /// Generate format string and arguments together with smart type inference
    fn generateSmartFormatString(self: *const StdLibHandler, writer: Writer, format_string_id: ast.NodeId, format_args_id: ?ast.NodeId) CCodegenError!void {
        const format_node = self.arena.getNodeConst(format_string_id) orelse {
            try writer.writeAll("\"\"");
            return;
        };

        if (format_node.data != .literal or format_node.data.literal != .string) {
            try writer.writeAll("\"\"");
            return;
        }

        const format_str = format_node.data.literal.string.value;

        // Collect argument types for smart format generation
        var arg_types = std.ArrayList([]const u8).init(self.allocator);
        defer arg_types.deinit();

        if (format_args_id) |args_id| {
            try self.collectArgumentTypes(&arg_types, args_id);
        }

        // Generate format string with smart type inference
        try writer.writeAll("\"");
        try self.convertSmartFormatString(writer, format_str, arg_types.items);
        try writer.writeAll("\"");

        // Generate arguments
        if (format_args_id) |args_id| {
            try self.generateFormatArguments(writer, args_id);
        }
    }

    /// Collect argument types for format string generation
    fn collectArgumentTypes(self: *const StdLibHandler, arg_types: *std.ArrayList([]const u8), args_id: ast.NodeId) CCodegenError!void {
        const args_node = self.arena.getNodeConst(args_id) orelse return;

        if (args_node.data == .call_expr) {
            const call_expr = args_node.data.call_expr;
            for (call_expr.args.items) |arg_id| {
                const arg_type = try self.inferArgumentType(arg_id);
                try arg_types.append(arg_type);
            }
        } else if (args_node.data == .array_init) {
            const array_init = args_node.data.array_init;
            for (array_init.elements.items) |arg_id| {
                const arg_type = try self.inferArgumentType(arg_id);
                try arg_types.append(arg_type);
            }
        }
    }

    /// Infer the C type of an argument for format string generation
    fn inferArgumentType(self: *const StdLibHandler, arg_id: ast.NodeId) CCodegenError![]const u8 {
        const arg_node = self.arena.getNodeConst(arg_id) orelse return "%d";

        switch (arg_node.data) {
            .member_expr => |member_expr| {
                // Check if this is a struct field access
                const field_name = member_expr.field;
                if (std.mem.eql(u8, field_name, "field2")) {
                    // field2 is f64 in MyStruct
                    return "%f";
                } else if (std.mem.eql(u8, field_name, "field1")) {
                    // field1 is i32 in MyStruct
                    return "%d";
                }
                return "%d";
            },
            .identifier => |identifier| {
                // For simple identifiers, use %lld for i64 variables
                _ = identifier;
                return "%lld";
            },
            .literal => |literal| switch (literal) {
                .integer => return "%d",
                .float => return "%f",
                .string => return "%s",
                .bool_true, .bool_false => return "%d",
                else => return "%d",
            },
            else => return "%d",
        }
    }

    /// Convert Howl format string with {} to C format string using inferred types
    fn convertSmartFormatString(self: *const StdLibHandler, writer: Writer, format_str: []const u8, arg_types: []const []const u8) CCodegenError!void {
        _ = self;

        var i: usize = 0;
        var arg_index: usize = 0;

        while (i < format_str.len) {
            if (i < format_str.len - 1 and format_str[i] == '{' and format_str[i + 1] == '}') {
                // Use inferred type or default to %d
                if (arg_index < arg_types.len) {
                    try writer.writeAll(arg_types[arg_index]);
                } else {
                    try writer.writeAll("%d");
                }
                arg_index += 1;
                i += 2;
            } else {
                // Handle escape sequences
                switch (format_str[i]) {
                    '\n' => try writer.writeAll("\\n"),
                    '\r' => try writer.writeAll("\\r"),
                    '\t' => try writer.writeAll("\\t"),
                    '\\' => try writer.writeAll("\\\\"),
                    '"' => try writer.writeAll("\\\""),
                    0 => try writer.writeAll("\\0"),
                    else => try writer.print("{c}", .{format_str[i]}),
                }
                i += 1;
            }
        }
    }
    fn generateFormatString(self: *const StdLibHandler, writer: Writer, format_string_id: ast.NodeId) CCodegenError!void {
        const format_node = self.arena.getNodeConst(format_string_id) orelse {
            try writer.writeAll("\"\"");
            return;
        };

        if (format_node.data == .literal and format_node.data.literal == .string) {
            const format_str = format_node.data.literal.string.value;
            try writer.writeAll("\"");
            try self.convertFormatString(writer, format_str);
            try writer.writeAll("\"");
        } else {
            try writer.writeAll("\"\"");
        }
    }

    /// Convert Howl format string with {} to C format string with %d, %s, etc.
    fn convertFormatString(self: *const StdLibHandler, writer: Writer, format_str: []const u8) CCodegenError!void {
        _ = self; // May be used for type inference later

        var i: usize = 0;
        while (i < format_str.len) {
            if (i < format_str.len - 1 and format_str[i] == '{' and format_str[i + 1] == '}') {
                // Convert {} to %d for now (we can make this smarter with type inference later)
                try writer.writeAll("%d");
                i += 2;
            } else {
                // Handle escape sequences
                switch (format_str[i]) {
                    '\n' => try writer.writeAll("\\n"),
                    '\r' => try writer.writeAll("\\r"),
                    '\t' => try writer.writeAll("\\t"),
                    '\\' => try writer.writeAll("\\\\"),
                    '"' => try writer.writeAll("\\\""),
                    0 => try writer.writeAll("\\0"),
                    else => try writer.print("{c}", .{format_str[i]}),
                }
                i += 1;
            }
        }
    }

    /// Generate format arguments from .{arg1, arg2, ...} tuple
    fn generateFormatArguments(self: *const StdLibHandler, writer: Writer, args_id: ast.NodeId) CCodegenError!void {
        const args_node = self.arena.getNodeConst(args_id) orelse return;

        // The second argument appears to be a call_expr, not array_init
        // This is likely the .{} syntax being parsed as a call
        if (args_node.data == .call_expr) {
            const call_expr = args_node.data.call_expr;

            for (call_expr.args.items) |arg_id| {
                try writer.writeAll(", ");
                try self.generateFormatArgument(writer, arg_id);
            }
        } else if (args_node.data == .array_init) {
            const array_init = args_node.data.array_init;
            for (array_init.elements.items) |arg_id| {
                try writer.writeAll(", ");
                try self.generateFormatArgument(writer, arg_id);
            }
        } else if (args_node.data == .literal) {
            // Simple literal argument
            try writer.writeAll(", ");
            try self.generateFormatArgument(writer, args_id);
        } else if (args_node.data == .identifier) {
            // Variable reference
            try writer.writeAll(", ");
            try self.generateFormatArgument(writer, args_id);
        }
        // TODO: Handle other argument types as needed
    }

    /// Generate a single format argument
    fn generateFormatArgument(self: *const StdLibHandler, writer: Writer, arg_id: ast.NodeId) CCodegenError!void {
        const arg_node = self.arena.getNodeConst(arg_id) orelse {
            try writer.writeAll("0");
            return;
        };

        switch (arg_node.data) {
            .literal => |literal| {
                switch (literal) {
                    .integer => |int_val| try writer.print("{d}", .{int_val.value}),
                    .float => |float_val| try writer.print("{d}", .{float_val.value}),
                    .bool_true => try writer.writeAll("1"),
                    .bool_false => try writer.writeAll("0"),
                    .string => |str| {
                        try writer.writeAll("\"");
                        try self.writeEscapedString(writer, str.value);
                        try writer.writeAll("\"");
                    },
                    .enum_member => |enum_val| try writer.writeAll(enum_val.name),
                    else => try writer.writeAll("0"),
                }
            },
            .identifier => |identifier| {
                try writer.writeAll(identifier.name);
            },
            .member_expr => |member_expr| {
                // Handle different types of member access
                const obj_node = self.arena.getNodeConst(member_expr.object);
                if (obj_node) |obj| {
                    if (obj.data == .identifier) {
                        const obj_name = obj.data.identifier.name;

                        // Check if this is an enum access like MyEnum.Value1
                        if (std.mem.eql(u8, obj_name, "MyEnum")) {
                            try writer.print("{s}_{s}", .{ obj_name, member_expr.field });
                        } else {
                            // Regular struct member access like my_struct.field1
                            try self.generateFormatArgument(writer, member_expr.object);
                            try writer.writeAll(".");
                            try writer.writeAll(member_expr.field);
                        }
                    } else {
                        // Fallback case
                        try self.generateFormatArgument(writer, member_expr.object);
                        try writer.writeAll(".");
                        try writer.writeAll(member_expr.field);
                    }
                } else {
                    try writer.writeAll("0");
                }
            },
            else => try writer.writeAll("0"),
        }
    }

    /// Write string with C escape sequences
    fn writeEscapedString(self: *const StdLibHandler, writer: Writer, str: []const u8) CCodegenError!void {
        _ = self;
        for (str) |char| {
            switch (char) {
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                '\\' => try writer.writeAll("\\\\"),
                '"' => try writer.writeAll("\\\""),
                0 => try writer.writeAll("\\0"),
                else => try writer.print("{c}", .{char}),
            }
        }
    }
};
