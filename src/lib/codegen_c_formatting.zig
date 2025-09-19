const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("codegen_c_utils.zig");

// Re-export common types
const CCodegenError = utils.CCodegenError;
const Writer = utils.Writer;

/// Generate C printf call from Howl debug.print
pub fn generateCPrintfCall(codegen: anytype, writer: Writer, call_expr: anytype) CCodegenError!void {
    // Check if we have arguments
    if (call_expr.args.items.len == 0) {
        try writer.writeAll("printf(\"\\n\")");
        return;
    }

    // Get the format string from the first argument
    const first_arg_node = codegen.arena.getNodeConst(call_expr.args.items[0]);
    if (first_arg_node) |arg_node| {
        if (arg_node.data == .literal and arg_node.data.literal == .string) {
            const format_string = arg_node.data.literal.string.value;

            // If we have more arguments, use them
            if (call_expr.args.items.len > 1) {
                const args = call_expr.args.items[1..];
                try generateCFormattedPrintfWithArgs(codegen, writer, format_string, args);
            } else {
                // No additional arguments, just print the format string
                const c_format_result = try convertHowlFormatToCWithCount(codegen, format_string);
                defer codegen.allocator.free(c_format_result.format);

                if (c_format_result.arg_count > 0) {
                    // Format string has placeholders but no args provided - this might be an error
                    std.log.warn("Format string has {} placeholders but no arguments provided", .{c_format_result.arg_count});
                }

                try writer.print("printf(\"{s}\")", .{c_format_result.format});
            }
        } else {
            // First argument is not a string literal - fall back to simple printf
            switch (arg_node.data) {
                .literal => |literal| {
                    const format_spec = mapLiteralToFormatSpec(literal);
                    try writer.print("printf(\"{s}\\n\", ", .{format_spec});
                    try generateCLiteralForPrintf(writer, literal);
                    try writer.writeAll(")");
                },
                else => {
                    try writer.writeAll("printf(\"complex expression\\n\")");
                },
            }
        }
    }
}

/// Generate formatted printf with field initializers
pub fn generateCFormattedPrintf(codegen: anytype, writer: Writer, format_string: []const u8, field_inits: []ast.FieldInit) CCodegenError!void {
    // Convert Howl format string to C format string
    const c_format_result = try convertHowlFormatToCWithCount(codegen, format_string);
    defer codegen.allocator.free(c_format_result.format);

    try writer.print("printf(\"{s}\"", .{c_format_result.format});

    // Add arguments based on field initializers
    for (field_inits) |field_init| {
        try writer.writeAll(", ");
        const value_node = codegen.arena.getNodeConst(field_init.value);
        if (value_node) |node| {
            switch (node.data) {
                .literal => |literal| {
                    try generateCLiteralForPrintf(writer, literal);
                },
                .identifier => |identifier| {
                    try writer.writeAll(identifier.name);
                },
                else => {
                    try writer.writeAll("/* complex field value */");
                },
            }
        }
    }

    try writer.writeAll(")");
}

/// Generate formatted printf with node arguments
pub fn generateCFormattedPrintfWithArgs(codegen: anytype, writer: Writer, format_string: []const u8, args: []ast.NodeId) CCodegenError!void {
    // Convert Howl format string to C format string and get expected argument count
    const c_format_result = try convertHowlFormatToCWithCount(codegen, format_string);
    defer codegen.allocator.free(c_format_result.format);

    try writer.print("printf(\"{s}\"", .{c_format_result.format});

    // Add arguments
    for (args) |arg_id| {
        try writer.writeAll(", ");
        const arg_node = codegen.arena.getNodeConst(arg_id);
        if (arg_node) |arg| {
            switch (arg.data) {
                .literal => |literal| {
                    try generateCLiteralForPrintf(writer, literal);
                },
                .identifier => |identifier| {
                    try writer.writeAll(identifier.name);
                },
                .member_expr => |member_expr| {
                    const object_node = codegen.arena.getNodeConst(member_expr.object);
                    if (object_node) |obj| {
                        if (obj.data == .identifier) {
                            try writer.print("{s}.{s}", .{ obj.data.identifier.name, member_expr.field });
                        } else {
                            try writer.writeAll("/* complex member access */");
                        }
                    }
                },
                .binary_expr => {
                    std.log.err("Binary expressions in printf arguments are not yet supported in C target", .{});
                    return error.UnsupportedExpression;
                },
                else => {
                    std.log.err("Complex expressions in printf arguments are not yet supported in C target: {}", .{arg.data});
                    return error.UnsupportedExpression;
                },
            }
        }
    }

    try writer.writeAll(")");
}

/// Infer format specifier from AST node
pub fn inferFormatSpecifier(codegen: anytype, arg_node: *const ast.AstNode) []const u8 {
    switch (arg_node.data) {
        .literal => |literal| return mapLiteralToFormatSpec(literal),
        .identifier => |identifier| {
            // Try to infer type from semantic analysis
            if (codegen.semantic_analyzer.variable_types.get(identifier.name)) |var_type| {
                return typeToFormatSpecifier(codegen, var_type);
            }
            return "%d"; // Default to int
        },
        .member_expr => |member_expr| {
            // Try to get the field type
            const object_node = codegen.arena.getNodeConst(member_expr.object);
            if (object_node) |obj_node| {
                if (obj_node.data == .identifier) {
                    const obj_name = obj_node.data.identifier.name;
                    if (codegen.semantic_analyzer.variable_types.get(obj_name)) |obj_type| {
                        if (obj_type.data == .custom) {
                            // Try to get field type from struct definition
                            if (codegen.getStructFieldType(obj_type, member_expr.field)) |field_type| {
                                return typeToFormatSpecifier(codegen, field_type);
                            }
                        }
                    }
                }
            }
            return "%d"; // Default
        },
        else => return "%s", // Default to string for unknown
    }
}

/// Convert AST type to C format specifier
pub fn typeToFormatSpecifier(codegen: anytype, type_info: ast.Type) []const u8 {
    _ = codegen; // May be needed for complex type resolution

    switch (type_info.data) {
        .primitive => |prim| {
            return switch (prim.kind) {
                .int8 => "%hhd",
                .uint8 => "%hhu",
                .int16 => "%hd",
                .uint16 => "%hu",
                .int32 => "%d",
                .uint32 => "%u",
                .int64 => "%ld",
                .uint64 => "%llu",
                .float32 => "%.6f",
                .float64 => "%.6f",
                .bool => "%d",
                .char => "%c",
                .string => "%s",
                else => "%d",
            };
        },
        .custom => |custom| {
            // For custom types, default to pointer format
            _ = custom;
            return "%p";
        },
        else => return "%d",
    }
}

/// Get struct field type by name
pub fn getStructFieldType(codegen: anytype, struct_type: ast.Type, field_name: []const u8) ?ast.Type {
    if (struct_type.data != .custom) return null;

    const type_name = struct_type.data.custom.name;
    if (codegen.semantic_analyzer.type_registry.get(type_name)) |registered_type| {
        if (registered_type.data == .@"struct") {
            const struct_info = registered_type.data.@"struct";
            for (struct_info.fields) |field| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    const type_node = codegen.arena.getNodeConst(field.type);
                    if (type_node) |type_node_val| {
                        if (type_node_val.data == .type_expr) {
                            return type_node_val.data.type_expr.resolved_type;
                        }
                    }
                    break;
                }
            }
        }
    }
    return null;
}

/// Convert Howl format string to C format with argument count
pub fn convertHowlFormatToCWithCount(codegen: anytype, howl_format: []const u8) !struct { format: []const u8, arg_count: usize } {
    var result = std.ArrayList(u8).init(codegen.allocator);
    defer result.deinit();

    var i: usize = 0;
    var arg_count: usize = 0;

    while (i < howl_format.len) {
        if (howl_format[i] == '{' and i + 1 < howl_format.len and howl_format[i + 1] == '}') {
            // Found {} placeholder - replace with %d (default)
            try result.appendSlice("%d");
            arg_count += 1;
            i += 2; // Skip both characters
        } else if (howl_format[i] == '\\' and i + 1 < howl_format.len and howl_format[i + 1] == 'n') {
            // Handle \n escape sequence
            try result.appendSlice("\\n");
            i += 2;
        } else {
            // Regular character
            try result.append(howl_format[i]);
            i += 1;
        }
    }

    return .{
        .format = try codegen.allocator.dupe(u8, result.items),
        .arg_count = arg_count,
    };
}

/// Convert Howl format string to C format string
pub fn convertHowlFormatToC(codegen: anytype, howl_format: []const u8) ![]const u8 {
    const result = try convertHowlFormatToCWithCount(codegen, howl_format);
    return result.format;
}

/// Generate format code for advanced printf
pub fn generateCFormatCode(codegen: anytype, format_string: []const u8, args_node_id: ast.NodeId) ![]const u8 {
    var result = std.ArrayList(u8).init(codegen.allocator);
    defer result.deinit();

    _ = args_node_id; // May be used for more complex formatting
    var arg_index: usize = 0;

    var i: usize = 0;
    while (i < format_string.len) {
        if (format_string[i] == '{') {
            // Look for closing }
            var j = i + 1;
            while (j < format_string.len and format_string[j] != '}') {
                j += 1;
            }

            if (j < format_string.len) {
                // Found format specifier
                const spec = format_string[i + 1 .. j];
                const c_spec = try convertFormatSpec(codegen, spec);
                try result.appendSlice(c_spec);
                arg_index += 1;
                i = j + 1; // Skip past }
            } else {
                // No closing }, treat as literal
                try result.append(format_string[i]);
                i += 1;
            }
        } else {
            try result.append(format_string[i]);
            i += 1;
        }
    }

    return try codegen.allocator.dupe(u8, result.items);
}

/// Convert advanced Howl format to C format with count
pub fn convertAdvancedHowlFormatToC(codegen: anytype, howl_format: []const u8) !struct { format: []const u8, arg_count: usize } {
    var result = std.ArrayList(u8).init(codegen.allocator);
    defer result.deinit();

    var i: usize = 0;
    var arg_count: usize = 0;

    while (i < howl_format.len) {
        if (howl_format[i] == '{') {
            // Look for closing }
            var j = i + 1;
            while (j < howl_format.len and howl_format[j] != '}') {
                j += 1;
            }

            if (j < howl_format.len) {
                // Found format specifier
                const spec = howl_format[i + 1 .. j];
                const c_spec = try convertFormatSpec(codegen, spec);
                try result.appendSlice(c_spec);
                arg_count += 1;
                i = j + 1;
            } else {
                try result.append(howl_format[i]);
                i += 1;
            }
        } else {
            try result.append(howl_format[i]);
            i += 1;
        }
    }

    return .{
        .format = try codegen.allocator.dupe(u8, result.items),
        .arg_count = arg_count,
    };
}

/// Convert format specifier from Howl to C
pub fn convertFormatSpec(codegen: anytype, spec: []const u8) ![]const u8 {
    _ = codegen;

    if (spec.len == 0) return "%d"; // Default

    // Handle common Howl format specs
    if (std.mem.eql(u8, spec, "d")) return "%d";
    if (std.mem.eql(u8, spec, "s")) return "%s";
    if (std.mem.eql(u8, spec, "f")) return "%.6f";
    if (std.mem.eql(u8, spec, "c")) return "%c";
    if (std.mem.eql(u8, spec, "x")) return "%x";
    if (std.mem.eql(u8, spec, "X")) return "%X";
    if (std.mem.eql(u8, spec, "o")) return "%o";
    if (std.mem.eql(u8, spec, "p")) return "%p";

    // Default fallback
    return "%d";
}

/// Map literal type to format specifier
fn mapLiteralToFormatSpec(literal: ast.Literal) []const u8 {
    return switch (literal) {
        .integer => "%d",
        .float => "%.6f",
        .string => "%s",
        .char => "%c",
        .bool_true, .bool_false => "%d",
        else => "%s",
    };
}

/// Generate C literal for printf context
fn generateCLiteralForPrintf(writer: Writer, literal: ast.Literal) !void {
    switch (literal) {
        .integer => |int_literal| {
            try writer.print("{d}", .{int_literal.value});
        },
        .float => |float_literal| {
            if (float_literal.value == @floor(float_literal.value)) {
                try writer.print("{d:.1}f", .{float_literal.value});
            } else {
                try writer.print("{d}f", .{float_literal.value});
            }
        },
        .string => |string_literal| {
            try writer.print("\"{s}\"", .{string_literal.value});
        },
        .char => |char_literal| {
            try writer.print("'{c}'", .{char_literal.value});
        },
        .bool_true => {
            try writer.writeAll("1");
        },
        .bool_false => {
            try writer.writeAll("0");
        },
        else => {
            try writer.writeAll("0");
        },
    }
}
