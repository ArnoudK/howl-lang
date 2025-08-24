const std = @import("std");
const ast = @import("ast.zig");

pub const CCodegenError = error{
    InvalidNodeType,
    UnhandledNodeType,
    MissingFieldInStruct,
    UnsupportedOperation,
    UnsupportedExpression,
    CompilationFailed,
} || std.ArrayList(u8).Writer.Error || std.mem.Allocator.Error || std.fs.OpenSelfExeError || std.fs.GetAppDataDirError ||
    std.fs.GetAppDataDirError || std.fs.Dir.OpenError || std.fs.Dir.MakeError || std.fs.Dir.WriteFileError;

pub const Writer = std.ArrayList(u8).Writer;

/// Maps a literal AST node to its corresponding C type string
pub fn mapLiteralToCType(literal: ast.Literal) []const u8 {
    return switch (literal) {
        .integer => "int32_t",
        .float => "howl_f32_t",
        .bool_true, .bool_false => "bool",
        .string => "char*",
        .char => "uint8_t",
        .enum_member => "int32_t", // Enums are represented as integers in C
        .none => "void", // None literal
        .some => "void", // Some literal (type determined by context)
    };
}

/// Maps a literal AST node to its corresponding C printf format specifier
pub fn mapLiteralToFormatSpec(literal: ast.Literal) []const u8 {
    return switch (literal) {
        .integer => "%d",
        .float => "%f",
        .bool_true, .bool_false => "%d", // bools as integers
        .string => "%s",
        .char => "%c",
        .enum_member => "%d", // Enums as integers
        .none => "%s", // None (placeholder)
        .some => "%s", // Some (placeholder)
    };
}

/// Maps Howl primitive types to C types
pub fn mapHowlTypeToCType(howl_type: []const u8) []const u8 {
    // Map Howl types to C types
    if (std.mem.eql(u8, howl_type, "i32")) return "int32_t";
    if (std.mem.eql(u8, howl_type, "i64")) return "int64_t";
    if (std.mem.eql(u8, howl_type, "u32")) return "uint32_t";
    if (std.mem.eql(u8, howl_type, "u64")) return "uint64_t";
    if (std.mem.eql(u8, howl_type, "f32")) return "howl_f32_t";
    if (std.mem.eql(u8, howl_type, "f64")) return "howl_f64_t";
    if (std.mem.eql(u8, howl_type, "bool")) return "bool";
    if (std.mem.eql(u8, howl_type, "str")) return "char*";
    if (std.mem.eql(u8, howl_type, "void")) return "void";

    // For custom types (structs, enums), use the type name as-is
    return howl_type;
}

/// Convert C type names to valid identifier parts for type names
pub fn sanitizeTypeForName(type_str: []const u8) []const u8 {
    // Convert C type names to valid identifier parts
    if (std.mem.eql(u8, type_str, "int32_t")) return "i32";
    if (std.mem.eql(u8, type_str, "int64_t")) return "i64";
    if (std.mem.eql(u8, type_str, "int16_t")) return "i16";
    if (std.mem.eql(u8, type_str, "int8_t")) return "i8";
    if (std.mem.eql(u8, type_str, "uint32_t")) return "u32";
    if (std.mem.eql(u8, type_str, "uint64_t")) return "u64";
    if (std.mem.eql(u8, type_str, "uint16_t")) return "u16";
    if (std.mem.eql(u8, type_str, "uint8_t")) return "u8";
    if (std.mem.eql(u8, type_str, "howl_f32_t")) return "f32";
    if (std.mem.eql(u8, type_str, "howl_f64_t")) return "f64";
    if (std.mem.eql(u8, type_str, "float")) return "f32";
    if (std.mem.eql(u8, type_str, "double")) return "f64";
    if (std.mem.eql(u8, type_str, "bool")) return "bool";
    if (std.mem.eql(u8, type_str, "void*")) return "ptr";
    if (std.mem.eql(u8, type_str, "const char*")) return "str";
    if (std.mem.eql(u8, type_str, "char*")) return "string";
    if (std.mem.eql(u8, type_str, "size_t")) return "usize";
    if (std.mem.eql(u8, type_str, "ptrdiff_t")) return "isize";
    if (std.mem.eql(u8, type_str, "void")) return "void";

    // For custom types, just use the type string directly
    return type_str;
}

/// Convert C type names to valid identifier parts for optional types
pub fn sanitizeTypeForOptionalName(type_str: []const u8) []const u8 {
    // Convert C type names to valid identifier parts for optional types

    // Integer types
    if (std.mem.eql(u8, type_str, "int8_t")) return "int8";
    if (std.mem.eql(u8, type_str, "int16_t")) return "int16";
    if (std.mem.eql(u8, type_str, "int32_t")) return "int32";
    if (std.mem.eql(u8, type_str, "int64_t")) return "int64";

    // Unsigned integer types
    if (std.mem.eql(u8, type_str, "uint8_t")) return "uint8";
    if (std.mem.eql(u8, type_str, "uint16_t")) return "uint16";
    if (std.mem.eql(u8, type_str, "uint32_t")) return "uint32";
    if (std.mem.eql(u8, type_str, "uint64_t")) return "uint64";

    // Size types
    if (std.mem.eql(u8, type_str, "size_t")) return "usize";
    if (std.mem.eql(u8, type_str, "ptrdiff_t")) return "isize";

    // Floating point types
    if (std.mem.eql(u8, type_str, "howl_f32_t")) return "f32";
    if (std.mem.eql(u8, type_str, "howl_f64_t")) return "f64";

    // Other built-in types
    if (std.mem.eql(u8, type_str, "bool")) return "bool";
    if (std.mem.eql(u8, type_str, "char")) return "char";
    if (std.mem.eql(u8, type_str, "const char*")) return "str";
    if (std.mem.eql(u8, type_str, "char*")) return "str";
    if (std.mem.eql(u8, type_str, "void")) return "void";

    // For custom types (structs, enums), just use the type string directly
    return type_str;
}

pub fn convertHowlFormatToCWithCount(allocator: std.mem.Allocator, howl_format: []const u8) !struct { format: []const u8, arg_count: usize } {
    // Convert Howl format string to C format string and count expected arguments
    // Supports: {} -> %d (default), {d} -> %d, {s} -> %s, {f} -> %f
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    var arg_count: usize = 0;

    var i: usize = 0;
    while (i < howl_format.len) {
        if (howl_format[i] == '\n') {
            // Convert actual newline character to \n escape sequence
            try result.appendSlice("\\n");
            i += 1;
        } else if (howl_format[i] == '\t') {
            // Convert actual tab character to \t escape sequence
            try result.appendSlice("\\t");
            i += 1;
        } else if (howl_format[i] == '\r') {
            // Convert actual carriage return to \r escape sequence
            try result.appendSlice("\\r");
            i += 1;
        } else if (howl_format[i] == '"') {
            // Escape double quotes
            try result.appendSlice("\\\"");
            i += 1;
        } else if (howl_format[i] == '\\' and i + 1 < howl_format.len) {
            // Handle escape sequences
            const next_char = howl_format[i + 1];
            switch (next_char) {
                'n' => {
                    try result.appendSlice("\\n");
                    i += 2;
                },
                't' => {
                    try result.appendSlice("\\t");
                    i += 2;
                },
                'r' => {
                    try result.appendSlice("\\r");
                    i += 2;
                },
                '\\' => {
                    try result.appendSlice("\\\\");
                    i += 2;
                },
                '"' => {
                    try result.appendSlice("\\\"");
                    i += 2;
                },
                else => {
                    try result.append(howl_format[i]);
                    i += 1;
                },
            }
        } else if (howl_format[i] == '{' and i + 1 < howl_format.len) {
            const next_char = howl_format[i + 1];
            switch (next_char) {
                'd' => {
                    try result.appendSlice("%d");
                    arg_count += 1;
                    i += 2; // Skip '{d'
                    // Skip until '}'
                    while (i < howl_format.len and howl_format[i] != '}') {
                        i += 1;
                    }
                    i += 1; // Skip '}'
                },
                's' => {
                    try result.appendSlice("%s");
                    arg_count += 1;
                    i += 2; // Skip '{s'
                    // Skip until '}'
                    while (i < howl_format.len and howl_format[i] != '}') {
                        i += 1;
                    }
                    i += 1; // Skip '}'
                },
                'f' => {
                    try result.appendSlice("%f");
                    arg_count += 1;
                    i += 2; // Skip '{f'
                    // Skip until '}'
                    while (i < howl_format.len and howl_format[i] != '}') {
                        i += 1;
                    }
                    i += 1; // Skip '}'
                },
                '}' => {
                    // Empty {} - determine format based on context
                    // For now, use heuristic: if we're likely dealing with float variables use %f
                    try result.appendSlice("%d"); // Default to int, could be enhanced with type inference
                    arg_count += 1;
                    i += 2;
                },
                else => {
                    // If we find a '{' followed by something else, check if it's the end of a generic {}
                    if (next_char == '}') {
                        try result.appendSlice("%d");
                        arg_count += 1;
                        i += 2;
                    } else {
                        // If it's not a recognized format specifier, just copy the '{'
                        try result.append(howl_format[i]);
                        i += 1;
                    }
                },
            }
        } else {
            try result.append(howl_format[i]);
            i += 1;
        }
    }

    return .{ .format = try allocator.dupe(u8, result.items), .arg_count = arg_count };
}

pub fn convertHowlFormatToC(allocator: std.mem.Allocator, howl_format: []const u8) ![]const u8 {
    const result = try convertHowlFormatToCWithCount(allocator, howl_format);
    return result.format;
}

pub fn convertAdvancedHowlFormatToC(allocator: std.mem.Allocator, howl_format: []const u8) !struct { format: []const u8, arg_count: usize } {
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    var arg_count: usize = 0;
    var i: usize = 0;

    while (i < howl_format.len) {
        if (howl_format[i] == '{' and i + 1 < howl_format.len) {
            // Parse format specifier: {type:format_spec}
            var end_pos = i + 1;
            while (end_pos < howl_format.len and howl_format[end_pos] != '}') {
                end_pos += 1;
            }

            if (end_pos < howl_format.len) {
                const format_spec = howl_format[i + 1 .. end_pos];

                // Parse the format specification
                const converted_spec = try convertFormatSpec(format_spec);
                try result.appendSlice(converted_spec);
                arg_count += 1;

                i = end_pos + 1; // Skip past '}'
            } else {
                // Malformed format spec, just copy the '{'
                try result.append(howl_format[i]);
                i += 1;
            }
        } else if (howl_format[i] == '\\' and i + 1 < howl_format.len) {
            // Handle escape sequences
            const next_char = howl_format[i + 1];
            switch (next_char) {
                'n' => try result.appendSlice("\\n"),
                't' => try result.appendSlice("\\t"),
                'r' => try result.appendSlice("\\r"),
                '\\' => try result.appendSlice("\\\\"),
                '"' => try result.appendSlice("\\\""),
                else => {
                    try result.append(howl_format[i]);
                    try result.append(next_char);
                },
            }
            i += 2;
        } else if (howl_format[i] == '"') {
            // Escape quotes in format string
            try result.appendSlice("\\\"");
            i += 1;
        } else {
            try result.append(howl_format[i]);
            i += 1;
        }
    }

    return .{
        .format = try allocator.dupe(u8, result.items),
        .arg_count = arg_count,
    };
}

// Convert individual format specification (e.g., "d:.2", "s:!>4")
fn convertFormatSpec(spec: []const u8) ![]const u8 {
    if (spec.len == 0) {
        return "%d"; // Default format
    }

    // Split on ':' if present
    var type_part = spec;
    var format_part: []const u8 = "";

    if (std.mem.indexOf(u8, spec, ":")) |colon_pos| {
        type_part = spec[0..colon_pos];
        format_part = spec[colon_pos + 1 ..];
    }

    // Handle type specifiers
    if (std.mem.eql(u8, type_part, "d")) {
        // Integer format
        if (format_part.len == 0) {
            return "%d";
        } else if (std.mem.startsWith(u8, format_part, ".")) {
            // Precision for decimal display
            return "%.0f"; // For now, treat as float with 0 decimal places
        } else {
            return "%d";
        }
    } else if (std.mem.eql(u8, type_part, "s")) {
        // String format
        if (format_part.len == 0) {
            return "%s";
        } else {
            // For now, ignore complex string formatting
            return "%s";
        }
    } else if (std.mem.eql(u8, type_part, "f")) {
        // Float format
        if (format_part.len == 0) {
            return "%f";
        } else if (std.mem.startsWith(u8, format_part, ".")) {
            // Extract precision
            const precision_str = format_part[1..];
            if (std.mem.eql(u8, precision_str, "2")) {
                return "%.2f";
            } else if (std.mem.eql(u8, precision_str, "1")) {
                return "%.1f";
            } else if (std.mem.eql(u8, precision_str, "3")) {
                return "%.3f";
            } else {
                return "%.2f"; // Default precision
            }
        } else {
            return "%f";
        }
    } else {
        // Default case
        return "%d";
    }
}

pub fn extractOptionalNameFromErrorUnion(error_union_name: []const u8) []const u8 {
    // Extract Optional_X_t from MyError_Optional_X_t_ErrorUnion
    if (std.mem.indexOf(u8, error_union_name, "Optional_")) |start| {
        const optional_part = error_union_name[start..];
        if (std.mem.indexOf(u8, optional_part, "_ErrorUnion")) |end| {
            return optional_part[0..end];
        }
    }
    return "Optional_unknown_t"; // Fallback
}

pub fn generateErrorUnionTypeName(allocator: std.mem.Allocator, error_set_name: []const u8, payload_type: []const u8) ![]const u8 {
    // Generate error union type name: MyError_PayloadType_ErrorUnion
    return try std.fmt.allocPrint(allocator, "{s}_{s}_ErrorUnion", .{ error_set_name, payload_type });
}