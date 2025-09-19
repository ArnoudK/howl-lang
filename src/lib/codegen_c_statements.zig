const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("codegen_c_utils.zig");
const types = @import("codegen_c_types.zig");
const expressions = @import("codegen_c_expressions.zig");

const CCodegenError = utils.CCodegenError;
const Writer = utils.Writer;

/// Generate main function body
pub fn generateMainBody(
    writer: Writer,
    node_id: ast.NodeId,
    indent_level: u32,
    codegen: anytype,
) !void {
    _ = indent_level;
    try writer.writeAll("int main(void) {\n");
    try codegen.generateCFromAST(writer, node_id, 1);
    try writer.writeAll("    return 0;\n");
    try writer.writeAll("}\n");
}

/// Write indentation helper
pub fn writeIndentation(writer: Writer, indent_level: u32) !void {
    var i: u32 = 0;
    while (i < indent_level * 4) : (i += 1) {
        try writer.writeAll(" ");
    }
}

/// Generate variable declaration
pub fn generateVarDecl(
    arena: *const ast.AstArena,
    writer: Writer,
    var_decl: anytype,
    indent_level: u32,
    codegen: anytype,
) CCodegenError!void {
    // Check if this is a struct declaration like MyStruct :: struct { ... }
    if (var_decl.initializer) |init_id| {
        const init_node = arena.getNodeConst(init_id);
        if (init_node) |init_n| {
            if (init_n.data == .struct_decl) {
                // This is a struct type declaration, not a variable declaration
                // Skip struct declarations as they are handled by the collection system
                return;
            }
        }
    }

    // Write indentation
    try writeIndentation(writer, indent_level);

    // Generate the variable type
    const var_type = if (var_decl.type_annotation) |type_node_id| blk: {
        const type_info = codegen.getNodeType(type_node_id);
        if (type_info) |t| {
            break :blk types.generateCType(undefined, t);
        } else {
            break :blk "int32_t";
        }
    } else if (var_decl.initializer) |init_id| blk: {
        // Infer type from initializer for := declarations
        const init_type_info = codegen.getNodeType(init_id);
        if (init_type_info) |t| {
            break :blk types.generateCType(undefined, t);
        } else {
            // Try to infer type from function call return types or try expressions
            const init_node = arena.getNodeConst(init_id);
            if (init_node) |init_n| {
                if (init_n.data == .try_expr) {
                    // Handle try expressions - use semantic analyzer to infer the payload type
                    const inner_expr = init_n.data.try_expr.expression;
                    const inferred_type = codegen.inferNodeCType(inner_expr);
                    break :blk inferred_type;
                } else if (init_n.data == .call_expr) {
                    // Use semantic analyzer to infer the return type of the call expression
                    const inferred_type = codegen.inferNodeCType(init_id);
                    break :blk inferred_type;
                } else if (init_n.data == .array_init) {
                    // Handle array initialization like [1, 2, 3] or $[1, 2, 3]
                    const array_init = init_n.data.array_init;
                    if (array_init.elements.items.len > 0) {
                        // Infer element type from first element
                        const first_elem_node = arena.getNodeConst(array_init.elements.items[0]);
                        if (first_elem_node) |elem| {
                            if (elem.data == .literal) {
                                switch (elem.data.literal) {
                                    .integer => break :blk "int32_t",
                                    .float => break :blk "howl_f64_t",
                                    .bool_true, .bool_false => break :blk "bool",
                                    else => break :blk "int32_t",
                                }
                            } else if (elem.data == .call_expr) {
                                // Check if this is a struct constructor call like MyStruct{...}
                                const call_expr = elem.data.call_expr;
                                const callee_node = arena.getNodeConst(call_expr.callee);
                                if (callee_node) |callee| {
                                    if (callee.data == .identifier) {
                                        const struct_name = callee.data.identifier.name;
                                        // For now, assume it's a struct type - TODO: verify it's actually a struct
                                        break :blk struct_name;
                                    }
                                }
                                break :blk "int32_t"; // Fallback
                            } else if (elem.data == .struct_init) {
                                // Array of structs - use generic struct type
                                // TODO: infer actual struct type from semantic analysis
                                break :blk "GenericStruct";
                            }
                        }
                    }
                    break :blk "int32_t"; // Default array type
                }
            }
            break :blk "int32_t"; // Default if inference fails
        }
    } else "int32_t"; // Default type when no annotation or initializer

    try writer.print("{s} {s}", .{ var_type, var_decl.name });

    // Check if this is an array declaration and handle GC vs stack arrays
    const init_node = arena.getNodeConst(var_decl.initializer orelse 0);
    var is_gc_array = false;
    if (init_node) |init_n| {
        if (init_n.data == .array_init) {
            const array_init = init_n.data.array_init;
            if (array_init.use_gc) {
                // GC array: add * to make it a pointer type
                try writer.writeAll("*");
                is_gc_array = true;
            } else {
                // Stack array: add []
                try writer.writeAll("[]");
            }
        }
    }

    // Generate initializer if present
    if (var_decl.initializer) |init_id| {
        try writer.writeAll(" = ");
        try codegen.generateCExpressionWithContext(writer, init_id, var_type);
    }

    try writer.writeAll(";\n");
}

/// Generate return statement
pub fn generateReturnStmt(
    arena: *const ast.AstArena,
    writer: Writer,
    return_stmt: anytype,
    indent_level: u32,
    current_function_return_type: ?[]const u8,
    codegen: anytype,
    extractOptionalNameFromErrorUnion: anytype,
) CCodegenError!void {
    // Write indentation
    try writeIndentation(writer, indent_level);

    try writer.writeAll("return");
    if (return_stmt.value) |value_id| {
        try writer.writeAll(" ");

        // Handle different return value types and wrap them in error unions if needed
        var handled = false;
        const value_node = arena.getNodeConst(value_id);
        if (value_node) |val_node| {
            switch (val_node.data) {
                .member_expr => |member_expr| {
                    // This handles MyError.InvalidValue -> MyError_InvalidValue
                    const obj_node = arena.getNodeConst(member_expr.object);
                    if (obj_node) |obj| {
                        if (obj.data == .identifier) {
                            const obj_name = obj.data.identifier.name;
                            // Check if this is an error set member like MyError.InvalidValue
                            if (std.mem.startsWith(u8, obj_name, "MyError")) {
                                // This is an error value, wrap it in error union
                                if (current_function_return_type) |ret_type| {
                                    if (std.mem.endsWith(u8, ret_type, "_ErrorUnion")) {
                                        try writer.print("({s}){{.error_code = {s}_{s}, .payload = 0}}", .{ ret_type, obj_name, member_expr.field });
                                        handled = true;
                                    }
                                }
                            }
                        }
                    }
                },
                .call_expr => {
                    // Handle function calls like MyStruct_init(a, b)
                    if (current_function_return_type) |ret_type| {
                        if (std.mem.endsWith(u8, ret_type, "_ErrorUnion")) {
                            // Wrap function call in success error union
                            try writer.print("({s}){{.error_code = MyError_SUCCESS, .payload = ", .{ret_type});
                            try codegen.generateCExpressionWithContext(writer, value_id, null);
                            try writer.writeAll("}");
                            handled = true;
                        }
                    }
                },
                .struct_init => {
                    // Handle struct initialization like MyStruct_init(a, b)
                    if (current_function_return_type) |ret_type| {
                        if (std.mem.endsWith(u8, ret_type, "_ErrorUnion")) {
                            // Check if this is an optional error union that needs Some() wrapping
                            if (std.mem.indexOf(u8, ret_type, "Optional_") != null) {
                                // This is an error union containing an optional, wrap the struct in Some()
                                const optional_name = extractOptionalNameFromErrorUnion(ret_type);
                                try writer.print("({s}){{.error_code = MyError_SUCCESS, .payload = {s}_some(", .{ ret_type, optional_name });
                                try codegen.generateCExpressionWithContext(writer, value_id, null);
                                try writer.writeAll(")}");
                            } else {
                                // Regular error union, wrap struct directly
                                try writer.print("({s}){{.error_code = MyError_SUCCESS, .payload = ", .{ret_type});
                                try codegen.generateCExpressionWithContext(writer, value_id, null);
                                try writer.writeAll("}");
                            }
                            handled = true;
                        }
                    }
                },
                .literal => |literal| {
                    if (literal == .none) {
                        // Handle None literal for optional error unions
                        if (current_function_return_type) |ret_type| {
                            if (std.mem.endsWith(u8, ret_type, "_ErrorUnion") and std.mem.indexOf(u8, ret_type, "Optional_") != null) {
                                // This is an error union containing an optional, return None wrapped in success
                                const optional_name = extractOptionalNameFromErrorUnion(ret_type);
                                try writer.print("({s}){{.error_code = MyError_SUCCESS, .payload = {s}_none()}}", .{ ret_type, optional_name });
                                handled = true;
                            }
                        }
                    }
                },
                else => {},
            }
        }

        // If we didn't handle it specially, use default generation
        if (!handled) {
            try codegen.generateCExpressionWithContext(writer, value_id, current_function_return_type);
        }
    } else {
        // Handle bare return statement
        if (current_function_return_type) |ret_type| {
            if (std.mem.eql(u8, ret_type, "int")) {
                // In main function, bare return becomes return 0
                try writer.writeAll(" 0");
            }
        }
    }
    try writer.writeAll(";\n");
}

/// Generate if expression as statement
pub fn generateIfExpr(
    writer: Writer,
    if_expr: anytype,
    indent_level: u32,
    codegen: anytype,
) CCodegenError!void {
    // Write indentation for if expressions used as statements
    try writeIndentation(writer, indent_level);

    try writer.writeAll("if (");
    try codegen.generateCExpressionWithContext(writer, if_expr.condition, null);
    try writer.writeAll(") {\n");
    try codegen.generateCFromAST(writer, if_expr.then_branch, indent_level + 1);

    if (if_expr.else_branch) |else_id| {
        try writeIndentation(writer, indent_level);
        try writer.writeAll("} else {\n");
        try codegen.generateCFromAST(writer, else_id, indent_level + 1);
    }

    try writeIndentation(writer, indent_level);
    try writer.writeAll("}\n");
}

/// Generate call expression as statement
pub fn generateCallStmt(
    writer: Writer,
    node_id: ast.NodeId,
    indent_level: u32,
    codegen: anytype,
) CCodegenError!void {
    // Handle function calls as statements
    try writeIndentation(writer, indent_level);
    // Generate the function call
    try codegen.generateCExpressionWithContext(writer, node_id, null);
    try writer.writeAll(";\n");
}

/// Generate match expression as statement
pub fn generateMatchStmt(
    writer: Writer,
    node_id: ast.NodeId,
    indent_level: u32,
    codegen: anytype,
) CCodegenError!void {
    const node = codegen.arena.getNodeConst(node_id) orelse return CCodegenError.InvalidNodeType;

    if (node.data != .match_expr) {
        return CCodegenError.InvalidNodeType;
    }

    const match_expr = node.data.match_expr;

    // Generate if-else chain for match statement
    for (match_expr.arms.items, 0..) |arm, i| {
        try writeIndentation(writer, indent_level);

        if (i > 0) {
            try writer.writeAll("else ");
        }

        switch (arm.pattern) {
            .comparison => |comp| {
                try writer.writeAll("if (");
                try codegen.generateCExpressionWithContext(writer, match_expr.expression, null);
                try writer.writeAll(" ");
                switch (comp.operator) {
                    .LessThan => try writer.writeAll("<"),
                    .LessThanEqual => try writer.writeAll("<="),
                    .GreaterThan => try writer.writeAll(">"),
                    .GreaterThanEqual => try writer.writeAll(">="),
                    .EqualEqual => try writer.writeAll("=="),
                    .NotEqual => try writer.writeAll("!="),
                    else => try writer.writeAll("=="), // Default to equality
                }
                try writer.writeAll(" ");
                try codegen.generateCExpressionWithContext(writer, comp.value, null);
                try writer.writeAll(") ");
            },
            .wildcard => {
                // Wildcard matches everything - generate an else clause
                if (i == 0) {
                    try writer.writeAll("if (1) "); // Always true
                }
                // For subsequent wildcard, it will be an 'else' due to the loop logic
            },
            .literal => |lit| {
                try writer.writeAll("if (");
                try codegen.generateCExpressionWithContext(writer, match_expr.expression, null);
                try writer.writeAll(" == ");
                try generateLiteralValue(writer, lit);
                try writer.writeAll(") ");
            },
            .identifier => |ident| {
                try writer.writeAll("if (");
                try codegen.generateCExpressionWithContext(writer, match_expr.expression, null);
                try writer.writeAll(" == ");
                try writer.writeAll(ident);
                try writer.writeAll(") ");
            },
            .enum_member => |member| {
                try writer.writeAll("if (");
                try codegen.generateCExpressionWithContext(writer, match_expr.expression, null);
                try writer.writeAll(" == ");
                try writer.writeAll(member);
                try writer.writeAll(") ");
            },
            else => {
                // For other patterns, just match directly (fallback)
                try writer.writeAll("if (1) ");
            },
        }

        try writer.writeAll("{\n");
        // Generate the body - should handle blocks and single statements
        try codegen.generateCFromAST(writer, arm.body, indent_level + 1);
        try writeIndentation(writer, indent_level);
        try writer.writeAll("}\n");
    }
}

// Helper function to generate literal values
fn generateLiteralValue(writer: Writer, literal: ast.Literal) CCodegenError!void {
    switch (literal) {
        .integer => |val| try writer.print("{}", .{val.value}),
        .float => |val| try writer.print("{d}", .{val.value}),
        .string => |val| try writer.print("\"{s}\"", .{val.value}),
        .bool_true => try writer.writeAll("true"),
        .bool_false => try writer.writeAll("false"),
        .char => |val| try writer.print("'{c}'", .{val.value}),
        .none => try writer.writeAll("0"), // or appropriate null representation
        else => try writer.writeAll("0"), // fallback
    }
}

/// Generate default unhandled statement
pub fn generateUnhandledStmt(writer: Writer, indent_level: u32) CCodegenError!void {
    // Handle all other node types with generic comment
    try writeIndentation(writer, indent_level);
    try writer.writeAll("/* unhandled node type */;\n");
}
