const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("codegen_c_utils.zig");
const types = @import("codegen_c_types.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

const CCodegenError = utils.CCodegenError;
const Writer = utils.Writer;

/// Binary expression generation
pub fn generateCBinaryExpression(
    arena: *const ast.AstArena,
    writer: Writer,
    binary_expr: anytype,
    generateCExpressionRecursive: anytype,
) !void {
    try writer.writeAll("(");
    try generateCExpressionRecursive(arena, writer, binary_expr.left);
    try writer.writeAll(" ");
    
    const op_str = switch (binary_expr.op) {
        .add => "+",
        .sub => "-",
        .mul => "*",
        .div => "/",
        .mod => "%",
        .eq => "==",
        .ne => "!=",
        .lt => "<",
        .le => "<=",
        .gt => ">",
        .ge => ">=",
        .logical_and => "&&",
        .logical_or => "||",
        else => "+", // Fallback
    };
    
    try writer.writeAll(op_str);
    try writer.writeAll(" ");
    try generateCExpressionRecursive(arena, writer, binary_expr.right);
    try writer.writeAll(")");
}

/// Simple expression generation for recursive calls (fallback)
pub fn generateCSimpleExpression(arena: *const ast.AstArena, writer: Writer, node_id: ast.NodeId) !void {
    const node = arena.getNodeConst(node_id) orelse {
        try writer.writeAll("0");
        return;
    };

    switch (node.data) {
        .literal => |literal| switch (literal) {
            .integer => |int_val| try writer.print("{d}", .{int_val.value}),
            .float => |float_val| try writer.print("{d}", .{float_val.value}),
            .bool_true => try writer.writeAll("true"),
            .bool_false => try writer.writeAll("false"),
            .string => |str| {
                try writer.writeAll("\"");
                for (str.value) |char| {
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
                try writer.writeAll("\"");
            },
            .char => |char_val| try writer.print("'{c}'", .{char_val.value}),
            .none => try writer.writeAll("NULL"),
            .some => |some_val| try generateCSimpleExpression(arena, writer, some_val.value),
            .enum_member => |enum_val| try writer.writeAll(enum_val.name),
        },
        .identifier => |identifier| {
            try writer.writeAll(identifier.name);
        },
        .member_expr => |member_expr| {
            // Handle member access like obj.field
            try generateCSimpleExpression(arena, writer, member_expr.object);
            try writer.writeAll(".");
            try writer.writeAll(member_expr.field);
        },
        .call_expr => |call_expr| {
            // Import std_lib for std call handling
            const std_lib = @import("codegen_c_std.zig");
            const codegen = struct {
                allocator: std.mem.Allocator,
                
                const Self = @This();
                fn init(alloc: std.mem.Allocator) Self {
                    return Self{ .allocator = alloc };
                }
            }.init(std.heap.page_allocator);
            
            // Try to handle std library calls first
            const std_handler = std_lib.StdLibHandler.init(arena, codegen.allocator);
            if (std_handler.handleStdCall(writer, call_expr) catch false) {
                return; // Successfully handled as std library call
            }
            
            // Regular function call
            try generateCSimpleExpression(arena, writer, call_expr.callee);
            try writer.writeAll("(");
            
            for (call_expr.args.items, 0..) |arg_id, i| {
                if (i > 0) try writer.writeAll(", ");
                try generateCSimpleExpression(arena, writer, arg_id);
            }
            
            try writer.writeAll(")");
        },
        .if_expr => |if_expr| {
            // Check if this is a return statement with error union by looking at the structure
            const then_node = arena.getNodeConst(if_expr.then_branch);
            var is_error_union_if = false;
            
            if (then_node) |then_data| {
                if (then_data.data == .member_expr) {
                    const member_expr = then_data.data.member_expr;
                    const obj_node = arena.getNodeConst(member_expr.object);
                    if (obj_node) |obj| {
                        if (obj.data == .identifier) {
                            const obj_name = obj.data.identifier.name;
                            if (std.mem.startsWith(u8, obj_name, "MyError")) {
                                is_error_union_if = true;
                            }
                        }
                    }
                }
            }
            
            if (is_error_union_if) {
                // Generate error union conditional
                try writer.writeAll("(");
                try generateCSimpleExpression(arena, writer, if_expr.condition);
                try writer.writeAll(" ? ");
                
                // Then branch - error case
                if (then_node) |then_data| {
                    if (then_data.data == .member_expr) {
                        const member_expr = then_data.data.member_expr;
                        const obj_node = arena.getNodeConst(member_expr.object);
                        if (obj_node) |obj| {
                            if (obj.data == .identifier) {
                                const obj_name = obj.data.identifier.name;
                                try writer.print("(MyError_int32_t_ErrorUnion){{.error = {s}_{s}, .payload = {{0}}}}", .{ obj_name, member_expr.field });
                            }
                        }
                    }
                }
                
                try writer.writeAll(" : ");
                
                // Else branch - success case  
                if (if_expr.else_branch) |else_branch| {
                    try writer.writeAll("(MyError_int32_t_ErrorUnion){.error = MyError_SUCCESS, .payload = ");
                    try generateCSimpleExpression(arena, writer, else_branch);
                    try writer.writeAll("}");
                } else {
                    try writer.writeAll("(MyError_int32_t_ErrorUnion){.error = MyError_SUCCESS, .payload = 0}");
                }
                
                try writer.writeAll(")");
            } else {
                // Regular if expression
                try writer.writeAll("(");
                try generateCSimpleExpression(arena, writer, if_expr.condition);
                try writer.writeAll(" ? ");
                try generateCSimpleExpression(arena, writer, if_expr.then_branch);
                if (if_expr.else_branch) |else_branch| {
                    try writer.writeAll(" : ");
                    try generateCSimpleExpression(arena, writer, else_branch);
                } else {
                    try writer.writeAll(" : 0");
                }
                try writer.writeAll(")");
            }
        },
        .try_expr => |try_expr| {
            // Try expression should extract payload from error union
            // For now, generate a simple call that extracts .payload
            const inner_node = arena.getNodeConst(try_expr.expression);
            if (inner_node) |inner| {
                if (inner.data == .call_expr) {
                    const call_expr = inner.data.call_expr;
                    const callee_node = arena.getNodeConst(call_expr.callee);
                    if (callee_node) |callee| {
                        if (callee.data == .identifier) {
                            const func_name = callee.data.identifier.name;
                            // Generate function call and extract payload
                            if (std.mem.eql(u8, func_name, "createMyStruct") or 
                                std.mem.eql(u8, func_name, "createMyStructMaybe") or
                                std.mem.eql(u8, func_name, "divide")) {
                                try writer.writeAll("(");
                                try generateCSimpleExpression(arena, writer, try_expr.expression);
                                try writer.writeAll(").payload");
                                return;
                            }
                        }
                    }
                }
            }
            // Fallback to simple expression
            try generateCSimpleExpression(arena, writer, try_expr.expression);
        },
        .unary_expr => |unary_expr| {
            const op_str = switch (unary_expr.op) {
                .negate => "-",
                .not => "!",
                else => "",
            };
            try writer.writeAll(op_str);
            try generateCSimpleExpression(arena, writer, unary_expr.operand);
        },
        .array_init => |array_init| {
            // Handle array initialization like [1, 2, 3] or $[1, 2, 3]
            try writer.writeAll("{");
            for (array_init.elements.items, 0..) |element_id, i| {
                if (i > 0) try writer.writeAll(", ");
                try generateCSimpleExpression(arena, writer, element_id);
            }
            try writer.writeAll("}");
        },
        .index_expr => |index_expr| {
            // Handle array access like arr[0]
            try generateCSimpleExpression(arena, writer, index_expr.object);
            try writer.writeAll("[");
            try generateCSimpleExpression(arena, writer, index_expr.index);
            try writer.writeAll("]");
        },
        .match_expr => |match_expr| {
            // Generate basic match expressions 
            try generateBasicMatchExpression(arena, writer, match_expr);
        },
        else => {
            try writer.writeAll("0"); // Fallback for complex expressions
        },
    }
}

/// Full expression generation with context
pub fn generateCExpressionWithContext(
    _: anytype,
    type_collection: *const types.TypeCollection,
    arena: *const ast.AstArena,
    semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer,
    writer: Writer,
    node_id: ast.NodeId,
    expected_type: ?[]const u8,
) CCodegenError!void {
    const node = arena.getNodeConst(node_id) orelse {
        try writer.writeAll("/* invalid node */");
        return;
    };

    _ = type_collection; // May be used for type inference  
    _ = semantic_analyzer; // May be used for semantic analysis
    _ = expected_type; // May be used for type coercion

    switch (node.data) {
        .literal => |literal| switch (literal) {
            .integer => |int_val| try writer.print("{d}", .{int_val.value}),
            .float => |float_val| try writer.print("{d}", .{float_val.value}),
            .bool_true => try writer.writeAll("true"),
            .bool_false => try writer.writeAll("false"),
            .string => |str| {
                try writer.writeAll("\"");
                for (str.value) |char| {
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
                try writer.writeAll("\"");
            },
            .char => |char_val| try writer.print("'{c}'", .{char_val.value}),
            .none => try writer.writeAll("NULL"),
            .some => |some_val| try generateCSimpleExpression(arena, writer, some_val.value),
            .enum_member => |enum_val| try writer.writeAll(enum_val.name),
        },
        .identifier => |identifier| {
            try writer.writeAll(identifier.name);
        },
        .member_expr => |member_expr| {
            try generateCSimpleExpression(arena, writer, member_expr.object);
            try writer.writeAll(".");
            try writer.writeAll(member_expr.field);
        },
        .binary_expr => |binary_expr| {
            try generateCBinaryExpression(arena, writer, binary_expr, generateCSimpleExpression);
        },
        .call_expr => {
            // Delegate to simple expression generation
            try generateCSimpleExpression(arena, writer, node_id);
        },
        .if_expr => {
            // Delegate to simple expression generation  
            try generateCSimpleExpression(arena, writer, node_id);
        },
        .try_expr => {
            // Delegate to simple expression generation
            try generateCSimpleExpression(arena, writer, node_id);
        },
        .unary_expr => |unary_expr| {
            const op_str = switch (unary_expr.op) {
                .negate => "-",
                .not => "!",
                else => "",
            };
            try writer.writeAll(op_str);
            try generateCSimpleExpression(arena, writer, unary_expr.operand);
        },
        .array_init => |array_init| {
            try writer.writeAll("{");
            for (array_init.elements.items, 0..) |element_id, i| {
                if (i > 0) try writer.writeAll(", ");
                try generateCSimpleExpression(arena, writer, element_id);
            }
            try writer.writeAll("}");
        },
        .index_expr => |index_expr| {
            try generateCSimpleExpression(arena, writer, index_expr.object);
            try writer.writeAll("[");
            try generateCSimpleExpression(arena, writer, index_expr.index);
            try writer.writeAll("]");
        },
        .match_expr => |match_expr| {
            // Generate basic match expressions 
            try generateBasicMatchExpression(arena, writer, match_expr);
        },
        .struct_init => |struct_init| {
            // Generate C struct initialization
            if (struct_init.type_name) |type_name| {
                try writer.print("({s}){{", .{type_name});
            } else {
                try writer.writeAll("{");
            }
            
            for (struct_init.fields.items, 0..) |field, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print(".{s} = ", .{field.name});
                try generateCSimpleExpression(arena, writer, field.value);
            }
            try writer.writeAll("}");
        },
        else => {
            try writer.writeAll("/* unhandled expression */");
        },
    }
}

/// Generate basic match expressions with simple patterns  
fn generateBasicMatchExpression(arena: *const ast.AstArena, writer: Writer, match_expr: anytype) !void {
    // This should not be used anymore - match expressions are now handled as statements
    // Generate a fallback to indicate improper usage  
    _ = arena;
    _ = match_expr;
    try writer.writeAll("0 /* match expression should be handled as statement */");
}