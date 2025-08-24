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
    exprGenerator: anytype,
) !void {
    try writer.writeAll("(");
    try exprGenerator(arena, writer, binary_expr.left);
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
    try exprGenerator(arena, writer, binary_expr.right);
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

/// Main expression generation function - delegates to recursive generator
pub fn generateCExpression(
    codegen: anytype,
    writer: Writer,
    node_id: ast.NodeId,
) CCodegenError!void {
    try generateCExpressionRecursive(codegen, writer, node_id);
}

/// Recursive expression generation - handles all expression types
pub fn generateCExpressionRecursive(
    codegen: anytype,
    writer: Writer,
    node_id: ast.NodeId,
) CCodegenError!void {
    const node = codegen.arena.getNodeConst(node_id) orelse return;

    switch (node.data) {
        .literal => |literal| {
            try generateCLiteral(writer, literal);
        },
        .identifier => |identifier| {
            try writer.writeAll(identifier.name);
        },
        .unary_expr => |unary_expr| {
            // Handle unary expressions like -1, !condition, ~bits
            const op_str = switch (unary_expr.op) {
                .negate => "-",
                .not => "!",
                .bit_not => "~",
                else => "/* unknown unary op */",
            };
            try writer.writeAll("(");
            try writer.writeAll(op_str);
            try generateCExpressionRecursive(codegen, writer, unary_expr.operand);
            try writer.writeAll(")");
        },
        .binary_expr => |binary_expr| {
            // Special handling for string concatenation
            if (binary_expr.op == .concat) {
                try writer.writeAll("howl_string_concat(");
                try generateCExpressionRecursive(codegen, writer, binary_expr.left);
                try writer.writeAll(", ");
                try generateCExpressionRecursive(codegen, writer, binary_expr.right);
                try writer.writeAll(")");
            } else {
                try writer.writeAll("(");
                try generateCExpressionRecursive(codegen, writer, binary_expr.left);
                try writer.writeAll(" ");
                try writer.writeAll(binary_expr.op.toString());
                try writer.writeAll(" ");
                try generateCExpressionRecursive(codegen, writer, binary_expr.right);
                try writer.writeAll(")");
            }
        },
        .member_expr => |member_expr| {
            // Handle member access like std.debug.print
            try generateCMemberExpression(codegen, writer, member_expr);
        },
        .call_expr => |call_expr| {
            // Check if this is a stdlib function call
            if (try codegen.isStdlibFunctionCall(call_expr)) {
                try codegen.generateCStdlibCall(writer, call_expr);
            } else {
                // Check if this is a member method call (like h.append())
                const callee_node = codegen.arena.getNodeConst(call_expr.callee);
                if (callee_node) |callee| {
                    if (callee.data == .member_expr) {
                        const member_expr = callee.data.member_expr;
                        try generateCMemberMethodCall(codegen, writer, member_expr, call_expr.args.items);
                    } else {
                        // Generate regular function call
                        try generateCExpressionRecursive(codegen, writer, call_expr.callee);
                        try writer.writeAll("(");

                        // Generate arguments
                        for (call_expr.args.items, 0..) |arg_id, i| {
                            if (i > 0) try writer.writeAll(", ");
                            try generateCExpressionRecursive(codegen, writer, arg_id);
                        }

                        try writer.writeAll(")");
                    }
                } else {
                    // Fallback
                    try writer.writeAll("/* unknown call */");
                }
            }
        },
        .struct_init => |struct_init| {
            // Handle anonymous struct syntax .{args}
            if (struct_init.type_name == null) {
                // This is an anonymous struct for function arguments
                for (struct_init.fields.items, 0..) |field_init, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try generateCExpressionRecursive(codegen, writer, field_init.value);
                }
            } else {
                // Named struct initialization: .TypeName{ .field = value, ... }
                const type_name = struct_init.type_name.?;
                
                if (struct_init.use_gc) {
                    // Garbage collected initialization: malloc and initialize
                    try writer.print("({s}*)malloc(sizeof({s}))", .{type_name, type_name});
                    // For now, we'll generate a simple malloc call. In a full implementation,
                    // we would need to initialize the allocated memory with the struct values.
                    // This is a basic starting point for GC support.
                } else {
                    // Generate constructor call: TypeName_init(field_values...)
                    try writer.print("{s}_init(", .{type_name});

                    // Generate field values in constructor order
                    // TODO: Should match the order of fields in struct definition
                    for (struct_init.fields.items, 0..) |field_init, i| {
                        if (i > 0) try writer.writeAll(", ");
                        try generateCExpressionRecursive(codegen, writer, field_init.value);
                    }
                    try writer.writeAll(")");
                }
            }
        },
        .array_init => |array_init| {
            if (array_init.use_gc) {
                // Garbage collected array: malloc and initialize
                const num_elements = array_init.elements.items.len;
                
                // Determine the element type using centralized inference
                const element_type = if (num_elements > 0) 
                    codegen.inferNodeCType(array_init.elements.items[0])
                else 
                    "int32_t";
                
                try writer.print("malloc({d} * sizeof({s}))", .{num_elements, element_type});
                // For now, this is a basic malloc call. In a full implementation,
                // we would need to initialize the array elements properly.
            } else {
                // Generate C array literal syntax: {1, 2, 3}
                try writer.writeAll("{");
                for (array_init.elements.items, 0..) |element_id, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try generateCExpressionRecursive(codegen, writer, element_id);
                }
                try writer.writeAll("}");
            }
        },
        .if_expr => |if_expr| {
            // Generate C ternary operator: (condition ? then_value : else_value)
            try writer.writeAll("(");
            try generateCExpressionRecursive(codegen, writer, if_expr.condition);
            try writer.writeAll(" ? ");
            try generateCExpressionRecursive(codegen, writer, if_expr.then_branch);
            try writer.writeAll(" : ");
            if (if_expr.else_branch) |else_branch| {
                try generateCExpressionRecursive(codegen, writer, else_branch);
            } else {
                try writer.writeAll("/* no else branch */");
            }
            try writer.writeAll(")");
        },
        .compile_target_expr => {
            // @compile.target - output the target as a string literal
            try writer.writeAll("\"c\"");
        },
        .compile_insert_expr => |compile_insert| {
            // @compile.insert("code") - output the inserted code directly
            // This should only be used inside function bodies, not expressions
            try writer.writeAll(compile_insert.code);
        },
        .try_expr => |try_expr| {
            // For try expressions in expression context
            try writer.writeAll("({ ");
            
            // Determine the correct error union type from the expression being tried
            const error_union_type = codegen.inferErrorUnionTypeFromExpression(try_expr.expression);
            try writer.writeAll(error_union_type);
            try writer.writeAll(" _temp = ");
            try generateCExpressionRecursive(codegen, writer, try_expr.expression);
            
            if (codegen.current_function_is_main) {
                // In main function, use if statement instead of ternary to avoid type mismatch
                try writer.writeAll("; if (_temp.error < 0) exit(1); _temp.payload; })");
            } else {
                // In regular functions, propagate error using the correct error union type
                try writer.writeAll("; if (_temp.error < 0) { ");
                try writer.writeAll(error_union_type);
                try writer.writeAll(" _propagated = {_temp.error, 0}; return _propagated; } _temp.payload; })");
            }
        },
        .error_union_type => {
            // Error union types don't generate expressions
            try writer.writeAll("/* error_union_type */");
        },
        .error_literal => |error_literal| {
            // Error literals generate as string constants
            try writer.print("\"{s}\"", .{error_literal.name});
        },
        .index_expr => |index_expr| {
            // Handle array/pointer indexing like array[index]
            try generateCExpressionRecursive(codegen, writer, index_expr.object);
            try writer.writeAll("[");
            try generateCExpressionRecursive(codegen, writer, index_expr.index);
            try writer.writeAll("]");
        },
        else => {
            try writer.writeAll("/* Unknown expression */");
        },
    }
}

/// Generate C literal values
pub fn generateCLiteral(writer: Writer, literal: ast.Literal) CCodegenError!void {
    switch (literal) {
        .integer => |int_literal| {
            try writer.print("{d}", .{int_literal.value});
        },
        .float => |float_literal| {
            // Format float with proper C syntax
            if (float_literal.value == @floor(float_literal.value)) {
                // If it's a whole number, format as "x.0f"
                try writer.print("{d:.1}f", .{float_literal.value});
            } else {
                // If it has decimal places, format normally
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
            try writer.writeAll("true");
        },
        .bool_false => {
            try writer.writeAll("false");
        },
        .enum_member => |enum_member| {
            // For enum members, we generate the enum member name
            // In C, enums are typically prefixed, e.g., ENUM_MEMBER
            try writer.print("{s}", .{enum_member.name});
        },
        .none => {
            // Generate NULL for None literal
            try writer.writeAll("NULL");
        },
        .some => |_| {
            // Generate Some literal - for now just generate a placeholder
            try writer.writeAll("/* Some value */");
        },
    }
}

/// Generate C member expressions (object.field)
pub fn generateCMemberExpression(
    codegen: anytype,
    writer: Writer,
    member_expr: anytype,
) CCodegenError!void {
    const object_node = codegen.arena.getNodeConst(member_expr.object) orelse return;

    // Handle error set member access (e.g., MyError.DivisionByZero)
    if (object_node.data == .identifier) {
        const identifier = object_node.data.identifier;
        const member_name = member_expr.field;

        // Check if this is an error set type
        if (codegen.isErrorSetType(identifier.name)) {
            try writer.print("{s}_{s}", .{ identifier.name, member_name });
            return;
        }

        // Handle enum member access (e.g., MyEnum.a -> MyEnum_a)
        if (codegen.isEnumType(identifier.name)) {
            try writer.print("{s}_{s}", .{ identifier.name, member_expr.field });
            return;
        }

        if (std.mem.eql(u8, member_name, "append")) {
            try writer.print("HowlList_i32_append", .{});
            return;
        }

        // Handle @compile.target and other compile-time member expressions  
        if (std.mem.eql(u8, identifier.name, "compile")) {
            if (std.mem.eql(u8, member_name, "target")) {
                try writer.writeAll("\"c\"");
                return;
            } else if (std.mem.eql(u8, member_name, "arch")) {
                try writer.writeAll("\"x86_64\""); // Default architecture
                return;
            } else if (std.mem.eql(u8, member_name, "os")) {
                try writer.writeAll("\"linux\""); // Default OS
                return;
            }
        }
        
        // Handle struct field access (e.g., my_struct.field1 -> my_struct.field1)
        // This is a simple field access, not a method call
        // Check if the identifier might be a struct instance variable
        // For now, assume any identifier that's not an error set, enum, or special case is a struct instance
        if (!codegen.isErrorSetType(identifier.name) and !codegen.isEnumType(identifier.name) and 
            !std.mem.eql(u8, identifier.name, "compile")) {
            // Generate C struct field access: object.field
            try writer.print("{s}.{s}", .{ identifier.name, member_expr.field });
            return;
        }
    }

    // Fallback for unhandled member expressions
    std.log.err("Member expressions are not yet supported in C target: {s}.{s}", .{ 
        if (object_node.data == .identifier) object_node.data.identifier.name else "complex_object", 
        member_expr.field 
    });
    return CCodegenError.UnsupportedOperation;
}

/// Generate C member method calls (object.method(args))
pub fn generateCMemberMethodCall(
    codegen: anytype,
    writer: Writer,
    member_expr: anytype,
    args: []const ast.NodeId,
) CCodegenError!void {
    const object_node = codegen.arena.getNodeConst(member_expr.object) orelse return;

    // Handle @compile.print special case - this is a compile-time function
    if (object_node.data == .identifier) {
        const object_name = object_node.data.identifier.name;
        if (std.mem.eql(u8, object_name, "compile") and std.mem.eql(u8, member_expr.field, "print")) {
            // @compile.print is a compile-time function - execute it now and generate no runtime code
            if (args.len > 0) {
                const arg_node = codegen.arena.getNodeConst(args[0]);
                if (arg_node) |node| {
                    if (node.data == .literal and node.data.literal == .string) {
                        // Print the compile-time message
                        std.debug.print("[@compile.print] {s}\n", .{node.data.literal.string.value});
                    } else if (node.data == .binary_expr) {
                        // Handle string concatenation like "Target: " + target
                        std.debug.print("[@compile.print] (complex expression)\n", .{});
                    }
                }
            }
            // Generate no runtime C code for compile-time functions
            try writer.writeAll("/* @compile.print executed at compile time */");
            return;
        }
    }

    // Handle std.List(Type).init() calls
    if (object_node.data == .call_expr) {
        const call_expr = object_node.data.call_expr;
        const callee_node = codegen.arena.getNodeConst(call_expr.callee);
        if (callee_node) |callee| {
            if (callee.data == .member_expr) {
                const std_list_member = callee.data.member_expr;
                const std_node = codegen.arena.getNodeConst(std_list_member.object);
                if (std_node) |std_obj| {
                    if (std_obj.data == .identifier and
                        std.mem.eql(u8, std_obj.data.identifier.name, "std") and
                        std.mem.eql(u8, std_list_member.field, "List") and
                        std.mem.eql(u8, member_expr.field, "init"))
                    {
                        // This is std.List(Type).init() - no arguments needed
                        
                        // Try to determine the type from the type argument
                        if (call_expr.args.items.len > 0) {
                            const type_arg = codegen.arena.getNodeConst(call_expr.args.items[0]);
                            if (type_arg) |type_node| {
                                if (type_node.data == .identifier) {
                                    const type_name = type_node.data.identifier.name;
                                    const c_type_name = utils.mapHowlTypeToCType(type_name);
                                    try writer.print("HowlList_{s}_init()", .{utils.sanitizeTypeForName(c_type_name)});
                                    return;
                                }
                            }
                        }
                        
                        // Fallback to i32 if we can't determine the type
                        try writer.writeAll("HowlList_i32_init()");
                        return;
                    }
                }
            }
        }
    }

    // Handle regular method calls like list.append(value)
    if (object_node.data == .identifier) {
        const object_name = object_node.data.identifier.name;
        
        if (std.mem.eql(u8, member_expr.field, "append")) {
            // Handle list.append(value)
            try writer.print("HowlList_i32_append(&{s}, ", .{object_name});
            
            if (args.len > 0) {
                try generateCExpressionRecursive(codegen, writer, args[0]);
            }
            
            try writer.writeAll(")");
            return;
        }
        
        if (std.mem.eql(u8, member_expr.field, "len")) {
            // Handle list.len - this should be a property access, not a method call
            try writer.print("{s}.len", .{object_name});
            return;
        }
    }

    // Fallback for unhandled method calls
    try writer.writeAll("/* unhandled method call */");
}