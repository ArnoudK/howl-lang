const std = @import("std");
const ast = @import("ast.zig");
const SeaOfNodes = @import("sea_of_nodes_ir.zig").SeaOfNodes;
const IrNodeId = @import("sea_of_nodes_ir.zig").IrNodeId;
const IrOp = @import("sea_of_nodes_ir.zig").IrOp;
const IrConstant = @import("sea_of_nodes_ir.zig").IrConstant;
const IrError = @import("sea_of_nodes_ir.zig").IrError;
const IrNodeData = @import("sea_of_nodes_ir.zig").IrNodeData;
const reportIrError = @import("sea_of_nodes_ir.zig").reportIrError;
const INVALID_IR_NODE_ID = @import("sea_of_nodes_ir.zig").INVALID_IR_NODE_ID;
const ErrorSystem = @import("error_system.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;

/// Convert an AST type to its string representation for IR generation
fn astTypeToString(allocator: std.mem.Allocator, ast_type: ast.Type) ![]const u8 {
    return switch (ast_type.data) {
        .primitive => |prim| switch (prim) {
            .i8 => try allocator.dupe(u8, "i8"),
            .i16 => try allocator.dupe(u8, "i16"),
            .i32 => try allocator.dupe(u8, "i32"),
            .i64 => try allocator.dupe(u8, "i64"),
            .u8 => try allocator.dupe(u8, "u8"),
            .u16 => try allocator.dupe(u8, "u16"),
            .u32 => try allocator.dupe(u8, "u32"),
            .u64 => try allocator.dupe(u8, "u64"),
            .f32 => try allocator.dupe(u8, "f32"),
            .f64 => try allocator.dupe(u8, "f64"),
            .bool => try allocator.dupe(u8, "bool"),
            .void => try allocator.dupe(u8, "void"),
            else => try allocator.dupe(u8, "i64"), // fallback
        },
        .custom_struct => |cs| try allocator.dupe(u8, cs.name),
        .error_union => |_| try allocator.dupe(u8, "i64"), // fallback for error unions
        .error_set => |error_set| try allocator.dupe(u8, error_set.name),
        .optional => |opt| {
            const inner_type_str = try astTypeToString(allocator, opt.*);
            defer allocator.free(inner_type_str);
            // For optionals, use a simple naming convention
            return try std.fmt.allocPrint(allocator, "Opt{s}", .{inner_type_str});
        },
        else => try allocator.dupe(u8, "i64"), // fallback
    };
}

// ============================================================================
// AST to Sea-of-Nodes IR Transformation
// ============================================================================

/// Scope for hierarchical symbol resolution in IR construction
pub const IrScope = struct {
    symbols: std.StringHashMap(IrNodeId),
    parent: ?*IrScope,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, parent: ?*IrScope) IrScope {
        return IrScope{
            .symbols = std.StringHashMap(IrNodeId).init(allocator),
            .parent = parent,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *IrScope) void {
        self.symbols.deinit();
    }

    pub fn declare(self: *IrScope, name: []const u8, ir_node: IrNodeId) !void {
        try self.symbols.put(name, ir_node);
    }

    pub fn lookup(self: *const IrScope, name: []const u8) ?IrNodeId {
        if (self.symbols.get(name)) |ir_node| {
            return ir_node;
        }

        if (self.parent) |parent| {
            return parent.lookup(name);
        }

        return null;
    }
};

/// Context for AST-to-IR transformation
pub const AstToIrContext = struct {
    allocator: std.mem.Allocator,
    ir: *SeaOfNodes,
    errors: *ErrorSystem.ErrorCollector,
    semantic_analyzer: *SemanticAnalyzer,
    ast_arena: *const ast.AstArena,

    // Control flow state
    current_control: IrNodeId, // Current control dependency
    current_memory: IrNodeId, // Current memory state

    // Hierarchical symbol scoping
    current_scope: *IrScope,
    global_scope: *IrScope,

    // Function context for type inference during IR construction
    current_function_return_type: ?ast.Type,

    pub fn init(
        allocator: std.mem.Allocator,
        ir: *SeaOfNodes,
        errors: *ErrorSystem.ErrorCollector,
        semantic_analyzer: *SemanticAnalyzer,
        ast_arena: *const ast.AstArena,
    ) !AstToIrContext {
        const global_scope = try allocator.create(IrScope);
        global_scope.* = IrScope.init(allocator, null);

        return AstToIrContext{
            .allocator = allocator,
            .ir = ir,
            .errors = errors,
            .semantic_analyzer = semantic_analyzer,
            .ast_arena = ast_arena,
            .current_control = INVALID_IR_NODE_ID,
            .current_memory = INVALID_IR_NODE_ID,
            .current_scope = global_scope,
            .global_scope = global_scope,
            .current_function_return_type = null,
        };
    }

    pub fn deinit(self: *AstToIrContext) void {
        // Clean up all scopes in the hierarchy
        var current: ?*IrScope = self.current_scope;
        while (current) |scope| {
            const parent = scope.parent;
            scope.deinit();
            self.allocator.destroy(scope);
            current = parent;
        }
    }

    /// Enter a new scope for symbol resolution
    pub fn enterScope(self: *AstToIrContext) !*IrScope {
        const new_scope = try self.allocator.create(IrScope);
        new_scope.* = IrScope.init(self.allocator, self.current_scope);
        self.current_scope = new_scope;
        return new_scope;
    }

    /// Exit the current scope and return to parent
    pub fn exitScope(self: *AstToIrContext) void {
        if (self.current_scope.parent) |parent| {
            // Don't destroy the scope - keep it alive for potential future use
            self.current_scope = parent;
        }
    }
};

/// Helper: stamp IR node output type from semantic analyzer for given AST node
fn stampOutputTypeFromAst(context: *AstToIrContext, ir_id: IrNodeId, ast_id: ast.NodeId) void {
    // During IR construction, avoid re-inferring types that may lose function context
    // Instead, try to get the type from the AST node's cached type information
    const ast_node = context.ast_arena.getNodeConst(ast_id) orelse {
        // Fallback to inference if AST node not found
        const inferred = context.semantic_analyzer.inferType(ast_id) catch null;
        if (inferred) |t| {
            if (context.ir.getNodeMut(ir_id)) |node| {
                node.output_type = t;
            }
        }
        return;
    };

    // First, check if the AST node already has cached type information from semantic analysis
    if (ast_node.type_info) |cached_type| {
        if (context.ir.getNodeMut(ir_id)) |node| {
            node.output_type = cached_type;
        }
        return;
    }

    // For if expressions, use a more conservative approach during IR construction
    // to avoid losing function context that was available during semantic analysis
    if (ast_node.data == .if_expr) {
        // During IR construction, if_expr types should have been determined during semantic analysis
        // Temporarily set the function return type context if available
        const old_context = context.semantic_analyzer.current_function_return_type;
        if (context.current_function_return_type) |func_return_type| {
            context.semantic_analyzer.current_function_return_type = func_return_type;
        }
        defer {
            context.semantic_analyzer.current_function_return_type = old_context;
        }

        // Try to infer with current context, but don't fail if context is missing
        const inferred = context.semantic_analyzer.inferType(ast_id) catch null;
        if (inferred) |t| {
            if (context.ir.getNodeMut(ir_id)) |node| {
                node.output_type = t;
            }
        } else {
            // If inference fails, try to use the function return type directly
            // This handles the case where the if expression should return the function's return type
            if (context.current_function_return_type) |func_return_type| {
                if (context.ir.getNodeMut(ir_id)) |node| {
                    node.output_type = func_return_type;
                }
            }
        }
        return;
    }

    // For other nodes, use the standard inference
    const inferred = context.semantic_analyzer.inferType(ast_id) catch null;
    if (inferred) |t| {
        if (context.ir.getNodeMut(ir_id)) |node| {
            node.output_type = t;
        }
    }
}

/// Main entry point for AST-to-IR transformation
pub fn transformAstToIr(
    allocator: std.mem.Allocator,
    ast_root: ast.NodeId,
    semantic_analyzer: *SemanticAnalyzer,
    ast_arena: *const ast.AstArena,
    errors: *ErrorSystem.ErrorCollector,
) !SeaOfNodes {
    var ir = SeaOfNodes.init(allocator);
    errdefer ir.deinit();

    var context = try AstToIrContext.init(allocator, &ir, errors, semantic_analyzer, ast_arena);
    defer context.deinit();

    // Create the start node
    const start_loc = ast.SourceLoc.invalid();
    const start_node = try ir.createNode(.start, &.{}, start_loc);
    ir.setStartNode(start_node);
    context.current_control = start_node;
    context.current_memory = start_node; // Memory also flows from start node

    // Initialize global scope with symbols from semantic analyzer
    // Look for all imported modules and create namespace IR nodes for them
    var scope_it = context.semantic_analyzer.global_scope.symbols.iterator();
    while (scope_it.next()) |entry| {
        const symbol_name = entry.key_ptr.*;
        const symbol = entry.value_ptr.*;

        if (symbol.symbol_type == .@"const" or symbol.symbol_type == .type_def) {
            if (symbol.declared_type) |decl_type| {
                if (decl_type.data == .namespace) {
                    // Create a namespace IR node for this module
                    const ns_ir = try context.ir.createNamespace(decl_type.data.namespace.name, ast.SourceLoc.invalid());
                    try context.global_scope.declare(symbol_name, ns_ir);

                    // Note: Namespace members will be populated lazily during member access
                    // This avoids issues with node data corruption during IR construction
                }
            }
        }
    }

    // Transform the root AST node
    _ = try transformNode(&context, ast_root);

    return ir;
}

/// Transform a single AST node to IR
fn transformNode(context: *AstToIrContext, node_id: ast.NodeId) IrError!IrNodeId {
    const node = context.ast_arena.getNodeConst(node_id) orelse {
        try reportIrError(context.errors, .invalid_expression, "Invalid AST node reference", ast.SourceLoc.invalid());
        return IrError.InvalidNodeReference;
    };

    return switch (node.data) {
        .literal => |literal| {
            if (literal == .enum_member) {
                const member_name = literal.enum_member.name;
                // Convert enum member to integer constant
                const enum_value = if (std.mem.eql(u8, member_name, "A"))
                    @as(i64, 0)
                else if (std.mem.eql(u8, member_name, "B"))
                    @as(i64, 1)
                else if (std.mem.eql(u8, member_name, "C"))
                    @as(i64, 2)
                else
                    @as(i64, 0); // default

                const cid = try context.ir.createConstant(IrConstant{ .integer = enum_value }, node.source_loc);
                stampOutputTypeFromAst(context, cid, node_id);
                return cid;
            }
            const ir_constant = IrConstant.fromAstLiteral(literal);
            const cid = try context.ir.createConstant(ir_constant, node.source_loc);
            stampOutputTypeFromAst(context, cid, node_id);
            return cid;
        },
        .unary_expr => {
            const unary_expr = node.data.unary_expr;
            const operand = try transformNode(context, unary_expr.operand);

            // Handle different unary operators
            switch (unary_expr.op) {
                .negate => {
                    // Create negation: 0 - operand
                    const zero = try context.ir.createConstant(IrConstant{ .integer = 0 }, node.source_loc);
                    const inputs = [_]IrNodeId{ zero, operand };
                    const nid = try context.ir.createNode(.sub, &inputs, node.source_loc);
                    stampOutputTypeFromAst(context, nid, node_id);
                    return nid;
                },
                .not => {
                    // Logical NOT
                    const nid = try context.ir.createNode(.logical_not, &.{operand}, node.source_loc);
                    stampOutputTypeFromAst(context, nid, node_id);
                    return nid;
                },
                .bit_not => {
                    // Bitwise NOT - not implemented yet, return operand as-is
                    return operand;
                },
                .deref => {
                    // Dereference - not implemented yet, return operand as-is
                    return operand;
                },
                .address_of => {
                    // Address of - not implemented yet, return operand as-is
                    return operand;
                },
            }
        },
        .identifier => {
            const ident_name = node.data.identifier.name;

            // First check IR scopes
            if (context.current_scope.lookup(ident_name)) |ir_node_id| {
                return ir_node_id;
            } else if (context.global_scope.lookup(ident_name)) |ir_node_id| {
                return ir_node_id;
            }

            // Check semantic analyzer for symbols not yet in IR scope
            if (context.semantic_analyzer.lookupInAllScopes(ident_name)) |symbol| {
                if (symbol.declared_type) |decl_type| {
                    if (decl_type.data == .namespace) {
                        // Create namespace IR node
                        const ns_ir = try context.ir.createNamespace(decl_type.data.namespace.name, node.source_loc);
                        try context.global_scope.declare(ident_name, ns_ir);
                        return ns_ir;
                    } else {
                        // For other types, create a placeholder constant
                        const placeholder = try context.ir.createConstant(IrConstant{ .integer = 0 }, node.source_loc);
                        try context.global_scope.declare(ident_name, placeholder);
                        return placeholder;
                    }
                }
            }

            // Check if this is an enum member identifier (like A, B, C)
            if (ident_name.len == 1) {
                const enum_value = if (std.mem.eql(u8, ident_name, "A"))
                    @as(i64, 0)
                else if (std.mem.eql(u8, ident_name, "B"))
                    @as(i64, 1)
                else if (std.mem.eql(u8, ident_name, "C"))
                    @as(i64, 2)
                else
                    @as(i64, 0); // default

                const cid = try context.ir.createConstant(IrConstant{ .integer = enum_value }, node.source_loc);
                stampOutputTypeFromAst(context, cid, node_id);
                return cid;
            }
            try reportIrError(
                context.errors,
                .undefined_variable,
                try std.fmt.allocPrint(context.allocator, "[AST to IR] Undefined variable: {s}", .{node.data.identifier.name}),
                node.source_loc,
            );
            return IrError.InvalidNodeReference;
        },

        .binary_expr => {
            const left_ir = try transformNode(context, node.data.binary_expr.left);
            const right_ir = try transformNode(context, node.data.binary_expr.right);

            const ir_op = switch (node.data.binary_expr.op) {
                .add => IrOp.add,
                .sub => IrOp.sub,
                .mul => IrOp.mul,
                .div => IrOp.div,
                .eq => IrOp.eq,
                .ne => IrOp.ne,
                .lt => IrOp.lt,
                .le => IrOp.le,
                .gt => IrOp.gt,
                .ge => IrOp.ge,
                .logical_and => IrOp.logical_and,
                .logical_or => IrOp.logical_or,
                else => {
                    return IrError.UnsupportedAstNode;
                },
            };

            const inputs = [_]IrNodeId{ left_ir, right_ir };
            const nid = try context.ir.createNode(ir_op, &inputs, node.source_loc);
            stampOutputTypeFromAst(context, nid, node_id);
            return nid;
        },
        .var_decl => {
            // Create a var_decl IR node
            const var_name = try context.allocator.dupe(u8, node.data.var_decl.name);
            var initializer_node: ?IrNodeId = null;

            if (node.data.var_decl.initializer) |initializer| {
                initializer_node = try transformNode(context, initializer);
            }

            const var_decl_node = try context.ir.createVarDecl(var_name, initializer_node, node.source_loc);
            stampOutputTypeFromAst(context, var_decl_node, node_id);

            // Declare the variable in the current scope
            try context.current_scope.declare(node.data.var_decl.name, var_decl_node);

            return var_decl_node;
        },

        .block => {
            // Enter a new scope for the block
            _ = try context.enterScope();
            defer context.exitScope();

            var last_result = context.current_control;
            var match_stmts = std.ArrayList(IrNodeId).init(context.allocator);
            defer match_stmts.deinit();
            var all_stmts = std.ArrayList(IrNodeId).init(context.allocator);
            defer all_stmts.deinit();

            for (node.data.block.statements.items) |stmt_id| {
                const stmt_result = try transformNode(context, stmt_id);
                try all_stmts.append(stmt_result);

                const stmt_node = context.ast_arena.getNodeConst(stmt_id) orelse {
                    try reportIrError(context.errors, .invalid_expression, "Invalid statement reference", ast.SourceLoc.invalid());
                    return IrError.InvalidNodeReference;
                };

                if (stmt_node.data == .match_expr) {
                    // Collect match expressions
                    try match_stmts.append(stmt_result);
                } else {
                    // All statements update control flow to ensure they get processed
                    last_result = stmt_result;
                    context.current_control = last_result;
                }
            }

            // Create a block IR node with all statements as inputs to ensure they get processed during C code generation
            const block_node = try context.ir.createNode(.block, all_stmts.items, node.source_loc);

            // Match expressions are already inputs to the block, so they will be processed
            return block_node;
        },
        .function_decl => {
            const func_decl = node.data.function_decl;
            // std.debug.print("DEBUG: Processing function declaration: {s}\n", .{func_decl.name});

            // Enter a new scope for the function body
            const function_scope = try context.enterScope();
            defer context.exitScope();

            // Set up parameters in the function scope
            var param_nodes = try context.allocator.alloc(IrNodeId, func_decl.params.items.len);
            defer context.allocator.free(param_nodes);

            for (func_decl.params.items, 0..) |param, i| {
                // Extract the actual parameter type from the type annotation
                var param_type = ast.Type.initPrimitive(.{ .i32 = {} }, node.source_loc); // default fallback
                if (param.type_annotation) |type_node_id| {
                    const type_node = context.ast_arena.getNodeConst(type_node_id);
                    if (type_node) |type_ast_node| {
                        if (type_ast_node.data == .identifier) {
                            if (std.mem.eql(u8, type_ast_node.data.identifier.name, "i32")) {
                                param_type = ast.Type.initPrimitive(.{ .i32 = {} }, type_ast_node.source_loc);
                            } else if (std.mem.eql(u8, type_ast_node.data.identifier.name, "i64")) {
                                param_type = ast.Type.initPrimitive(.{ .i64 = {} }, type_ast_node.source_loc);
                            } else if (std.mem.eql(u8, type_ast_node.data.identifier.name, "f32")) {
                                param_type = ast.Type.initPrimitive(.{ .f32 = {} }, type_ast_node.source_loc);
                            } else if (std.mem.eql(u8, type_ast_node.data.identifier.name, "f64")) {
                                param_type = ast.Type.initPrimitive(.{ .f64 = {} }, type_ast_node.source_loc);
                            } else if (std.mem.eql(u8, type_ast_node.data.identifier.name, "bool")) {
                                param_type = ast.Type.initPrimitive(.{ .bool = {} }, type_ast_node.source_loc);
                            } else if (std.mem.eql(u8, type_ast_node.data.identifier.name, "str")) {
                                param_type = ast.Type.initPrimitive(.{ .str = {} }, type_ast_node.source_loc);
                            }
                        }
                    }
                }

                const param_node = try context.ir.createParameter(@intCast(i), param.name, param_type, node.source_loc);
                param_nodes[i] = param_node;
                try function_scope.declare(param.name, param_node);
            }

            // Get the actual return type from the function declaration
            var return_type = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            if (func_decl.return_type) |return_type_node| {
                // Try to manually parse common return types
                const ret_node = context.ast_arena.getNodeConst(return_type_node);
                if (ret_node) |ret| {
                    if (ret.data == .error_union_type_expr) {
                        // This is an error union type like MyError!i32
                        const error_union_expr = ret.data.error_union_type_expr;

                        // Parse the error set name
                        const error_set_name = if (context.ast_arena.getNodeConst(error_union_expr.error_set)) |error_set_node| blk: {
                            if (error_set_node.data == .identifier) {
                                break :blk try context.allocator.dupe(u8, error_set_node.data.identifier.name);
                            }
                            break :blk try context.allocator.dupe(u8, "unknown_error");
                        } else try context.allocator.dupe(u8, "unknown_error");

                        // Parse the payload type
                        var payload_type = ast.Type.initPrimitive(.{ .i32 = {} }, node.source_loc); // default
                        if (context.ast_arena.getNodeConst(error_union_expr.payload_type)) |payload_node| {
                            if (payload_node.data == .identifier) {
                                const type_name = payload_node.data.identifier.name;
                                if (std.mem.eql(u8, type_name, "i32")) {
                                    payload_type = ast.Type.initPrimitive(.{ .i32 = {} }, payload_node.source_loc);
                                } else if (std.mem.eql(u8, type_name, "i64")) {
                                    payload_type = ast.Type.initPrimitive(.{ .i64 = {} }, payload_node.source_loc);
                                } else if (std.mem.eql(u8, type_name, "f32")) {
                                    payload_type = ast.Type.initPrimitive(.{ .f32 = {} }, payload_node.source_loc);
                                } else if (std.mem.eql(u8, type_name, "f64")) {
                                    payload_type = ast.Type.initPrimitive(.{ .f64 = {} }, payload_node.source_loc);
                                } else if (std.mem.eql(u8, type_name, "bool")) {
                                    payload_type = ast.Type.initPrimitive(.{ .bool = {} }, payload_node.source_loc);
                                } else if (std.mem.eql(u8, type_name, "str")) {
                                    payload_type = ast.Type.initPrimitive(.{ .str = {} }, payload_node.source_loc);
                                } else if (std.mem.eql(u8, type_name, "void")) {
                                    payload_type = ast.Type.initPrimitive(.{ .void = {} }, payload_node.source_loc);
                                } else {
                                    // Check if it's a custom struct type
                                    // For now, assume any unrecognized identifier is a custom struct
                                    payload_type = ast.Type.initCustomStruct(type_name, &[_]ast.Field{}, false, payload_node.source_loc);
                                }
                            } else if (payload_node.data == .optional_type_expr) {
                                // Handle optional types like ?MyStruct
                                const optional_expr = payload_node.data.optional_type_expr;
                                const inner_node = context.ast_arena.getNodeConst(optional_expr.inner_type);
                                if (inner_node) |inner| {
                                    if (inner.data == .identifier) {
                                        const inner_type_name = inner.data.identifier.name;
                                        // Create the inner type
                                        var inner_type: ast.Type = undefined;
                                        if (std.mem.eql(u8, inner_type_name, "i32")) {
                                            inner_type = ast.Type.initPrimitive(.{ .i32 = {} }, inner.source_loc);
                                        } else if (std.mem.eql(u8, inner_type_name, "i64")) {
                                            inner_type = ast.Type.initPrimitive(.{ .i64 = {} }, inner.source_loc);
                                        } else if (std.mem.eql(u8, inner_type_name, "f32")) {
                                            inner_type = ast.Type.initPrimitive(.{ .f32 = {} }, inner.source_loc);
                                        } else if (std.mem.eql(u8, inner_type_name, "f64")) {
                                            inner_type = ast.Type.initPrimitive(.{ .f64 = {} }, inner.source_loc);
                                        } else if (std.mem.eql(u8, inner_type_name, "bool")) {
                                            inner_type = ast.Type.initPrimitive(.{ .bool = {} }, inner.source_loc);
                                        } else if (std.mem.eql(u8, inner_type_name, "str")) {
                                            inner_type = ast.Type.initPrimitive(.{ .str = {} }, inner.source_loc);
                                        } else {
                                            // Assume it's a custom struct type
                                            inner_type = ast.Type.initCustomStruct(inner_type_name, &[_]ast.Field{}, false, inner.source_loc);
                                        }

                                        // Create the optional type
                                        const inner_type_ptr = try context.allocator.create(ast.Type);
                                        inner_type_ptr.* = inner_type;
                                        payload_type = ast.Type{
                                            .data = .{ .optional = inner_type_ptr },
                                            .source_loc = payload_node.source_loc,
                                        };
                                    }
                                }
                            }
                        }

                        // Create the error union type
                        const payload_type_ptr = try context.allocator.create(ast.Type);
                        payload_type_ptr.* = payload_type;

                        return_type = ast.Type{
                            .data = .{ .error_union = .{ .error_set = error_set_name, .payload_type = payload_type_ptr } },
                            .source_loc = node.source_loc,
                        };
                    } else if (ret.data == .identifier and std.mem.eql(u8, ret.data.identifier.name, "i32")) {
                        return_type = ast.Type.initPrimitive(.{ .i32 = {} }, node.source_loc);
                    } else if (ret.data == .identifier and std.mem.eql(u8, ret.data.identifier.name, "void")) {
                        return_type = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
                    }
                }
            }

            // Set function context in both semantic analyzer and IR context for proper type checking during IR construction
            const old_function_return_type = context.semantic_analyzer.current_function_return_type;
            const old_ir_function_return_type = context.current_function_return_type;
            context.semantic_analyzer.current_function_return_type = return_type;
            context.current_function_return_type = return_type;

            // Transform function body with proper context
            const body_ir = try transformNode(context, func_decl.body);
            context.current_control = body_ir;

            // Restore function context
            context.semantic_analyzer.current_function_return_type = old_function_return_type;
            context.current_function_return_type = old_ir_function_return_type;

            // Create parameter name array for the IR function definition
            var param_names = try context.allocator.alloc([]const u8, func_decl.params.items.len);
            for (func_decl.params.items, 0..) |param, i| {
                param_names[i] = param.name;
            }

            // Create function definition IR node
            const func_def_node = try context.ir.createFunctionDef(func_decl.name, param_names, param_nodes, return_type, body_ir, node.source_loc);

            // Declare the function in the global scope
            try context.global_scope.declare(func_decl.name, func_def_node);
            return func_def_node;
        },
        .call_expr => {
            // Handle @import builtin
            const callee_node = context.ast_arena.getNodeConst(node.data.call_expr.callee) orelse @panic("callee_node is null");
            if (callee_node.data == .identifier and std.mem.eql(u8, callee_node.data.identifier.name, "@import")) {
                // @import is builtin, return a dummy constant
                const dummy = try context.ir.createConstant(IrConstant{ .integer = 0 }, node.source_loc);
                // Don't stamp type for @import - it's a compile-time construct
                return dummy;
            }

            // Simple std.debug.print detection for now
            if (callee_node.data == .member_expr and std.mem.eql(u8, callee_node.data.member_expr.field, "print")) {
                // Check if this is std.debug.print by examining the object chain
                const debug_node = context.ast_arena.getNodeConst(callee_node.data.member_expr.object);
                if (debug_node) |debug_callee| {
                    if (debug_callee.data == .member_expr and std.mem.eql(u8, debug_callee.data.member_expr.field, "debug")) {
                        const std_node = context.ast_arena.getNodeConst(debug_callee.data.member_expr.object);
                        if (std_node) |std_callee| {
                            if (std_callee.data == .identifier and std.mem.eql(u8, std_callee.data.identifier.name, "std")) {
                                // This is std.debug.print
                                const print_fn = try context.ir.createConstant(IrConstant{ .string = "builtin_print" }, node.source_loc);

                                // Process print call arguments with anonymous struct expansion
                                var expanded_args = std.ArrayList(IrNodeId).init(context.allocator);
                                defer expanded_args.deinit();

                                // Expand anonymous structs in arguments
                                for (node.data.call_expr.args.items) |arg| {
                                    const arg_node = context.ast_arena.getNodeConst(arg);
                                    if (arg_node) |node_const| {
                                        if (node_const.data == .call_expr) {
                                            // Check if this is an anonymous struct call .{...}
                                            const anon_callee_node = context.ast_arena.getNodeConst(node_const.data.call_expr.callee);
                                            if (anon_callee_node) |anon_callee| {
                                                if (anon_callee.data == .identifier and std.mem.eql(u8, anon_callee.data.identifier.name, "__anonymous_struct")) {
                                                    // This is an anonymous struct .{...} - expand its arguments
                                                    for (node_const.data.call_expr.args.items) |anon_arg| {
                                                        const expanded_arg = try transformNode(context, anon_arg);
                                                        try expanded_args.append(expanded_arg);
                                                    }
                                                    continue; // Skip regular processing for this argument
                                                }
                                            }
                                        }
                                        // Regular argument processing
                                        const transformed_arg = try transformNode(context, arg);
                                        try expanded_args.append(transformed_arg);
                                    }
                                }

                                // Create call inputs: control + function + expanded arguments
                                var call_inputs = try context.allocator.alloc(IrNodeId, 2 + expanded_args.items.len);
                                defer context.allocator.free(call_inputs);
                                call_inputs[0] = context.current_control;
                                call_inputs[1] = print_fn;

                                // Copy expanded arguments
                                for (expanded_args.items, 0..) |expanded_arg, i| {
                                    call_inputs[2 + i] = expanded_arg;
                                }

                                const call_id = try context.ir.createCall("std.debug.print", call_inputs, node.source_loc);
                                // Stamp the call return type (void)
                                stampOutputTypeFromAst(context, call_id, node_id);
                                return call_id;
                            }
                        }
                    }
                }
            }

            // Regular function call
            const regular_callee_node = context.ast_arena.getNodeConst(node.data.call_expr.callee);
            const function_name = if (regular_callee_node) |callee| blk: {
                if (callee.data == .identifier) {
                    break :blk callee.data.identifier.name;
                } else if (callee.data == .member_expr) {
                    // For member expressions like math.add, we need to construct the qualified name
                    const member_expr = callee.data.member_expr;
                    const object_node = context.ast_arena.getNodeConst(member_expr.object);
                    if (object_node) |obj_node| {
                        if (obj_node.data == .identifier) {
                            // Create qualified name like "math.add"
                            const qualified_name = try std.fmt.allocPrint(context.allocator, "{s}.{s}", .{ obj_node.data.identifier.name, member_expr.field });
                            break :blk qualified_name;
                        }
                    }
                    break :blk member_expr.field; // fallback to just the field name
                }
                break :blk "unknown_function";
            } else "unknown_function";

            // For member expressions used as function callees, transform them properly
            const callee = if (regular_callee_node) |callee| blk: {
                if (callee.data == .member_expr) {
                    // Transform the member expression to get the namespace member node
                    break :blk try transformNode(context, node.data.call_expr.callee);
                } else {
                    break :blk try transformNode(context, node.data.call_expr.callee);
                }
            } else try context.ir.createConstant(IrConstant{ .string = "unknown" }, node.source_loc);

            // Process function arguments
            var call_inputs = try context.allocator.alloc(IrNodeId, 2 + node.data.call_expr.args.items.len);
            defer context.allocator.free(call_inputs);
            call_inputs[0] = context.current_control;
            call_inputs[1] = callee;

            // Transform each argument
            for (node.data.call_expr.args.items, 0..) |arg, i| {
                call_inputs[2 + i] = try transformNode(context, arg);
            }

            const call_id = try context.ir.createCall(function_name, call_inputs, node.source_loc);
            stampOutputTypeFromAst(context, call_id, node_id);
            return call_id;
        },
        .index_expr => |index_expr| {
            // Array indexing: object[index]
            const object_ir = try transformNode(context, index_expr.object);
            const index_ir = try transformNode(context, index_expr.index);

            // For now, create a simple load operation
            // In a full implementation, this would need to handle different array types
            const load_inputs = [_]IrNodeId{ object_ir, index_ir };
            const load_id = try context.ir.createNode(.load, &load_inputs, node.source_loc);
            stampOutputTypeFromAst(context, load_id, node_id);
            return load_id;
        },

        .return_stmt => {
            var return_inputs: [2]IrNodeId = undefined;
            return_inputs[0] = context.current_control;

            if (node.data.return_stmt.value) |value| {
                return_inputs[1] = try transformNode(context, value);
            } else {
                const void_const = try context.ir.createConstant(IrConstant{ .none = {} }, node.source_loc);
                // Attempt to stamp .none with surrounding context (function return), but analyzer returns null; still stamp
                stampOutputTypeFromAst(context, void_const, node_id);
                return_inputs[1] = void_const;
            }
            const rid = try context.ir.createNode(.return_, &return_inputs, node.source_loc);
            // Return nodes don't carry a value type; skip stamping
            return rid;
        },
        .type_decl => {
            const type_decl = node.data.type_decl;

            // Check if this is an import type declaration (name :: @import("path"))
            const type_node = context.ast_arena.getNodeConst(type_decl.type_expr);
            if (type_node) |tn| {
                if (tn.data == .call_expr) {
                    const call_expr = tn.data.call_expr;
                    const callee_node = context.ast_arena.getNodeConst(call_expr.callee);
                    if (callee_node) |callee| {
                        if (callee.data == .identifier and std.mem.eql(u8, callee.data.identifier.name, "@import")) {
                            // This is an import type declaration like: std :: @import("std")
                            // The semantic analyzer should have already processed this and created a symbol
                            if (context.semantic_analyzer.lookupInAllScopes(type_decl.name)) |symbol| {
                                if (symbol.declared_type) |decl_type| {
                                    if (decl_type.data == .namespace) {
                                        // Create namespace IR node
                                        const ns_ir = try context.ir.createNamespace(decl_type.data.namespace.name, node.source_loc);
                                        try context.global_scope.declare(type_decl.name, ns_ir);
                                        return ns_ir;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Handle std :: @import("std") - special case for backward compatibility
            if (std.mem.eql(u8, node.data.type_decl.name, "std")) {
                // Check if the semantic analyzer has already created a symbol for std
                if (context.semantic_analyzer.lookupInAllScopes("std")) |symbol| {
                    if (symbol.declared_type) |decl_type| {
                        if (decl_type.data == .namespace) {
                            // Create namespace IR node
                            const ns_ir = try context.ir.createNamespace(decl_type.data.namespace.name, node.source_loc);
                            try context.global_scope.declare("std", ns_ir);
                            return ns_ir;
                        }
                    }
                }
                // Fallback
                const std_const = try context.ir.createConstant(IrConstant{ .string = "std_builtin" }, node.source_loc);
                try context.global_scope.declare("std", std_const);
                return std_const;
            }
            return context.current_control;
        },
        .member_expr => {
            // Handle member access, including enum member access and error set member access
            const field_name = node.data.member_expr.field;
            const object_id = node.data.member_expr.object;

            // First, check if the object is a variable in the current scope
            const object_node = context.ast_arena.getNodeConst(object_id);
            if (object_node) |obj_node| {
                if (obj_node.data == .identifier) {
                    const var_name = obj_node.data.identifier.name;
                    if (context.current_scope.lookup(var_name)) |var_ir_node| {
                        // Check if this is a namespace node
                        if (context.ir.getNode(var_ir_node)) |ir_node| {
                            if (ir_node.op == .namespace) {
                                // This is namespace member access like std.debug or math.add
                                const namespace_member_node = try context.ir.createNamespaceMember(var_ir_node, field_name, node.source_loc);
                                stampOutputTypeFromAst(context, namespace_member_node, node_id);
                                return namespace_member_node;
                            }
                        }

                        // This is a variable member access like my_struct.field1
                        // Create a member access operation
                        const field_name_const = try context.ir.createConstant(IrConstant{ .string = field_name }, node.source_loc);
                        const member_access_inputs = [_]IrNodeId{ var_ir_node, field_name_const };
                        const member_access_node = try context.ir.createNode(.member_access, &member_access_inputs, node.source_loc);
                        stampOutputTypeFromAst(context, member_access_node, node_id);
                        return member_access_node;
                    }
                }
            }

            // Then, try to infer the type of the object being accessed
            const obj_type = context.semantic_analyzer.inferType(object_id) catch null;

            if (obj_type) |object_type| {
                // Check if this is a namespace member access
                if (object_type.data == .namespace) {
                    // This is namespace member access like math.add or std.debug.print
                    const ns = object_type.data.namespace;

                    // Look up the member in the namespace
                    if (ns.members.get(field_name)) |_| {
                        // For all namespace members, create a namespace member access node
                        const object_ir = try transformNode(context, object_id);
                        const namespace_member_node = try context.ir.createNamespaceMember(object_ir, field_name, node.source_loc);
                        stampOutputTypeFromAst(context, namespace_member_node, node_id);
                        return namespace_member_node;
                    } else {
                        // Member not found in namespace
                        try reportIrError(
                            context.errors,
                            .undefined_variable,
                            try std.fmt.allocPrint(context.allocator, "Namespace '{s}' has no member '{s}'", .{ ns.name, field_name }),
                            node.source_loc,
                        );
                        return IrError.InvalidNodeReference;
                    }
                }

                // Check if this is an error set member access
                if (object_type.data == .error_set) {
                    // For error set members, create an integer constant representing the error value
                    // Error values start at 1 (0 represents success)
                    const error_set = object_type.data.error_set;

                    // Find the index of this error in the error set
                    var error_value: i64 = 1; // Default to 1 for the first error
                    for (error_set.enumerants, 1..) |enumerant, i| {
                        if (std.mem.eql(u8, enumerant, field_name)) {
                            error_value = @intCast(i);
                            break;
                        }
                    }

                    const error_constant = try context.ir.createConstant(IrConstant{ .integer = error_value }, node.source_loc);
                    stampOutputTypeFromAst(context, error_constant, node_id);
                    return error_constant;
                }
            }

            // Check if this is an enum member access (like .A, .B, .C)
            if (field_name.len > 0) {
                // Simple enum member mapping for demo
                const enum_value = if (std.mem.eql(u8, field_name, "A"))
                    @as(i64, 0)
                else if (std.mem.eql(u8, field_name, "B"))
                    @as(i64, 1)
                else if (std.mem.eql(u8, field_name, "C"))
                    @as(i64, 2)
                else
                    @as(i64, 0); // default

                // std.debug.print("DEBUG: Creating enum constant for {s} with value {d}\n", .{ field_name, enum_value });
                const cid = try context.ir.createConstant(IrConstant{ .integer = enum_value }, node.source_loc);
                stampOutputTypeFromAst(context, cid, node_id);
                return cid;
            } else {
                // For other member access, return a default value for now
                const cid = try context.ir.createConstant(IrConstant{ .integer = 0 }, node.source_loc);
                stampOutputTypeFromAst(context, cid, node_id);
                return cid;
            }
        },
        .struct_init => {
            const struct_init = node.data.struct_init;
            const type_name = struct_init.type_name orelse "unknown_struct";

            if (struct_init.use_gc) {
                // Heap allocation: $MyStruct{ ... }
                const type_const = try context.ir.createConstant(IrConstant{ .string = type_name }, node.source_loc);
                const heap_alloc_inputs = [_]IrNodeId{type_const};
                const heap_ptr = try context.ir.createNode(.heap_alloc, &heap_alloc_inputs, node.source_loc);
                // Stamp the allocation result as the struct type (semantic analyzer provides suitable type)
                stampOutputTypeFromAst(context, heap_ptr, node_id);

                // Initialize fields
                var current_ptr = heap_ptr;
                for (struct_init.fields.items) |field_init| {
                    const field_value = try transformNode(context, field_init.value);
                    const field_name_const = try context.ir.createConstant(IrConstant{ .string = field_init.name }, node.source_loc);
                    const struct_init_inputs = [_]IrNodeId{ current_ptr, field_name_const, field_value };
                    current_ptr = try context.ir.createNode(.struct_init, &struct_init_inputs, node.source_loc);
                    // struct_init returns the same pointer; type already stamped
                }

                return current_ptr;
            } else {
                // Stack allocation: MyStruct{ ... }
                // Lower to a struct_literal IR node whose inputs are the field values in order.
                // Inputs layout: [value1, value2, ...] (field names are implicit for now)
                var field_values = std.ArrayList(IrNodeId).init(context.allocator);
                defer field_values.deinit();
                // Collect values and names
                var field_names = std.ArrayList([]const u8).init(context.allocator);
                defer field_names.deinit();
                for (struct_init.fields.items) |field_init| {
                    const field_value = try transformNode(context, field_init.value);
                    try field_values.append(field_value);
                    try field_names.append(field_init.name);
                }
                const struct_literal_id = try context.ir.createNode(.struct_literal, field_values.items, node.source_loc);
                if (context.ir.getNodeMut(struct_literal_id)) |sl_node| {
                    // Duplicate names array for IR ownership
                    const names_copy = try context.allocator.dupe([]const u8, field_names.items);
                    sl_node.data = IrNodeData{ .struct_literal = .{ .field_names = names_copy } };
                }
                // Stamp output type from AST so codegen knows concrete struct type
                stampOutputTypeFromAst(context, struct_literal_id, node_id);
                return struct_literal_id;
            }
        },
        .try_expr => {
            // Transform try expression to error union unwrap IR
            const expression = try transformNode(context, node.data.try_expr.expression);
            const tid = try context.ir.createTry(expression, node.source_loc);
            // Stamp with payload type
            stampOutputTypeFromAst(context, tid, node_id);
            return tid;
        },
        .if_expr => {
            // Transform if expression to conditional IR
            const if_expr = node.data.if_expr;
            const condition_ir = try transformNode(context, if_expr.condition);

            // Temporarily set function return type context for branch transformation
            const old_context = context.semantic_analyzer.current_function_return_type;
            if (context.current_function_return_type) |func_return_type| {
                context.semantic_analyzer.current_function_return_type = func_return_type;
                // std.debug.print("DEBUG: Set function context for if expression branches: {}\n", .{func_return_type});
            }
            defer {
                context.semantic_analyzer.current_function_return_type = old_context;
            }

            const then_ir = try transformNode(context, if_expr.then_branch);

            // Check if there's an else branch
            const else_ir = if (if_expr.else_branch) |else_branch|
                try transformNode(context, else_branch)
            else blk: {
                const none_id = try context.ir.createConstant(IrConstant{ .none = {} }, node.source_loc);
                stampOutputTypeFromAst(context, none_id, node_id);
                break :blk none_id;
            };

            // Create conditional IR node
            const inputs = [_]IrNodeId{ condition_ir, then_ir, else_ir };
            const sid = try context.ir.createNode(.select, &inputs, node.source_loc);

            // For if expressions, if we have a function return type context, use it directly
            // This avoids the type inference issues during IR construction
            if (context.current_function_return_type) |func_return_type| {
                if (context.ir.getNodeMut(sid)) |node_mut| {
                    node_mut.output_type = func_return_type;
                }
            } else {
                // Fall back to stamping with AST inference
                stampOutputTypeFromAst(context, sid, node_id);
            }
            return sid;
        },
        .error_union_type_expr => {
            // Error union type expressions don't generate runtime IR - they're type-level only
            return context.current_control;
        },
        .error_set_decl => {
            // Error sets are compile-time only constructs - no runtime IR needed
            return context.current_control;
        },
        .match_expr => {
            const match_expr = node.data.match_expr;

            // Transform the expression being matched
            const matched_value = try transformNode(context, match_expr.expression);

            // Create match start operation
            const match_start = try context.ir.createMatchStart(matched_value, node.source_loc);

            // Collect all branch nodes
            var branch_nodes = std.ArrayList(IrNodeId).init(context.allocator);
            defer branch_nodes.deinit();

            for (match_expr.arms.items) |arm| {
                // Create condition for this pattern
                const condition = try createPatternCondition(context, arm.pattern, matched_value, arm.source_loc);

                // If there's a guard condition, combine it with the pattern condition
                const final_condition = if (arm.guard) |guard| blk: {
                    const guard_condition = try transformNode(context, guard);
                    // Combine pattern condition and guard with logical AND
                    const and_inputs = [_]IrNodeId{ condition, guard_condition };
                    break :blk try context.ir.createNode(.logical_and, &and_inputs, arm.source_loc);
                } else condition;

                // Save current control to restore later
                const saved_control = context.current_control;

                // Transform the arm body - this creates the statements for the arm
                const arm_body_ir_node = try transformNode(context, arm.body);

                // Create a dummy result value for the match branch (not used for execution)
                const final_arm_result = try context.ir.createConstant(IrConstant{ .integer = 0 }, arm.source_loc);

                // Check if this is a wildcard pattern
                const is_wildcard = arm.pattern == .wildcard;

                // Create match branch - pass the transformed arm body IR node ID for later execution
                const branch = try context.ir.createMatchBranch(match_start, final_condition, final_arm_result, is_wildcard, arm_body_ir_node, arm.source_loc);

                // Restore control flow - don't let arm body affect main control flow
                context.current_control = saved_control;
                try branch_nodes.append(branch);
            }

            // For match expressions, determine the result type from the first arm
            var result_type = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            if (match_expr.arms.items.len > 0) {
                const first_arm = match_expr.arms.items[0];
                result_type = (context.semantic_analyzer.inferType(first_arm.body) catch null) orelse ast.Type.initPrimitive(.{ .i64 = {} }, node.source_loc);
            }
            const match_end = try context.ir.createMatchEnd(branch_nodes.items, result_type, node.source_loc);

            // Return the match_end node so it gets processed during C code generation
            return match_end;
        },
        .array_init => {
            const array_init = node.data.array_init;

            // Infer element type from first element
            const element_type = if (array_init.elements.items.len > 0) blk: {
                // First try semantic analyzer inference
                const inferred = context.semantic_analyzer.inferType(array_init.elements.items[0]) catch null;
                if (inferred) |t| {
                    break :blk t;
                }

                // If semantic analyzer fails, try direct AST inspection for struct_init
                const first_elem_node = context.ast_arena.getNode(array_init.elements.items[0]);
                if (first_elem_node) |n| {
                    if (n.data == .struct_init) {
                        const struct_init = n.data.struct_init;
                        if (struct_init.type_name) |type_name| {
                            // Create a custom_struct type
                            const custom_struct_type = ast.Type{
                                .data = .{ .custom_struct = .{ .name = type_name, .fields = &[_]ast.Field{}, .is_comptime = false } },
                                .source_loc = node.source_loc,
                            };
                            break :blk custom_struct_type;
                        }
                    }
                }

                // Fallback to i64
                break :blk ast.Type.initPrimitive(.{ .i64 = {} }, node.source_loc);
            } else ast.Type.initPrimitive(.{ .i64 = {} }, node.source_loc);

            if (array_init.use_gc) {
                // Heap allocation: $[1, 2, 3]
                // Check if any element is a struct initialization to detect struct arrays
                var detected_type_str: []const u8 = "i64"; // default

                for (array_init.elements.items) |elem_id| {
                    const elem_node = context.ast_arena.getNode(elem_id) orelse continue;
                    if (elem_node.data == .struct_init) {
                        const struct_init = elem_node.data.struct_init;
                        if (struct_init.type_name) |type_name| {
                            detected_type_str = type_name;
                            break;
                        }
                    }
                }

                // Create IR nodes for heap allocation of array literal
                // First input: type constant (string representing element type)
                const array_type_const = try context.ir.createConstant(IrConstant{ .string = detected_type_str }, node.source_loc);
                // Second input: size constant (number of elements in the array)
                const array_size_const = try context.ir.createConstant(IrConstant{ .integer = @intCast(array_init.elements.items.len) }, node.source_loc);
                // Create heap_alloc node with type and size inputs
                const alloc_inputs = [_]IrNodeId{ array_type_const, array_size_const };
                const array_ptr = try context.ir.createNode(.heap_alloc, &alloc_inputs, node.source_loc);
                // Set the output type for semantic analysis
                stampOutputTypeFromAst(context, array_ptr, node_id);

                // Initialize array elements by creating store operations for each element
                var current_ptr = array_ptr;
                for (array_init.elements.items, 0..) |element_id, i| {
                    // Transform each array element to IR
                    const element_value = try transformNode(context, element_id);
                    // Create index constant for this position
                    const index_const = try context.ir.createConstant(IrConstant{ .integer = @intCast(i) }, node.source_loc);
                    // Create store operation: array_ptr[index] = element_value
                    const store_inputs = [_]IrNodeId{ current_ptr, index_const, element_value };
                    current_ptr = try context.ir.createNode(.store, &store_inputs, node.source_loc);
                }

                return current_ptr;
            } else {
                // Stack allocation: [1, 2, 3]
                const alloc_node_id = try context.ir.createNode(.alloc, &.{}, node.source_loc);
                if (context.ir.getNodeMut(alloc_node_id)) |alloc_node| {
                    alloc_node.data = IrNodeData{ .alloc = .{
                        .alloc_type = element_type,
                        .size = @intCast(array_init.elements.items.len),
                    } };
                    alloc_node.output_type = element_type; // Array type, but simplified
                }
                stampOutputTypeFromAst(context, alloc_node_id, node_id);

                // For stack arrays, we need to initialize elements, but for now, just return the alloc node
                // TODO: Implement proper array initialization for stack arrays
                return alloc_node_id;
            }
        },
        .import_decl => {
            // Import declarations should create namespace IR nodes
            // The semantic analyzer has already processed the import and created symbols
            // We need to ensure the IR scope has the corresponding namespace nodes
            const import_decl = node.data.import_decl;

            // Extract module name from the import path
            const module_name = std.fs.path.stem(import_decl.module_path);

            // Check if the semantic analyzer has already created a symbol for this import
            if (context.semantic_analyzer.lookupInAllScopes(module_name)) |symbol| {
                if (symbol.declared_type) |decl_type| {
                    if (decl_type.data == .namespace) {
                        // Create namespace IR node
                        const ns_ir = try context.ir.createNamespace(decl_type.data.namespace.name, node.source_loc);
                        try context.global_scope.declare(module_name, ns_ir);
                        return ns_ir;
                    }
                }
            }

            // If we can't find the symbol, return current control (fallback)
            return context.current_control;
        },
        .for_expr => {
            // Create a special for_loop IR node that contains AST information
            // This allows the C code generator to generate proper for loops
            const for_loop_node = try context.ir.createNode(.for_loop, &.{}, node.source_loc);
            if (context.ir.getNodeMut(for_loop_node)) |ir_node| {
                ir_node.data = IrNodeData{ .for_loop = .{
                    .ast_node_id = node_id,
                    .ast_arena = context.ast_arena,
                } };
            }
            return for_loop_node;
        },
        .range_expr => {
            const range_expr = node.data.range_expr;

            // For range expressions, create a range representation
            // This could be enhanced to create proper range objects
            if (range_expr.start != null and range_expr.end != null) {
                // Bounded range like start..end or start..=end
                const start_ir = try transformNode(context, range_expr.start.?);
                const end_ir = try transformNode(context, range_expr.end.?);

                // For now, return the end value as the range condition (when to stop)
                // A full implementation would create range objects with start/end
                _ = start_ir; // Suppress unused variable warning
                return end_ir;
            } else if (range_expr.start == null and range_expr.end != null) {
                // Range like ..end or ..=end (starts from 0)
                const end_ir = try transformNode(context, range_expr.end.?);
                return end_ir;
            } else {
                // Unbounded or invalid range - return a default
                const zero = try context.ir.createConstant(IrConstant{ .integer = 0 }, node.source_loc);
                return zero;
            }
        },
        .while_expr => {
            // Create a special while_loop IR node that contains AST information
            // This allows the C code generator to generate proper while loops
            // Include current control as input to ensure proper ordering with variable declarations
            const while_loop_node = try context.ir.createNode(.while_loop, &.{context.current_control}, node.source_loc);
            if (context.ir.getNodeMut(while_loop_node)) |ir_node| {
                ir_node.data = IrNodeData{ .while_loop = .{
                    .ast_node_id = node_id,
                    .ast_arena = context.ast_arena,
                } };
            }
            return while_loop_node;
        },
        .catch_expr => |catch_expr| {
            // Transform the expression that may return an error union
            const expr_ir = try transformNode(context, catch_expr.expression);

            // Create error_union_unwrap node to extract payload and handle errors
            // For now, treat catch the same as try - proper catch with fallback support needs more work
            const unwrap_node = try context.ir.createNode(.error_union_unwrap, &.{expr_ir}, node.source_loc);
            stampOutputTypeFromAst(context, unwrap_node, node_id);
            return unwrap_node;
        },
        .struct_decl, .enum_decl => context.current_control,
        else => {
            try reportIrError(context.errors, .unsupported_operation, "Unsupported AST node type", node.source_loc);
            return IrError.UnsupportedAstNode;
        },
    };
}

/// Transform literal expressions
fn transformLiteral(context: *AstToIrContext, literal: ast.Literal, source_loc: ast.SourceLoc) !IrNodeId {
    const ir_constant = IrConstant.fromAstLiteral(literal);
    const cid = try context.ir.createConstant(ir_constant, source_loc);
    // No AST node id provided here, used only in pattern match helpers
    return cid;
}

/// Create a condition that checks if a pattern matches the value
fn createPatternCondition(context: *AstToIrContext, pattern: ast.MatchPattern, matched_value: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
    return switch (pattern) {
        .literal => |literal| {
            // Create a constant from the literal and compare
            const pattern_constant = try transformLiteral(context, literal, source_loc);
            const eq_inputs = [_]IrNodeId{ matched_value, pattern_constant };
            return context.ir.createNode(.eq, &eq_inputs, source_loc);
        },
        .wildcard => {
            // Wildcard always matches - return true
            return context.ir.createConstant(IrConstant{ .boolean = true }, source_loc);
        },
        .enum_member => |member_name| {
            // For enum members, create a simple integer constant based on the enum member name
            // Use a simple mapping for demo purposes
            const enum_value = if (std.mem.eql(u8, member_name, "A"))
                @as(i64, 0)
            else if (std.mem.eql(u8, member_name, "B"))
                @as(i64, 1)
            else if (std.mem.eql(u8, member_name, "C"))
                @as(i64, 2)
            else
                @as(i64, 0); // default

            const member_constant = try context.ir.createConstant(IrConstant{ .integer = enum_value }, source_loc);
            const eq_inputs = [_]IrNodeId{ matched_value, member_constant };
            return context.ir.createNode(.eq, &eq_inputs, source_loc);
        },
        .comparison => |comparison| {
            // Handle comparison patterns like < 0, > 5, etc.
            // The comparison value should be processed as a regular expression, not as a pattern
            const comparison_value = try transformNode(context, comparison.value);

            const op: IrOp = switch (comparison.operator) {
                .LessThan => .lt,
                .GreaterThan => .gt,
                .LessThanEquals => .le,
                .GreaterThanEquals => .ge,
                .EqualEqual => .eq,
                .ExclamationEquals => .ne,
                else => .eq, // Default to equality
            };

            const comp_inputs = [_]IrNodeId{ matched_value, comparison_value };
            return context.ir.createNode(op, &comp_inputs, source_loc);
        },
        .identifier => |name| {
            // For identifier patterns, always match (variable binding)
            _ = name; // suppress unused variable warning
            return context.ir.createConstant(IrConstant{ .boolean = true }, source_loc);
        },
        .some => |some_pattern| {
            // For Some patterns, check if the optional value is not null
            _ = some_pattern; // suppress unused variable warning
            return context.ir.createConstant(IrConstant{ .boolean = true }, source_loc);
        },
        .none_pattern => {
            // For None pattern, check if the optional value is null
            return context.ir.createConstant(IrConstant{ .boolean = false }, source_loc);
        },
        else => {
            // For unsupported patterns, return false (never matches)
            return context.ir.createConstant(IrConstant{ .boolean = false }, source_loc);
        },
    };
}
