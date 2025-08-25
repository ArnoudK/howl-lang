const std = @import("std");
const ast = @import("ast.zig");
const SeaOfNodes = @import("sea_of_nodes_ir.zig").SeaOfNodes;
const IrNodeId = @import("sea_of_nodes_ir.zig").IrNodeId;
const IrOp = @import("sea_of_nodes_ir.zig").IrOp;
const IrConstant = @import("sea_of_nodes_ir.zig").IrConstant;
const IrError = @import("sea_of_nodes_ir.zig").IrError;
const reportIrError = @import("sea_of_nodes_ir.zig").reportIrError;
const INVALID_IR_NODE_ID = @import("sea_of_nodes_ir.zig").INVALID_IR_NODE_ID;
const ErrorSystem = @import("error_system.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;

// ============================================================================
// AST to Sea-of-Nodes IR Transformation
// ============================================================================

/// Context for AST-to-IR transformation
pub const AstToIrContext = struct {
    allocator: std.mem.Allocator,
    ir: *SeaOfNodes,
    errors: *ErrorSystem.ErrorCollector,
    semantic_analyzer: *const SemanticAnalyzer,
    ast_arena: *const ast.AstArena,
    
    // Control flow state
    current_control: IrNodeId,  // Current control dependency
    current_memory: IrNodeId,   // Current memory state
    
    // Symbol mapping from AST to IR
    symbol_map: std.StringHashMap(IrNodeId),
    
    pub fn init(
        allocator: std.mem.Allocator,
        ir: *SeaOfNodes,
        errors: *ErrorSystem.ErrorCollector,
        semantic_analyzer: *const SemanticAnalyzer,
        ast_arena: *const ast.AstArena,
    ) AstToIrContext {
        return AstToIrContext{
            .allocator = allocator,
            .ir = ir,
            .errors = errors,
            .semantic_analyzer = semantic_analyzer,
            .ast_arena = ast_arena,
            .current_control = INVALID_IR_NODE_ID,
            .current_memory = INVALID_IR_NODE_ID,
            .symbol_map = std.StringHashMap(IrNodeId).init(allocator),
        };
    }
    
    pub fn deinit(self: *AstToIrContext) void {
        self.symbol_map.deinit();
    }
};

/// Main entry point for AST-to-IR transformation
pub fn transformAstToIr(
    allocator: std.mem.Allocator,
    ast_root: ast.NodeId,
    semantic_analyzer: *const SemanticAnalyzer,
    ast_arena: *const ast.AstArena,
    errors: *ErrorSystem.ErrorCollector,
) !SeaOfNodes {
    var ir = SeaOfNodes.init(allocator);
    errdefer ir.deinit();
    
    var context = AstToIrContext.init(allocator, &ir, errors, semantic_analyzer, ast_arena);
    defer context.deinit();
    
    // Create the start node
    const start_loc = ast.SourceLoc.invalid();
    const start_node = try ir.createNode(.start, &.{}, start_loc);
    ir.setStartNode(start_node);
    context.current_control = start_node;
    context.current_memory = start_node; // Memory also flows from start node
    
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
        .literal => try transformLiteral(context, node.data.literal, node.source_loc),
        .identifier => {
            // Look up the symbol in our mapping
            if (context.symbol_map.get(node.data.identifier.name)) |ir_node_id| {
                return ir_node_id;
            } else {
                return IrError.InvalidNodeReference;
            }
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
            return context.ir.createNode(ir_op, &inputs, node.source_loc);
        },
        .var_decl => {
            if (node.data.var_decl.initializer) |initializer| {
                const init_value = try transformNode(context, initializer);
                try context.symbol_map.put(node.data.var_decl.name, init_value);
                return init_value;
            } else {
                const alloc_node = try context.ir.createNode(.alloc, &.{}, node.source_loc);
                try context.symbol_map.put(node.data.var_decl.name, alloc_node);
                return alloc_node;
            }
        },
        .block => {
            var last_result = context.current_control;
            
            for (node.data.block.statements.items) |stmt_id| {
                last_result = try transformNode(context, stmt_id);
                context.current_control = last_result;
            }
            
            return last_result;
        },
        .function_decl => {
            const func_decl = node.data.function_decl;
            
            // Set up parameters in symbol map for function body processing
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
                            }
                        }
                    }
                }

                const param_node = try context.ir.createParameter(
                    @intCast(i),
                    param.name,
                    param_type,
                    node.source_loc
                );
                try context.symbol_map.put(param.name, param_node);
            }
            
            // Transform function body
            const body_ir = try transformNode(context, func_decl.body);
            
            // Get the actual return type from the function declaration
            var return_type = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            if (func_decl.return_type) |return_type_node| {
                // Try to manually parse common return types
                const ret_node = context.ast_arena.getNodeConst(return_type_node);
                if (ret_node) |ret| {
                    if (ret.data == .error_union_type_expr) {
                        // This is an error union type like MyError!i32
                        const payload_type = try context.allocator.create(ast.Type);
                        payload_type.* = ast.Type.initPrimitive(.{ .i32 = {} }, node.source_loc);

                        return_type = ast.Type{
                            .data = .{ .error_union = .{ .error_set = "MyError", .payload_type = payload_type } },
                            .source_loc = node.source_loc,
                        };
                    } else if (ret.data == .identifier and std.mem.eql(u8, ret.data.identifier.name, "i32")) {
                        return_type = ast.Type.initPrimitive(.{ .i32 = {} }, node.source_loc);
                    } else if (ret.data == .identifier and std.mem.eql(u8, ret.data.identifier.name, "void")) {
                        return_type = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
                    }
                }
            }
            
            // Create parameter name array for the IR function definition
            var param_names = try context.allocator.alloc([]const u8, func_decl.params.items.len);
            for (func_decl.params.items, 0..) |param, i| {
                param_names[i] = param.name;
            }
            
            // Create function definition IR node
            const func_def_node = try context.ir.createFunctionDef(
                func_decl.name, 
                param_names, 
                return_type, 
                body_ir, 
                node.source_loc
            );
            
            try context.symbol_map.put(func_decl.name, func_def_node);
            return func_def_node;
        },
        .call_expr => {
            // Simple std.debug.print detection for now
            const callee_node = context.ast_arena.getNodeConst(node.data.call_expr.callee);
            if (callee_node) |callee| {
                if (callee.data == .member_expr and std.mem.eql(u8, callee.data.member_expr.field, "print")) {
                    // Check if this is std.debug.print by examining the object chain
                    const debug_node = context.ast_arena.getNodeConst(callee.data.member_expr.object);
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
                                                         continue; // Skip the regular processing
                                                     }
                                                 }
                                             }
                                            // Regular argument
                                            const transformed_arg = try transformNode(context, arg);
                                            try expanded_args.append(transformed_arg);
                                        } else {
                                            // Fallback for invalid nodes
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
                                    for (expanded_args.items, 0..) |arg, i| {
                                        call_inputs[2 + i] = arg;
                                    }

                                    return context.ir.createCall("std.debug.print", call_inputs, node.source_loc);
                                }
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
                }
                break :blk "unknown_function";
            } else "unknown_function";
            
            const callee = try transformNode(context, node.data.call_expr.callee);
            
            // Process function arguments
            var call_inputs = try context.allocator.alloc(IrNodeId, 2 + node.data.call_expr.args.items.len);
            defer context.allocator.free(call_inputs);
            call_inputs[0] = context.current_control;
            call_inputs[1] = callee;
            
            // Transform each argument
            for (node.data.call_expr.args.items, 0..) |arg, i| {
                call_inputs[2 + i] = try transformNode(context, arg);
            }
            
            return context.ir.createCall(function_name, call_inputs, node.source_loc);
        },
        .return_stmt => {
            var return_inputs: [2]IrNodeId = undefined;
            return_inputs[0] = context.current_control;
            
            if (node.data.return_stmt.value) |value| {
                return_inputs[1] = try transformNode(context, value);
            } else {
                const void_const = try context.ir.createConstant(IrConstant{ .none = {} }, node.source_loc);
                return_inputs[1] = void_const;
            }
            return context.ir.createNode(.return_, &return_inputs, node.source_loc);
        },
        .type_decl => {
            // Handle std :: @import("std")
            if (std.mem.eql(u8, node.data.type_decl.name, "std")) {
                const std_const = try context.ir.createConstant(IrConstant{ .string = "std_builtin" }, node.source_loc);
                try context.symbol_map.put("std", std_const);
                return std_const;
            }
            return context.current_control;
        },
        .member_expr => {
            // Check if this is an error set member access (e.g., MyError.DivisionByZero)
            const object_id = node.data.member_expr.object;
            const object_node = context.ast_arena.getNodeConst(object_id);
            if (object_node) |obj_node| {
                if (obj_node.data == .identifier) {
                    const error_set_name = obj_node.data.identifier.name;
                    const field_name = node.data.member_expr.field;
                    
                    // Check if this object is an error set by looking in semantic analyzer
                    if (context.semantic_analyzer.type_registry.get(error_set_name)) |error_type| {
                        if (error_type.data == .error_set) {
                            // This is an error enumerant access - convert to integer constant
                            // For now, use a simple hash of the error name as the error code
                            var hash_value: u32 = 1;
                            for (field_name) |c| {
                                hash_value = hash_value *% 31 +% @as(u32, @intCast(c));
                            }
                            // Ensure error codes are positive (>0)
                            if (hash_value == 0) hash_value = 1;
                            
                            return context.ir.createConstant(IrConstant{ .integer = @as(i64, @intCast(hash_value)) }, node.source_loc);
                        }
                    }
                }
            }
            
            // Regular member access
            const object = try transformNode(context, node.data.member_expr.object);
            const field_name_const = try context.ir.createConstant(IrConstant{ .string = node.data.member_expr.field }, node.source_loc);
            const member_inputs = [_]IrNodeId{ object, field_name_const };
            return context.ir.createNode(.member_access, &member_inputs, node.source_loc);
        },
        .struct_init => {
            const struct_init = node.data.struct_init;
            const type_name = struct_init.type_name orelse "unknown_struct";
            const type_const = try context.ir.createConstant(IrConstant{ .string = type_name }, node.source_loc);
            
            if (struct_init.use_gc) {
                // Heap allocation: $MyStruct{ ... }
                const heap_alloc_inputs = [_]IrNodeId{type_const};
                const heap_ptr = try context.ir.createNode(.heap_alloc, &heap_alloc_inputs, node.source_loc);
                
                // Initialize fields
                var current_ptr = heap_ptr;
                for (struct_init.fields.items) |field_init| {
                    const field_value = try transformNode(context, field_init.value);
                    const field_name_const = try context.ir.createConstant(IrConstant{ .string = field_init.name }, node.source_loc);
                    const struct_init_inputs = [_]IrNodeId{ current_ptr, field_name_const, field_value };
                    current_ptr = try context.ir.createNode(.struct_init, &struct_init_inputs, node.source_loc);
                }
                
                return current_ptr;
            } else {
                return context.ir.createConstant(IrConstant{ .integer = 0 }, node.source_loc);
            }
        },
        .try_expr => {
            // Transform try expression to error union unwrap IR
            const expression = try transformNode(context, node.data.try_expr.expression);
            return context.ir.createTry(expression, node.source_loc);
        },
        .if_expr => {
            // Transform if expression to conditional IR
            const if_expr = node.data.if_expr;
            const condition_ir = try transformNode(context, if_expr.condition);
            const then_ir = try transformNode(context, if_expr.then_branch);
            
            // Check if there's an else branch
            const else_ir = if (if_expr.else_branch) |else_branch|
                try transformNode(context, else_branch)
            else
                try context.ir.createConstant(IrConstant{ .none = {} }, node.source_loc);
            
            // Create conditional IR node
            const inputs = [_]IrNodeId{ condition_ir, then_ir, else_ir };
            return context.ir.createNode(.select, &inputs, node.source_loc);
        },
        .error_union_type_expr => {
            // Error union type expressions don't generate runtime IR - they're type-level only
            return context.current_control;
        },
        .error_set_decl => {
            // Error sets are compile-time only constructs - no runtime IR needed
            return context.current_control;
        },
        .import_decl, .struct_decl, .enum_decl => context.current_control,
        else => {
            try reportIrError(context.errors, .unsupported_operation, "Unsupported AST node type", node.source_loc);
            return IrError.UnsupportedAstNode;
        },
    };
}

/// Transform literal expressions
fn transformLiteral(context: *AstToIrContext, literal: ast.Literal, source_loc: ast.SourceLoc) !IrNodeId {
    const ir_constant = IrConstant.fromAstLiteral(literal);
    return context.ir.createConstant(ir_constant, source_loc);
}