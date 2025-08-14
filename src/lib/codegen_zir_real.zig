const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

// Import Zig's real ZIR
const Zir = std.zig.Zir;

pub const ZirGenerator = struct {
    allocator: std.mem.Allocator,
    arena: *const ast.AstArena,
    semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer,
    
    // ZIR building state
    instructions: std.MultiArrayList(Zir.Inst),
    string_bytes: std.ArrayList(u8),
    extra: std.ArrayList(u32),

    pub fn init(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer) ZirGenerator {
        return ZirGenerator{
            .allocator = allocator,
            .arena = arena,
            .semantic_analyzer = semantic_analyzer,
            .instructions = std.MultiArrayList(Zir.Inst){},
            .string_bytes = std.ArrayList(u8).init(allocator),
            .extra = std.ArrayList(u32).init(allocator),
        };
    }

    pub fn deinit(self: *ZirGenerator) void {
        self.instructions.deinit(self.allocator);
        self.string_bytes.deinit();
        self.extra.deinit();
    }

    pub fn generate(self: *ZirGenerator, root_node_id: ast.NodeId) !Zir {
        // Initialize with reserved indexes
        try self.string_bytes.append(0); // Index 0 is reserved
        try self.extra.appendSlice(&[_]u32{0, 0}); // Reserved for compile_errors and imports
        
        // Generate ZIR instructions from AST
        _ = try self.generateNode(root_node_id);
        
        // Build final ZIR structure
        const instructions_slice = self.instructions.toOwnedSlice();
        const string_bytes_slice = try self.string_bytes.toOwnedSlice();
        const extra_slice = try self.extra.toOwnedSlice();
        
        return Zir{
            .instructions = instructions_slice,
            .string_bytes = string_bytes_slice,
            .extra = extra_slice,
        };
    }

    fn generateNode(self: *ZirGenerator, node_id: ast.NodeId) anyerror!Zir.Inst.Ref {
        const node = self.arena.getNodeConst(node_id) orelse return .none;
        
        switch (node.data) {
            .literal => |literal| return try self.generateLiteral(literal),
            .identifier => |id| return try self.generateIdentifier(id),
            .binary_expr => |binary| return try self.generateBinaryExpr(binary),
            .unary_expr => |unary| return try self.generateUnaryExpr(unary),
            .call_expr => |call| return try self.generateCallExpr(call),
            .var_decl => |var_decl| return try self.generateVarDecl(var_decl),
            .function_decl => |func| return try self.generateFunctionDecl(func),
            .block => |block| return try self.generateBlock(block),
            .return_stmt => |ret| return try self.generateReturnStmt(ret),
            .import_decl => |import_decl| return try self.generateImportDecl(import_decl),
            .for_expr => |for_expr| return try self.generateForExpr(for_expr),
            .range_expr => |range_expr| return try self.generateRangeExpr(range_expr),
            .array_init => |array_init| return try self.generateArrayInit(array_init),
            else => {
                // Unsupported node types - emit compile error
                return try self.emitCompileError("Unsupported AST node type");
            },
        }
    }

    fn generateLiteral(self: *ZirGenerator, literal: ast.Literal) !Zir.Inst.Ref {
        switch (literal) {
            .integer => |int| {
                const inst_index = self.instructions.len;
                try self.instructions.append(self.allocator, .{
                    .tag = .int,
                    .data = .{ .int = @intCast(int.value) },
                });
                return @enumFromInt(inst_index);
            },
            .float => |float| {
                const inst_index = self.instructions.len;
                try self.instructions.append(self.allocator, .{
                    .tag = .float,
                    .data = .{ .float = float.value },
                });
                return @enumFromInt(inst_index);
            },
            .string => |str| {
                const string_index = self.string_bytes.items.len;
                try self.string_bytes.appendSlice(str.value);
                try self.string_bytes.append(0); // null terminate
                
                const inst_index = self.instructions.len;
                try self.instructions.append(self.allocator, .{
                    .tag = .str,
                    .data = .{ .str = .{
                        .start = @enumFromInt(string_index),
                        .len = @intCast(str.value.len),
                    }},
                });
                return @enumFromInt(inst_index);
            },
            .bool_true => {
                const inst_index = self.instructions.len;
                try self.instructions.append(self.allocator, .{
                    .tag = .int,
                    .data = .{ .int = 1 },
                });
                return @enumFromInt(inst_index);
            },
            .bool_false => {
                const inst_index = self.instructions.len;
                try self.instructions.append(self.allocator, .{
                    .tag = .int,
                    .data = .{ .int = 0 },
                });
                return @enumFromInt(inst_index);
            },
            else => return try self.emitCompileError("Unsupported literal type"),
        }
    }

    fn generateIdentifier(self: *ZirGenerator, id: anytype) !Zir.Inst.Ref {
        // Generate a decl_ref instruction
        const string_index = self.string_bytes.items.len;
        try self.string_bytes.appendSlice(id.name);
        try self.string_bytes.append(0); // null terminate

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .decl_ref,
            .data = .{ .str_tok = .{
                .start = @enumFromInt(string_index),
                .src_tok = 0, // TODO: proper source token
            }},
        });
        return @enumFromInt(inst_index);
    }

    fn generateBinaryExpr(self: *ZirGenerator, binary: anytype) !Zir.Inst.Ref {
        const lhs_ref = try self.generateNode(binary.left);
        const rhs_ref = try self.generateNode(binary.right);

        // Map Howl binary ops to ZIR tags
        const zir_tag: Zir.Inst.Tag = switch (binary.op) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            .eq => .cmp_eq,
            .ne => .cmp_neq,
            .lt => .cmp_lt,
            .le => .cmp_lte,
            .gt => .cmp_gt,
            .ge => .cmp_gte,
            .bit_and => .bit_and,
            .bit_or => .bit_or,
            .bit_xor => .xor,
            else => return try self.emitCompileError("Unsupported binary operator"),
        };

        // Store binary operands in extra data
        const payload_index = self.extra.items.len;
        try self.extra.appendSlice(&[_]u32{ @intFromEnum(lhs_ref), @intFromEnum(rhs_ref) });

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = zir_tag,
            .data = .{ .pl_node = .{
                .src_node = 0, // TODO: proper source node
                .payload_index = @intCast(payload_index),
            }},
        });
        return @enumFromInt(inst_index);
    }

    fn generateUnaryExpr(self: *ZirGenerator, unary: anytype) !Zir.Inst.Ref {
        const operand_ref = try self.generateNode(unary.operand);

        const zir_tag: Zir.Inst.Tag = switch (unary.op) {
            .negate => .negate,
            .not => .bool_not,
            .bit_not => .bit_not,
            else => return try self.emitCompileError("Unsupported unary operator"),
        };

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = zir_tag,
            .data = .{ .un_node = .{
                .src_node = 0, // TODO: proper source node
                .operand = operand_ref,
            }},
        });
        return @enumFromInt(inst_index);
    }

    fn generateCallExpr(self: *ZirGenerator, call: anytype) !Zir.Inst.Ref {
        // Check if this is a builtin function call
        if (try self.handleBuiltinCall(call)) |builtin_ref| {
            return builtin_ref;
        }
        
        const callee_ref = try self.generateNode(call.callee);
        
        // Generate arguments
        var arg_refs = std.ArrayList(Zir.Inst.Ref).init(self.allocator);
        defer arg_refs.deinit();
        
        for (call.args.items) |arg| {
            const arg_ref = try self.generateNode(arg);
            try arg_refs.append(arg_ref);
        }

        // Store call data in extra
        const payload_index = self.extra.items.len;
        try self.extra.append(@intFromEnum(callee_ref)); // callee
        try self.extra.append(@intCast(arg_refs.items.len)); // args_len
        for (arg_refs.items) |arg_ref| {
            try self.extra.append(@intFromEnum(arg_ref));
        }

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .call,
            .data = .{ .pl_node = .{
                .src_node = 0, // TODO: proper source node
                .payload_index = @intCast(payload_index),
            }},
        });
        return @enumFromInt(inst_index);
    }

    fn generateVarDecl(self: *ZirGenerator, var_decl: anytype) !Zir.Inst.Ref {
        // Skip std import declarations
        if (std.mem.eql(u8, var_decl.name, "std")) {
            return .none;
        }

        // Generate alloc instruction for the variable
        const alloc_tag: Zir.Inst.Tag = if (var_decl.is_mutable) .alloc_mut else .alloc;
        
        // For now, use a generic type (we'd need type inference here)
        const type_ref = Zir.Inst.Ref.i32_type; // Simplified

        const alloc_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = alloc_tag,
            .data = .{ .un_node = .{
                .src_node = 0, // TODO: proper source node
                .operand = type_ref,
            }},
        });
        const alloc_ref: Zir.Inst.Ref = @enumFromInt(alloc_index);

        // If there's an initializer, generate store instruction
        if (var_decl.initializer) |initializer| {
            const init_ref = try self.generateNode(initializer);
            
            // Store initialization value
            const payload_index = self.extra.items.len;
            try self.extra.appendSlice(&[_]u32{ @intFromEnum(alloc_ref), @intFromEnum(init_ref) });

            _ = self.instructions.len; // store_index unused for now
            try self.instructions.append(self.allocator, .{
                .tag = .store_node,
                .data = .{ .pl_node = .{
                    .src_node = 0, // TODO: proper source node
                    .payload_index = @intCast(payload_index),
                }},
            });
        }

        return alloc_ref;
    }

    fn generateFunctionDecl(self: *ZirGenerator, func: anytype) !Zir.Inst.Ref {
        // Generate function body
        const body_ref = try self.generateNode(func.body);
        
        // Store function data in extra
        const payload_index = self.extra.items.len;
        try self.extra.append(@intCast(func.params.items.len)); // param count
        try self.extra.append(@intFromEnum(body_ref)); // body
        try self.extra.append(0); // return type (simplified)

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .func,
            .data = .{ .pl_node = .{
                .src_node = 0, // TODO: proper source node
                .payload_index = @intCast(payload_index),
            }},
        });
        return @enumFromInt(inst_index);
    }

    fn generateBlock(self: *ZirGenerator, block: anytype) !Zir.Inst.Ref {
        // Generate all statements in the block
        var stmt_refs = std.ArrayList(Zir.Inst.Ref).init(self.allocator);
        defer stmt_refs.deinit();

        for (block.statements.items) |stmt_id| {
            const stmt_ref = try self.generateNode(stmt_id);
            if (stmt_ref != .none) {
                try stmt_refs.append(stmt_ref);
            }
        }

        // Store block data in extra
        const payload_index = self.extra.items.len;
        try self.extra.append(@intCast(stmt_refs.items.len)); // statement count
        for (stmt_refs.items) |stmt_ref| {
            try self.extra.append(@intFromEnum(stmt_ref));
        }

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .block,
            .data = .{ .pl_node = .{
                .src_node = 0, // TODO: proper source node
                .payload_index = @intCast(payload_index),
            }},
        });
        return @enumFromInt(inst_index);
    }

    fn generateReturnStmt(self: *ZirGenerator, ret: anytype) !Zir.Inst.Ref {
        const value_ref = if (ret.value) |value| 
            try self.generateNode(value) 
        else 
            Zir.Inst.Ref.void_value;

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .ret_node,
            .data = .{ .un_node = .{
                .src_node = 0, // TODO: proper source node
                .operand = value_ref,
            }},
        });
        return @enumFromInt(inst_index);
    }

    fn generateImportDecl(self: *ZirGenerator, import_decl: anytype) !Zir.Inst.Ref {
        _ = self; // Suppress unused parameter warning
        _ = import_decl; // Skip for now
        return .none;
    }

    fn emitCompileError(self: *ZirGenerator, message: []const u8) !Zir.Inst.Ref {
        const string_index = self.string_bytes.items.len;
        try self.string_bytes.appendSlice(message);
        try self.string_bytes.append(0); // null terminate

        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .compile_error,
            .data = .{ .un_node = .{
                .src_node = 0,
                .operand = @enumFromInt(string_index),
            }},
        });
        return @enumFromInt(inst_index);
    }
    
    /// Handle builtin function calls with special ZIR instructions
    fn handleBuiltinCall(self: *ZirGenerator, call: anytype) !?Zir.Inst.Ref {
        // Get the callee node to check if it's a builtin function
        const callee_node = self.arena.getNodeConst(call.callee) orelse return null;
        
        if (callee_node.data != .identifier) return null;
        const ident = callee_node.data.identifier;
        
        // Check for built-in functions
        if (std.mem.eql(u8, ident.name, "import")) {
            return try self.generateImportBuiltin(call);
        }
        if (std.mem.eql(u8, ident.name, "sizeOf")) {
            return try self.generateSizeOfBuiltin(call);
        }
        if (std.mem.eql(u8, ident.name, "TypeOf")) {
            return try self.generateTypeOfBuiltin(call);
        }
        if (std.mem.eql(u8, ident.name, "add_s")) {
            return try self.generateSafeArithmeticBuiltin("add", call);
        }
        if (std.mem.eql(u8, ident.name, "sub_s")) {
            return try self.generateSafeArithmeticBuiltin("sub", call);
        }
        if (std.mem.eql(u8, ident.name, "mul_s")) {
            return try self.generateSafeArithmeticBuiltin("mul", call);
        }
        if (std.mem.eql(u8, ident.name, "div_s")) {
            return try self.generateSafeArithmeticBuiltin("div", call);
        }
        if (std.mem.eql(u8, ident.name, "truncate")) {
            return try self.generateTruncateBuiltin(call);
        }
        if (std.mem.eql(u8, ident.name, "castUp")) {
            return try self.generateCastBuiltin("castUp", call);
        }
        if (std.mem.eql(u8, ident.name, "castDown")) {
            return try self.generateCastBuiltin("castDown", call);
        }
        if (std.mem.eql(u8, ident.name, "panic")) {
            return try self.generatePanicBuiltin(call);
        }
        if (std.mem.eql(u8, ident.name, "compileError")) {
            return try self.generateCompileErrorBuiltin(call);
        }
        
        // Not a recognized builtin function
        return null;
    }
    
    /// Generate @import builtin
    fn generateImportBuiltin(self: *ZirGenerator, call: anytype) !Zir.Inst.Ref {
        if (call.args.items.len != 1) {
            return self.emitCompileError("@import expects exactly one argument");
        }
        
        // For now, just generate a placeholder using c_import
        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .c_import,
            .data = .{ .un_node = .{
                .src_node = 0,
                .operand = .none,
            }},
        });
        return @enumFromInt(inst_index);
    }
    
    /// Generate @sizeOf builtin
    fn generateSizeOfBuiltin(self: *ZirGenerator, call: anytype) !Zir.Inst.Ref {
        if (call.args.items.len != 1) {
            return self.emitCompileError("@sizeOf expects exactly one argument");
        }
        
        const type_ref = try self.generateNode(call.args.items[0]);
        
        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .size_of,
            .data = .{ .un_node = .{
                .src_node = 0,
                .operand = type_ref,
            }},
        });
        return @enumFromInt(inst_index);
    }
    
    /// Generate @TypeOf builtin
    fn generateTypeOfBuiltin(self: *ZirGenerator, call: anytype) !Zir.Inst.Ref {
        if (call.args.items.len != 1) {
            return self.emitCompileError("@TypeOf expects exactly one argument");
        }
        
        const value_ref = try self.generateNode(call.args.items[0]);
        
        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .typeof,
            .data = .{ .un_node = .{
                .src_node = 0,
                .operand = value_ref,
            }},
        });
        return @enumFromInt(inst_index);
    }
    
    /// Generate safe arithmetic builtins like @add_s, @sub_s, etc.
    fn generateSafeArithmeticBuiltin(self: *ZirGenerator, operation: []const u8, call: anytype) !Zir.Inst.Ref {
        if (call.args.items.len != 2) {
            const msg = try std.fmt.allocPrint(self.allocator, "@{s}_s expects exactly two arguments", .{operation});
            defer self.allocator.free(msg);
            return self.emitCompileError(msg);
        }
        
        const lhs_ref = try self.generateNode(call.args.items[0]);
        const rhs_ref = try self.generateNode(call.args.items[1]);
        
        // Store operands in extra data
        const payload_index = self.extra.items.len;
        try self.extra.append(@intFromEnum(lhs_ref));
        try self.extra.append(@intFromEnum(rhs_ref));
        
        const inst_index = self.instructions.len;
        const tag: Zir.Inst.Tag = if (std.mem.eql(u8, operation, "add"))
            .add
        else if (std.mem.eql(u8, operation, "sub"))
            .sub
        else if (std.mem.eql(u8, operation, "mul"))
            .mul
        else if (std.mem.eql(u8, operation, "div"))
            .div
        else
            .add; // Default fallback
            
        try self.instructions.append(self.allocator, .{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = 0,
                .payload_index = @intCast(payload_index),
            }},
        });
        return @enumFromInt(inst_index);
    }
    
    /// Generate type casting builtins
    fn generateCastBuiltin(self: *ZirGenerator, cast_type: []const u8, call: anytype) !Zir.Inst.Ref {
        if (call.args.items.len != 2) {
            const msg = try std.fmt.allocPrint(self.allocator, "@{s} expects exactly two arguments", .{cast_type});
            defer self.allocator.free(msg);
            return self.emitCompileError(msg);
        }
        
        const target_type_ref = try self.generateNode(call.args.items[0]);
        const value_ref = try self.generateNode(call.args.items[1]);
        
        // Store cast data in extra
        const payload_index = self.extra.items.len;
        try self.extra.append(@intFromEnum(target_type_ref));
        try self.extra.append(@intFromEnum(value_ref));
        
        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .int_cast,
            .data = .{ .pl_node = .{
                .src_node = 0,
                .payload_index = @intCast(payload_index),
            }},
        });
        return @enumFromInt(inst_index);
    }
    
    /// Generate @truncate builtin
    fn generateTruncateBuiltin(self: *ZirGenerator, call: anytype) !Zir.Inst.Ref {
        return self.generateCastBuiltin("truncate", call);
    }
    
    /// Generate @panic builtin
    fn generatePanicBuiltin(self: *ZirGenerator, call: anytype) !Zir.Inst.Ref {
        if (call.args.items.len != 1) {
            return self.emitCompileError("@panic expects exactly one argument");
        }
        
        const message_ref = try self.generateNode(call.args.items[0]);
        
        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .panic,
            .data = .{ .un_node = .{
                .src_node = 0,
                .operand = message_ref,
            }},
        });
        return @enumFromInt(inst_index);
    }
    
    /// Generate @compileError builtin
    fn generateCompileErrorBuiltin(self: *ZirGenerator, call: anytype) !Zir.Inst.Ref {
        if (call.args.items.len != 1) {
            return self.emitCompileError("@compileError expects exactly one argument");
        }
        
        // For @compileError, we emit the error immediately
        return self.emitCompileError("Compile-time error triggered by @compileError");
    }
    
    /// Generate for loop expression
    fn generateForExpr(self: *ZirGenerator, for_expr: anytype) !Zir.Inst.Ref {
        // For now, implement a simplified for loop that just executes the body once
        // This is a basic implementation - full loop semantics would require 
        // more complex ZIR instruction sequences
        
        // Check if this is a range-based for loop
        const iterable_node = self.arena.getNodeConst(for_expr.iterable);
        if (iterable_node) |node| {
            if (node.data == .range_expr) {
                return try self.generateRangeForLoop(for_expr);
            }
        }
        
        // For array iteration, we'll generate the iterable and body
        // This is a simplified implementation
        _ = try self.generateNode(for_expr.iterable);
        const body_ref = try self.generateNode(for_expr.body);
        
        // For now, just return the body result
        // TODO: Implement proper loop iteration semantics
        return body_ref;
    }
    
    /// Generate range-based for loop
    fn generateRangeForLoop(self: *ZirGenerator, for_expr: anytype) !Zir.Inst.Ref {
        // Generate a simplified range loop
        // This implementation just returns a constant for now
        // TODO: Implement proper range iteration with loop counters
        
        const body_ref = try self.generateNode(for_expr.body);
        
        // For demonstration, return the body result
        // In a full implementation, this would generate loop initialization,
        // condition checking, increment, and body execution
        return body_ref;
    }
    
    /// Generate range expression
    fn generateRangeExpr(self: *ZirGenerator, range_expr: anytype) !Zir.Inst.Ref {
        // Generate the start and end expressions
        const start_ref = if (range_expr.start) |start_id|
            try self.generateNode(start_id)
        else
            try self.generateLiteral(.{ .integer = .{ .value = 0 } });
            
        const end_ref = if (range_expr.end) |end_id|
            try self.generateNode(end_id)
        else
            try self.generateLiteral(.{ .integer = .{ .value = 0 } });
        
        // For now, just return the end value as a simplified range representation
        // TODO: Create a proper range data structure
        _ = start_ref; // Acknowledge that we're not using start_ref yet
        return end_ref;
    }
    
    /// Generate array initialization
    fn generateArrayInit(self: *ZirGenerator, array_init: anytype) !Zir.Inst.Ref {
        // Generate all array elements
        var element_refs = std.ArrayList(Zir.Inst.Ref).init(self.allocator);
        defer element_refs.deinit();
        
        for (array_init.elements.items) |element_id| {
            const element_ref = try self.generateNode(element_id);
            try element_refs.append(element_ref);
        }
        
        // Store array elements in extra data
        const payload_index = self.extra.items.len;
        try self.extra.append(@intCast(element_refs.items.len)); // Array length
        for (element_refs.items) |ref| {
            try self.extra.append(@intFromEnum(ref));
        }
        
        // Generate array instruction
        const inst_index = self.instructions.len;
        try self.instructions.append(self.allocator, .{
            .tag = .array_init,
            .data = .{ .pl_node = .{
                .src_node = 0,
                .payload_index = @intCast(payload_index),
            }},
        });
        return @enumFromInt(inst_index);
    }
};