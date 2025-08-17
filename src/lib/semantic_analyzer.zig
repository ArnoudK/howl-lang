const std = @import("std");
const ast = @import("ast.zig");
const ErrorSystem = @import("error_system.zig");

// ============================================================================
// Compile-time Value System
// ============================================================================

/// Values that can be computed and manipulated at compile time
pub const ComptimeValue = union(enum) {
    type_value: ast.Type,
    integer: i64,
    float: f64,
    boolean: bool,
    string: []const u8,
    struct_type: struct {
        name: []const u8,
        fields: []ast.Field,
        is_comptime: bool,
    },
    enum_type: struct {
        name: []const u8,
        members: []ast.EnumMember,
    },
    enum_member: struct {
        enum_name: []const u8,
        member_name: []const u8,
    },
    function_type: struct {
        param_types: []ast.Type,
        return_type: ast.Type,
    },

    pub fn fromLiteral(literal: ast.Literal) ComptimeValue {
        return switch (literal) {
            .integer => |int| ComptimeValue{ .integer = int.value },
            .float => |float| ComptimeValue{ .float = float.value },
            .bool_true => ComptimeValue{ .boolean = true },
            .bool_false => ComptimeValue{ .boolean = false },
            .string => |str| ComptimeValue{ .string = str.value },
            .char => |char| ComptimeValue{ .integer = @intCast(char.value) }, // Convert char to int
            .enum_member => |member| ComptimeValue{
                .enum_member = .{
                    .enum_name = "", // Will be inferred during semantic analysis
                    .member_name = member.name,
                },
            },
        };
    }

    pub fn toType(self: ComptimeValue, source_loc: ast.SourceLoc) ?ast.Type {
        return switch (self) {
            .type_value => |t| t,
            .struct_type => |s| ast.Type.initCustomStruct(s.name, s.fields, s.is_comptime, source_loc),
            .function_type => |f| {
                // TODO: Fix function type allocation properly
                // For now, return a simple primitive type to avoid memory leaks
                _ = f; // suppress unused parameter warning
                return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
            },
            else => null,
        };
    }

    pub fn isType(self: ComptimeValue) bool {
        return switch (self) {
            .type_value, .struct_type, .function_type => true,
            else => false,
        };
    }
};

// ============================================================================
// Modern Semantic Analyzer with Error Recovery
// ============================================================================

/// Symbol information for tracking variables, functions, and types
pub const Symbol = struct {
    name: []const u8,
    symbol_type: SymbolType,
    declared_type: ?ast.Type,
    inferred_type: ?ast.Type,
    source_loc: ast.SourceLoc,
    is_mutable: bool,
    is_used: bool,
    comptime_value: ?ComptimeValue, // For compile-time constants and types

    // Function-specific information
    function_params: ?[]ast.Parameter, // Parameters for function symbols

    const SymbolType = enum {
        variable,
        parameter,
        function,
        type_def,
        struct_def,
        comptime_value,
        @"const",
    };
};

/// Scope for symbol resolution
pub const Scope = struct {
    symbols: std.StringHashMap(Symbol),
    parent: ?*Scope,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) Scope {
        return Scope{
            .symbols = std.StringHashMap(Symbol).init(allocator),
            .parent = parent,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.symbols.deinit();
    }

    pub fn declare(self: *Scope, name: []const u8, symbol: Symbol) !void {
        if (self.symbols.contains(name)) {
            return error.DuplicateSymbol;
        }
        try self.symbols.put(name, symbol);
    }

    pub fn lookup(self: *const Scope, name: []const u8) ?Symbol {
        if (self.symbols.get(name)) |symbol| {
            return symbol;
        }

        if (self.parent) |parent| {
            return parent.lookup(name);
        }

        return null;
    }

    pub fn lookupLocal(self: *const Scope, name: []const u8) ?Symbol {
        return self.symbols.get(name);
    }
};

/// Type checker and semantic analyzer
pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    arena: *ast.AstArena,
    errors: *ErrorSystem.ErrorCollector,
    current_scope: *Scope,
    global_scope: Scope,
    file_path: []const u8,

    // Analysis state
    current_function_return_type: ?ast.Type,
    in_loop: bool,

    // Compile-time evaluation context
    type_registry: std.StringHashMap(ast.Type),
    struct_definitions: std.StringHashMap(ComptimeValue),
    comptime_values: std.StringHashMap(ComptimeValue),

    // Memory management for allocated types
    allocated_types: std.ArrayList(*ast.Type),

    pub fn init(
        allocator: std.mem.Allocator,
        arena: *ast.AstArena,
        errors: *ErrorSystem.ErrorCollector,
        file_path: []const u8,
    ) SemanticAnalyzer {
        var global_scope = Scope.init(allocator, null);
        return SemanticAnalyzer{
            .allocator = allocator,
            .arena = arena,
            .errors = errors,
            .current_scope = &global_scope,
            .global_scope = global_scope,
            .file_path = file_path,
            .current_function_return_type = null,
            .in_loop = false,
            .type_registry = std.StringHashMap(ast.Type).init(allocator),
            .struct_definitions = std.StringHashMap(ComptimeValue).init(allocator),
            .comptime_values = std.StringHashMap(ComptimeValue).init(allocator),
            .allocated_types = std.ArrayList(*ast.Type).init(allocator),
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        // Clean up all allocated types
        for (self.allocated_types.items) |type_ptr| {
            self.allocator.destroy(type_ptr);
        }
        self.allocated_types.deinit();

        self.global_scope.deinit();
        self.type_registry.deinit();
        self.struct_definitions.deinit();
        self.comptime_values.deinit();
    }

    /// Helper method to create and track allocated types
    fn createTrackedType(self: *SemanticAnalyzer, typ: ast.Type) !*ast.Type {
        const type_ptr = try self.allocator.create(ast.Type);
        type_ptr.* = typ;
        try self.allocated_types.append(type_ptr);
        return type_ptr;
    }

    /// Enter a new scope for symbol resolution
    fn enterScope(self: *SemanticAnalyzer) !*Scope {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.allocator, self.current_scope);
        self.current_scope = new_scope;
        return new_scope;
    }

    /// Exit the current scope and return to parent
    fn exitScope(self: *SemanticAnalyzer) void {
        if (self.current_scope.parent) |parent| {
            const old_scope = self.current_scope;
            self.current_scope = parent;
            old_scope.deinit();
            self.allocator.destroy(old_scope);
        }
    }

    // ============================================================================
    // Compile-time Evaluation
    // ============================================================================

    /// Evaluate an expression at compile time
    fn evaluateComptimeExpression(self: *SemanticAnalyzer, node_id: ast.NodeId) !?ComptimeValue {
        const node = self.arena.getNode(node_id) orelse return null;

        switch (node.data) {
            .literal => |literal| {
                return ComptimeValue.fromLiteral(literal);
            },

            .identifier => |ident| {
                // Check for compile-time values
                if (self.comptime_values.get(ident.name)) |value| {
                    return value;
                }

                // Check for type names
                if (self.type_registry.get(ident.name)) |typ| {
                    return ComptimeValue{ .type_value = typ };
                }

                // Check for struct definitions
                if (self.struct_definitions.get(ident.name)) |struct_value| {
                    return struct_value;
                }

                return null;
            },

            .struct_decl => |struct_decl| {
                return ComptimeValue{ .struct_type = .{
                    .name = struct_decl.name,
                    .fields = struct_decl.fields.items,
                    .is_comptime = struct_decl.is_comptime,
                } };
            },

            .type_expr => |_| {
                // Evaluate type expressions
                return self.evaluateTypeExpression(node_id);
            },

            .struct_type_expr => |_| {
                // Evaluate struct type expressions
                return self.evaluateStructTypeExpression(node_id);
            },

            else => return null,
        }
    }

    /// Evaluate a type expression at compile time
    fn evaluateTypeExpression(self: *SemanticAnalyzer, node_id: ast.NodeId) anyerror!?ComptimeValue {
        const node = self.arena.getNode(node_id) orelse return null;

        switch (node.data) {
            .type_expr => |type_expr| {
                // Evaluate the base type expression
                const base_value = try self.evaluateComptimeExpression(type_expr.base_type);
                if (base_value) |value| {
                    if (value.isType()) {
                        return value;
                    }
                }
                return null;
            },
            else => return null,
        }
    }

    /// Evaluate a struct type expression at compile time
    fn evaluateStructTypeExpression(self: *SemanticAnalyzer, node_id: ast.NodeId) !?ComptimeValue {
        const node = self.arena.getNode(node_id) orelse return null;

        switch (node.data) {
            .struct_type_expr => |struct_type| {
                return ComptimeValue{
                    .struct_type = .{
                        .name = "", // Anonymous struct
                        .fields = struct_type.fields.items,
                        .is_comptime = true,
                    },
                };
            },
            else => return null,
        }
    }

    /// Resolve a primitive type by name
    pub fn resolvePrimitiveType(self: *SemanticAnalyzer, name: []const u8) ?ast.PrimitiveType {
        _ = self; // Suppress unused parameter

        if (std.mem.eql(u8, name, "bool")) return .{ .bool = {} };
        if (std.mem.eql(u8, name, "i8")) return .{ .i8 = {} };
        if (std.mem.eql(u8, name, "i16")) return .{ .i16 = {} };
        if (std.mem.eql(u8, name, "i32")) return .{ .i32 = {} };
        if (std.mem.eql(u8, name, "i64")) return .{ .i64 = {} };
        if (std.mem.eql(u8, name, "isize")) return .{ .i64 = {} }; // Howl's isize maps to i64
        if (std.mem.eql(u8, name, "u8")) return .{ .u8 = {} };
        if (std.mem.eql(u8, name, "u16")) return .{ .u16 = {} };
        if (std.mem.eql(u8, name, "u32")) return .{ .u32 = {} };
        if (std.mem.eql(u8, name, "u64")) return .{ .u64 = {} };
        if (std.mem.eql(u8, name, "f32")) return .{ .f32 = {} };
        if (std.mem.eql(u8, name, "f64")) return .{ .f64 = {} };
        if (std.mem.eql(u8, name, "char")) return .{ .char = {} };
        if (std.mem.eql(u8, name, "str")) return .{ .string = {} }; // Howl's str maps to string
        if (std.mem.eql(u8, name, "string")) return .{ .string = {} };
        if (std.mem.eql(u8, name, "void")) return .{ .void = {} };
        if (std.mem.eql(u8, name, "type")) return .{ .type = {} };

        return null;
    }

    fn reportError(
        self: *SemanticAnalyzer,
        code: ErrorSystem.ErrorCode,
        message: []const u8,
        source_loc: ast.SourceLoc,
    ) !void {
        _ = try self.errors.createAndAddError(
            code,
            .semantic,
            .error_,
            message,
            source_loc.toSourceSpan(),
        );
    }

    fn reportWarning(
        self: *SemanticAnalyzer,
        code: ErrorSystem.ErrorCode,
        message: []const u8,
        source_loc: ast.SourceLoc,
    ) !void {
        _ = try self.errors.createAndAddError(
            code,
            .semantic,
            .warning,
            message,
            source_loc.toSourceSpan(),
        );
    }

    // ============================================================================
    // Type Inference and Checking
    // ============================================================================

    fn inferType(self: *SemanticAnalyzer, node_id: ast.NodeId) !?ast.Type {
        const node = self.arena.getNode(node_id) orelse return null;

        switch (node.data) {
            .literal => |literal| {
                return switch (literal) {
                    .integer => |int_lit| ast.Type.initPrimitive(int_lit.type_hint orelse .{ .i32 = {} }, node.source_loc),
                    .float => |float_lit| ast.Type.initPrimitive(float_lit.type_hint orelse .{ .f64 = {} }, node.source_loc),
                    .string => ast.Type.initPrimitive(.{ .str = {} }, node.source_loc),
                    .char => ast.Type.initPrimitive(.{ .char = {} }, node.source_loc),
                    .bool_true, .bool_false => ast.Type.initPrimitive(.{ .bool = {} }, node.source_loc),
                    .enum_member => |member| self.inferEnumMemberType(member, node.source_loc),
                };
            },

            .identifier => |ident| {
                // Special case for error union types like "!void"
                if (std.mem.eql(u8, ident.name, "!void")) {
                    return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
                }

                if (self.current_scope.lookup(ident.name)) |symbol| {
                    // Mark symbol as used
                    var mutable_symbol = symbol;
                    mutable_symbol.is_used = true;
                    try self.current_scope.symbols.put(ident.name, mutable_symbol);

                    return symbol.inferred_type orelse symbol.declared_type;
                } else {
                    // Check if it's a type name or comptime value
                    if (self.type_registry.get(ident.name)) |_| {
                        return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
                    }
                    if (self.struct_definitions.get(ident.name)) |_| {
                        return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
                    }

                    // Check for primitive types
                    if (self.resolvePrimitiveType(ident.name)) |prim_type| {
                        return ast.Type.initPrimitive(prim_type, node.source_loc);
                    }

                    const msg = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'", .{ident.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.undefined_variable, msg, node.source_loc);
                    return null;
                }
            },

            .binary_expr => |binary| {
                const left_type = try self.inferType(binary.left);
                const right_type = try self.inferType(binary.right);

                return self.checkBinaryOperation(binary.op, left_type, right_type, node.source_loc);
            },

            .unary_expr => |unary| {
                const operand_type = try self.inferType(unary.operand);
                return self.checkUnaryOperation(unary.op, operand_type, node.source_loc);
            },

            .call_expr => |call| {
                return self.checkFunctionCall(call.callee, call.args.items, node.source_loc);
            },

            .member_expr => |member| {
                return self.checkMemberAccess(member.object, member.field, node.source_loc);
            },

            .if_expr => |if_expr| {
                // Check condition is boolean
                const cond_type = try self.inferType(if_expr.condition);
                if (cond_type) |ct| {
                    if (!self.isBoolean(ct)) {
                        try self.reportError(.type_mismatch, "If condition must be boolean", node.source_loc);
                    }
                }

                // Both branches should have compatible types
                const then_type = try self.inferType(if_expr.then_branch);
                var result_type = then_type;

                if (if_expr.else_branch) |else_branch| {
                    const else_type = try self.inferType(else_branch);
                    if (then_type != null and else_type != null) {
                        if (!self.typesCompatible(then_type.?, else_type.?)) {
                            try self.reportError(.type_mismatch, "If-else branches have incompatible types", node.source_loc);
                            result_type = null;
                        }
                    }
                } else {
                    // If without else returns void/unit
                    result_type = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
                }

                return result_type;
            },

            .match_expr => |match_expr| {
                // Check each arm and ensure compatibility
                const match_expr_type = try self.inferType(match_expr.expression);

                var result_type: ?ast.Type = null;
                for (match_expr.arms.items) |arm| {
                    // Analyze the pattern against the matched expression type
                    try self.analyzeMatchPattern(arm.pattern, match_expr_type, arm.source_loc);
                    
                    if (arm.guard) |guard| {
                        const guard_type = try self.inferType(guard);
                        if (guard_type) |gt| {
                            if (!self.isBoolean(gt)) {
                                try self.reportError(.type_mismatch, "Match guard must be boolean", node.source_loc);
                            }
                        }
                    }

                    const arm_type = try self.inferType(arm.body);
                    if (result_type == null) {
                        result_type = arm_type;
                    } else if (arm_type != null and result_type != null) {
                        if (!self.typesCompatible(result_type.?, arm_type.?)) {
                            try self.reportError(.type_mismatch, "Match arms have incompatible types", node.source_loc);
                            result_type = null;
                        }
                    }
                }

                return result_type;
            },

            // Add missing cases with basic implementations
            .index_expr => {
                // TODO: implement array/slice indexing
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },

            .var_decl => {
                // Variable declarations don't have a type in expression context
                return null;
            },

            .function_decl => {
                // Function declarations don't have a type in expression context
                return null;
            },

            .extern_fn_decl => {
                // Extern function declarations don't have a type in expression context
                return null;
            },

            .struct_decl => {
                // Struct declarations represent the type itself
                return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
            },

            .enum_decl => {
                // Enum declarations represent the type itself
                return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
            },

            .type_decl => {
                // Type declarations represent the type itself
                return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
            },

            .import_decl => {
                // Import declarations don't have a return type in expressions
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },

            .while_expr => {
                // While expressions return void
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },

            .for_expr => {
                // For expressions return void
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },

            .block => {
                // TODO: implement block type inference (type of last expression)
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },

            .struct_init => |struct_init| {
                return self.checkStructInitialization(struct_init, node.source_loc);
            },

            .array_init => |array_init| {
                return self.checkArrayInitialization(array_init, node.source_loc);
            },

            .type_expr => {
                // Type expressions represent types
                return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
            },

            .struct_type_expr => {
                // Struct type expressions represent types
                return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
            },

            .generic_type_expr => |generic_type| {
                // Generic type expressions like List(T) represent types
                return self.checkGenericTypeInstantiation(generic_type, node.source_loc);
            },

            .return_stmt => {
                // Return statements don't have a type in expression context
                return null;
            },

            .break_stmt => {
                // Break statements don't have a type in expression context
                return null;
            },

            .continue_stmt => {
                // Continue statements don't have a type in expression context
                return null;
            },

            .error_node => {
                // Error nodes don't have a valid type
                return null;
            },

            .range_expr => |range_expr| {
                return self.analyzeRangeExpression(range_expr, node.source_loc);
            },

            .compile_target_expr => {
                // @compile.target evaluates to the current compile target (as an enum-like value)
                // For now, return a simple type
                return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
            },

            .compile_insert_expr => {
                // @compile.insert("code") doesn't return a value, it's a compile-time instruction
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },

            .match_compile_expr => |match_compile| {
                return self.analyzeMatchCompileExpression(match_compile, node.source_loc);
            },
        }
    }

    fn checkBinaryOperation(
        self: *SemanticAnalyzer,
        op: ast.BinaryOp,
        left_type: ?ast.Type,
        right_type: ?ast.Type,
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        if (left_type == null or right_type == null) return null;

        const left = left_type.?;
        const right = right_type.?;

        switch (op) {
            .add, .sub, .mul, .div, .mod => {
                if (left.isNumeric() and right.isNumeric()) {
                    // Return the "wider" type
                    if (left.isFloat() or right.isFloat()) {
                        return ast.Type.initPrimitive(.{ .f64 = {} }, source_loc);
                    } else {
                        return ast.Type.initPrimitive(.{ .i32 = {} }, source_loc);
                    }
                } else {
                    try self.reportError(.binary_operation_type_error, "Arithmetic operations require numeric types", source_loc);
                    return null;
                }
            },

            .concat => {
                if (left.isString() and right.isString()) {
                    return ast.Type.initPrimitive(.{ .str = {} }, source_loc);
                } else {
                    try self.reportError(.binary_operation_type_error, "String concatenation (++) requires both operands to be strings", source_loc);
                    return null;
                }
            },

            .eq, .ne => {
                if (self.typesCompatible(left, right)) {
                    return ast.Type.initPrimitive(.{ .bool = {} }, source_loc);
                } else {
                    try self.reportError(.binary_operation_type_error, "Equality comparison requires compatible types", source_loc);
                    return null;
                }
            },

            .lt, .le, .gt, .ge => {
                if (left.isNumeric() and right.isNumeric()) {
                    return ast.Type.initPrimitive(.{ .bool = {} }, source_loc);
                } else {
                    try self.reportError(.binary_operation_type_error, "Comparison operations require numeric types", source_loc);
                    return null;
                }
            },

            .logical_and, .logical_or => {
                if (self.isBoolean(left) and self.isBoolean(right)) {
                    return ast.Type.initPrimitive(.{ .bool = {} }, source_loc);
                } else {
                    try self.reportError(.binary_operation_type_error, "Logical operations require boolean types", source_loc);
                    return null;
                }
            },

            .assign => {
                if (self.typesCompatible(left, right)) {
                    return right;
                } else {
                    try self.reportError(.type_mismatch, "Assignment requires compatible types", source_loc);
                    return null;
                }
            },

            else => {
                // TODO: Handle other binary operations
                return null;
            },
        }
    }

    fn checkUnaryOperation(
        self: *SemanticAnalyzer,
        op: ast.UnaryOp,
        operand_type: ?ast.Type,
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        if (operand_type == null) return null;

        const operand = operand_type.?;

        switch (op) {
            .negate => {
                if (operand.isNumeric()) {
                    return operand;
                } else {
                    try self.reportError(.binary_operation_type_error, "Negation requires a numeric type", source_loc);
                    return null;
                }
            },

            .not => {
                if (self.isBoolean(operand)) {
                    return operand;
                } else {
                    try self.reportError(.binary_operation_type_error, "Logical not requires a boolean type", source_loc);
                    return null;
                }
            },

            else => {
                // TODO: Handle other unary operations
                return operand;
            },
        }
    }

    fn checkFunctionCall(
        self: *SemanticAnalyzer,
        callee_id: ast.NodeId,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        const callee_node = self.arena.getNode(callee_id);
        if (callee_node == null) return null;

        // For now, assume all function calls return i32
        // TODO: Implement proper function type checking

        switch (callee_node.?.data) {
            .identifier => |ident| {
                // Check for built-in functions first
                if (try self.handleBuiltinFunction(ident.name, args, source_loc)) |builtin_type| {
                    return builtin_type;
                }

                if (self.current_scope.lookup(ident.name)) |symbol| {
                    if (symbol.symbol_type == .function) {
                        // TODO: Check argument count and types
                        return symbol.declared_type;
                    } else {
                        try self.reportError(.invalid_function_call, "Variable is not callable", source_loc);
                        return null;
                    }
                } else {
                    const msg = try std.fmt.allocPrint(self.allocator, "Undefined function '{s}'", .{ident.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.undefined_variable, msg, source_loc);
                    return null;
                }
            },

            .member_expr => |member| {
                // Check for built-in module functions like std.debug.print
                const member_obj_node = self.arena.getNode(member.object);
                if (member_obj_node) |member_obj| {
                    switch (member_obj.data) {
                        .member_expr => |nested_member| {
                            const base_obj_node = self.arena.getNode(nested_member.object);
                            if (base_obj_node) |base_obj| {
                                if (base_obj.data == .identifier) {
                                    const base_ident = base_obj.data.identifier;
                                    // Handle std.debug.print
                                    if (std.mem.eql(u8, base_ident.name, "std") and
                                        std.mem.eql(u8, nested_member.field, "debug") and
                                        std.mem.eql(u8, member.field, "print"))
                                    {
                                        // std.debug.print returns void and accepts any number of arguments
                                        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
                                    }
                                }
                            }
                        },
                        .generic_type_expr => |generic_type| {
                            // Handle method calls on generic types like std.List(T).init()
                            const base_node = self.arena.getNode(generic_type.base_type);
                            if (base_node) |base| {
                                if (base.data == .member_expr) {
                                    const base_member = base.data.member_expr;
                                    // Check if this is std.List
                                    if (std.mem.eql(u8, base_member.field, "List")) {
                                        const std_obj_node = self.arena.getNode(base_member.object);
                                        if (std_obj_node) |std_obj| {
                                            if (std_obj.data == .identifier) {
                                                const ident = std_obj.data.identifier;
                                                if (std.mem.eql(u8, ident.name, "std")) {
                                                    // This is std.List(T).method()
                                                    if (std.mem.eql(u8, member.field, "init")) {
                                                        // std.List(T).init() returns a List instance
                                                        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
                                                    } else if (std.mem.eql(u8, member.field, "from")) {
                                                        // std.List(T).from(array) returns a List instance
                                                        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
                                                    } else if (std.mem.eql(u8, member.field, "initCapacity")) {
                                                        // std.List(T).initCapacity(n) returns a List instance
                                                        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }

                // Fall back to method lookup for member expressions
                // This handles cases like h.append(x) where h is a variable and append is a function
                const object_type = try self.inferType(member.object);
                if (object_type) |obj_type| {
                    if (self.findMethodsForType(obj_type, member.field)) |method_symbol| {
                        // Found a method! Return the method's return type
                        return method_symbol.declared_type;
                    }
                }

                // No method found, report error
                try self.reportError(.invalid_function_call, "Member expression is not callable", source_loc);
                return null;
            },

            else => {
                try self.reportError(.invalid_function_call, "Expression is not callable", source_loc);
                return null;
            },
        }
    }

    /// Look for functions where the first parameter matches the given type
    fn findMethodsForType(self: *SemanticAnalyzer, target_type: ast.Type, method_name: []const u8) ?Symbol {
        // First check for built-in List methods
        if (std.mem.eql(u8, method_name, "append")) {
            // Create a dummy symbol for the append method
            const default_loc = ast.SourceLoc{ .file_path = "<builtin>", .start_pos = 0, .end_pos = 0, .line = 0, .column = 0 };
            const symbol = Symbol{
                .name = "append",
                .symbol_type = .function,
                .declared_type = ast.Type.initPrimitive(.{ .void = {} }, default_loc),
                .inferred_type = null,
                .source_loc = default_loc,
                .is_mutable = false,
                .is_used = false,
                .comptime_value = null,
                .function_params = null,
            };
            return symbol;
        }

        // Check if target type is a custom struct and look for methods in its definition
        if (target_type.isCustomStruct()) {
            const struct_name = target_type.data.custom_struct.name;

            // Look up the struct definition
            if (self.struct_definitions.get(struct_name)) |struct_comptime_value| {
                // Search through struct fields for methods
                for (struct_comptime_value.struct_type.fields) |field| {
                    if (std.mem.eql(u8, field.name, method_name)) {
                        // Check if this field is a method (has function type)
                        if (field.type_annotation) |type_node_id| {
                            if (self.arena.getNode(type_node_id)) |type_node| {
                                if (type_node.data == .identifier) {
                                    const identifier = type_node.data.identifier;
                                    if (std.mem.eql(u8, identifier.name, "fn")) {
                                        // This is a method! Create a symbol for it
                                        const symbol = Symbol{
                                            .name = method_name,
                                            .symbol_type = .function,
                                            .declared_type = ast.Type.initPrimitive(.{ .i64 = {} }, target_type.source_loc), // TODO: proper return type
                                            .inferred_type = null,
                                            .source_loc = target_type.source_loc,
                                            .is_mutable = false,
                                            .is_used = false,
                                            .comptime_value = null,
                                            .function_params = null, // TODO: parse function parameters
                                        };
                                        return symbol;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        var scope: ?*Scope = self.current_scope;

        // Search through all scopes for functions
        while (scope) |current_scope| {
            var iterator = current_scope.symbols.iterator();
            while (iterator.next()) |entry| {
                const symbol = entry.value_ptr.*;

                // Only consider functions
                if (symbol.symbol_type != .function) continue;

                // Skip if name doesn't match
                if (!std.mem.eql(u8, symbol.name, method_name)) continue;

                // For now, assume any function with the right name could be a method
                // This is a simplified implementation that we can improve later
                return symbol;
            }
            scope = current_scope.parent;
        }

        return null;
    }

    /// Helper to get function parameters from a function symbol
    fn getFunctionParameters(self: *SemanticAnalyzer, symbol: Symbol) ?[]ast.Parameter {
        _ = self;
        return symbol.function_params;
    }

    /// Helper to get the type of a parameter
    fn getParameterType(self: *SemanticAnalyzer, param: ast.Parameter) ?ast.Type {
        if (param.type_annotation) |type_node| {
            return self.inferType(type_node) catch null;
        }
        return null;
    }

    fn checkMemberAccess(
        self: *SemanticAnalyzer,
        object_id: ast.NodeId,
        field_name: []const u8,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        // Special handling for built-in modules
        const object_node = self.arena.getNode(object_id);
        if (object_node) |obj_node| {
            switch (obj_node.data) {
                .identifier => |ident| {
                    // Handle std module
                    if (std.mem.eql(u8, ident.name, "std")) {
                        if (std.mem.eql(u8, field_name, "debug")) {
                            // Return a special type for std.debug module
                            return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
                        }
                    }
                },
                .member_expr => |member| {
                    // Handle nested member access like std.debug.print
                    const member_obj_node = self.arena.getNode(member.object);
                    if (member_obj_node) |member_obj| {
                        if (member_obj.data == .identifier) {
                            const base_ident = member_obj.data.identifier;
                            // Check for std.debug.print
                            if (std.mem.eql(u8, base_ident.name, "std") and
                                std.mem.eql(u8, member.field, "debug") and
                                std.mem.eql(u8, field_name, "print"))
                            {
                                // Return function type for print
                                return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
                            }
                        }
                    }
                },
                else => {},
            }
        }

        // Get the type of the object being accessed
        const obj_type = try self.inferType(object_id) orelse return null;

        // Handle member access based on object type
        switch (obj_type.data) {
            .@"struct" => |struct_info| {
                // Find the field in the struct
                for (struct_info.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        // Resolve the field's type
                        if (field.type_annotation) |type_node| {
                            return self.inferType(type_node);
                        }
                        return null;
                    }
                }

                // Check for methods
                if (self.findMethodsForType(obj_type, field_name)) |method_symbol| {
                    return method_symbol.declared_type;
                }

                const msg = try std.fmt.allocPrint(self.allocator, "Struct '{s}' has no field or method '{s}'", .{ struct_info.name, field_name });
                defer self.allocator.free(msg);
                try self.reportError(.invalid_member_access, msg, source_loc);
                return null;
            },

            .custom_struct => |custom_struct| {
                // Find the field in the custom struct
                for (custom_struct.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        // Resolve the field's type
                        if (field.type_annotation) |type_node| {
                            return self.inferType(type_node);
                        }
                        return null;
                    }
                }

                // Check for methods
                if (self.findMethodsForType(obj_type, field_name)) |method_symbol| {
                    return method_symbol.declared_type;
                }

                const msg = try std.fmt.allocPrint(self.allocator, "Struct '{s}' has no field or method '{s}'", .{ custom_struct.name, field_name });
                defer self.allocator.free(msg);
                try self.reportError(.invalid_member_access, msg, source_loc);
                return null;
            },

            else => {
                // Before reporting an error, check if this could be a method call
                // Look for functions where the first parameter matches the object type
                if (self.findMethodsForType(obj_type, field_name)) |method_symbol| {
                    // Found a method! Return the method's return type
                    return method_symbol.declared_type;
                }

                try self.reportError(.invalid_member_access, "Member access is only supported on struct types, or no matching method found", source_loc);
                return null;
            },
        }
    }

    fn analyzeEnumDeclaration(
        self: *SemanticAnalyzer,
        enum_decl: @TypeOf(@as(ast.AstNode, undefined).data.enum_decl),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Validate enum explicit values are sorted in ascending order
        try self.validateEnumValueOrder(enum_decl.members.items, source_loc);
        
        // Create the enum type
        const enum_type = ast.Type.initEnum(enum_decl.name, enum_decl.members.items, source_loc);

        // Register the enum type
        try self.type_registry.put(enum_decl.name, enum_type);

        // Create compile-time value for the enum
        const comptime_value = ComptimeValue{ .enum_type = .{
            .name = enum_decl.name,
            .members = enum_decl.members.items,
        } };

        // Declare enum symbol
        const symbol = Symbol{
            .name = enum_decl.name,
            .symbol_type = .type_def,
            .declared_type = enum_type,
            .inferred_type = null,
            .source_loc = source_loc,
            .is_mutable = false,
            .is_used = false,
            .comptime_value = comptime_value,
            .function_params = null,
        };

        self.current_scope.declare(enum_decl.name, symbol) catch |err| {
            switch (err) {
                error.DuplicateSymbol => {
                    const msg = try std.fmt.allocPrint(self.allocator, "Enum '{s}' is already declared", .{enum_decl.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.duplicate_declaration, msg, source_loc);
                },
                else => return err,
            }
        };

        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
    }

    fn analyzeVariableDeclaration(
        self: *SemanticAnalyzer,
        var_decl: @TypeOf(@as(ast.AstNode, undefined).data.var_decl),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        var declared_type: ?ast.Type = null;
        var inferred_type: ?ast.Type = null;

        // Check type annotation
        if (var_decl.type_annotation) |type_node_id| {
            declared_type = try self.resolveType(type_node_id);
        }

        // Infer type from initializer
        if (var_decl.initializer) |init_id| {
            inferred_type = try self.inferType(init_id);
        }

        // Check type compatibility
        if (declared_type != null and inferred_type != null) {
            if (!self.typesCompatible(declared_type.?, inferred_type.?)) {
                try self.reportError(.type_mismatch, "Variable initializer type doesn't match declared type", source_loc);
            }
        }

        // Final type is declared type or inferred type
        const final_type = declared_type orelse inferred_type;

        // Declare symbol
        const symbol = Symbol{
            .name = var_decl.name,
            .symbol_type = if (var_decl.is_mutable) .variable else .@"const",
            .declared_type = declared_type,
            .inferred_type = inferred_type,
            .source_loc = source_loc,
            .is_mutable = var_decl.is_mutable,
            .is_used = false,
            .comptime_value = null,
            .function_params = null,
        };

        self.current_scope.declare(var_decl.name, symbol) catch |err| {
            switch (err) {
                error.DuplicateSymbol => {
                    const msg = try std.fmt.allocPrint(self.allocator, "Variable '{s}' is already declared in this scope", .{var_decl.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.duplicate_declaration, msg, source_loc);
                },
                else => return err,
            }
        };

        return final_type;
    }

    fn analyzeFunctionDeclaration(
        self: *SemanticAnalyzer,
        func_decl: @TypeOf(@as(ast.AstNode, undefined).data.function_decl),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Create function scope
        const func_scope = try self.enterScope();
        defer self.exitScope();
        _ = func_scope;

        // Process parameters
        for (func_decl.params.items) |param| {
            // Add parameter to function scope
            var param_type: ?ast.Type = null;
            if (param.type_annotation) |type_node| {
                param_type = try self.inferType(type_node);
            }

            const param_symbol = Symbol{
                .name = param.name,
                .symbol_type = .parameter,
                .declared_type = param_type,
                .inferred_type = null,
                .source_loc = param.source_loc,
                .is_mutable = false,
                .is_used = false,
                .comptime_value = null,
                .function_params = null,
            };

            try self.current_scope.declare(param.name, param_symbol);
        }

        // Process return type
        var return_type = ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        if (func_decl.return_type) |return_type_node| {
            if (try self.inferType(return_type_node)) |inferred_return_type| {
                return_type = inferred_return_type;
            }
        }

        // Set current function context
        const old_return_type = self.current_function_return_type;
        self.current_function_return_type = return_type;
        defer self.current_function_return_type = old_return_type;

        // Analyze function body
        _ = try self.analyzeNode(func_decl.body);

        // Declare function symbol in parent scope
        if (self.current_scope.parent) |parent_scope| {
            const symbol = Symbol{
                .name = func_decl.name,
                .symbol_type = .function,
                .declared_type = return_type,
                .inferred_type = null,
                .source_loc = source_loc,
                .is_mutable = false,
                .is_used = false,
                .comptime_value = null,
                .function_params = func_decl.params.items, // Store parameters for method lookup
            };

            parent_scope.declare(func_decl.name, symbol) catch |err| {
                switch (err) {
                    error.DuplicateSymbol => {
                        const msg = try std.fmt.allocPrint(self.allocator, "Function '{s}' is already declared", .{func_decl.name});
                        defer self.allocator.free(msg);
                        try self.reportError(.duplicate_declaration, msg, source_loc);
                    },
                    else => return err,
                }
            };
        }

        return return_type;
    }

    fn analyzeExternFunctionDeclaration(
        self: *SemanticAnalyzer,
        extern_fn_decl: @TypeOf(@as(ast.AstNode, undefined).data.extern_fn_decl),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Process parameters (same as regular functions)
        for (extern_fn_decl.params.items) |param| {
            var param_type: ?ast.Type = null;
            if (param.type_annotation) |type_node| {
                param_type = try self.inferType(type_node);
            }
            // Note: extern function parameters aren't added to any scope since
            // they're only used for compile-time code generation
        }

        // Process return type
        var return_type = ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        if (extern_fn_decl.return_type) |return_type_node| {
            if (try self.inferType(return_type_node)) |inferred_return_type| {
                return_type = inferred_return_type;
            }
        }

        // Analyze the compile-time body
        _ = try self.analyzeNode(extern_fn_decl.compile_time_body);

        // Declare extern function symbol
        const symbol = Symbol{
            .name = extern_fn_decl.name,
            .symbol_type = .function,
            .declared_type = return_type,
            .inferred_type = null,
            .source_loc = source_loc,
            .is_mutable = false,
            .is_used = false,
            .comptime_value = null,
            .function_params = extern_fn_decl.params.items,
        };

        self.current_scope.declare(extern_fn_decl.name, symbol) catch |err| {
            switch (err) {
                error.DuplicateSymbol => {
                    const msg = try std.fmt.allocPrint(self.allocator, "Extern function '{s}' is already declared", .{extern_fn_decl.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.duplicate_declaration, msg, source_loc);
                },
                else => return err,
            }
        };

        return return_type;
    }

    fn analyzeStructDeclaration(
        self: *SemanticAnalyzer,
        struct_decl: @TypeOf(@as(ast.AstNode, undefined).data.struct_decl),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Create the struct type
        const struct_type = ast.Type.initCustomStruct(struct_decl.name, struct_decl.fields.items, struct_decl.is_comptime, source_loc);

        // Register the struct type
        try self.type_registry.put(struct_decl.name, struct_type);

        // Create compile-time value for the struct
        const comptime_value = ComptimeValue{ .struct_type = .{
            .name = struct_decl.name,
            .fields = struct_decl.fields.items,
            .is_comptime = struct_decl.is_comptime,
        } };
        try self.struct_definitions.put(struct_decl.name, comptime_value);

        // Declare struct symbol
        const symbol = Symbol{
            .name = struct_decl.name,
            .symbol_type = .struct_def,
            .declared_type = struct_type,
            .inferred_type = null,
            .source_loc = source_loc,
            .is_mutable = false,
            .is_used = false,
            .comptime_value = comptime_value,
            .function_params = null,
        };

        self.current_scope.declare(struct_decl.name, symbol) catch |err| {
            switch (err) {
                error.DuplicateSymbol => {
                    const msg = try std.fmt.allocPrint(self.allocator, "Struct '{s}' is already declared", .{struct_decl.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.duplicate_declaration, msg, source_loc);
                },
                else => return err,
            }
        };

        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
    }

    fn analyzeTypeDeclaration(
        self: *SemanticAnalyzer,
        type_decl: @TypeOf(@as(ast.AstNode, undefined).data.type_decl),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Evaluate the type expression at compile time
        const comptime_value = try self.evaluateComptimeExpression(type_decl.type_expr);
        if (comptime_value == null or !comptime_value.?.isType()) {
            try self.reportError(.invalid_declaration, "Type declaration must evaluate to a type", source_loc);
            return null;
        }

        const resolved_type = comptime_value.?.toType(source_loc);
        if (resolved_type == null) {
            try self.reportError(.invalid_declaration, "Could not resolve type expression", source_loc);
            return null;
        }

        // Register the type
        try self.type_registry.put(type_decl.name, resolved_type.?);
        try self.comptime_values.put(type_decl.name, comptime_value.?);

        // Declare type symbol
        const symbol = Symbol{
            .name = type_decl.name,
            .symbol_type = .type_def,
            .declared_type = resolved_type,
            .inferred_type = null,
            .source_loc = source_loc,
            .is_mutable = false,
            .is_used = false,
            .comptime_value = comptime_value,
            .function_params = null,
        };

        self.current_scope.declare(type_decl.name, symbol) catch |err| {
            switch (err) {
                error.DuplicateSymbol => {
                    const msg = try std.fmt.allocPrint(self.allocator, "Type '{s}' is already declared", .{type_decl.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.duplicate_declaration, msg, source_loc);
                },
                else => return err,
            }
        };

        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
    }

    fn analyzeImportDeclaration(
        self: *SemanticAnalyzer,
        import_decl: @TypeOf(@as(ast.AstNode, undefined).data.import_decl),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // For now, just validate that the module path is a valid string
        // In a full implementation, this would:
        // 1. Resolve the module path
        // 2. Load and compile the imported module
        // 3. Add its exported symbols to current scope

        _ = self; // Suppress unused warning
        _ = import_decl.module_path; // Suppress unused warning for now

        // TODO: Implement actual module loading and symbol importing
        // For now, just return void since imports don't return values
        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    fn analyzeReturnStatement(
        self: *SemanticAnalyzer,
        return_stmt: @TypeOf(@as(ast.AstNode, undefined).data.return_stmt),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        if (self.current_function_return_type == null) {
            try self.reportError(.invalid_statement, "Return statement outside of function", source_loc);
            return null;
        }

        const expected_type = self.current_function_return_type.?;

        if (return_stmt.value) |value_id| {
            const value_type = try self.inferType(value_id);
            if (value_type) |vt| {
                if (!self.typesCompatible(expected_type, vt)) {
                    try self.reportError(.return_type_mismatch, "Return value type doesn't match function return type", source_loc);
                }
            }
        } else {
            // Bare return - check if function expects void
            if (!self.isVoid(expected_type)) {
                try self.reportError(.missing_return_value, "Function expects a return value", source_loc);
            }
        }

        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    fn isVoid(self: *SemanticAnalyzer, typ: ast.Type) bool {
        _ = self;
        return switch (typ.data) {
            .primitive => |prim| prim == .void,
            else => false,
        };
    }

    fn isBoolean(self: *SemanticAnalyzer, typ: ast.Type) bool {
        _ = self;
        return switch (typ.data) {
            .primitive => |prim| prim == .bool,
            else => false,
        };
    }

    /// Analyze for loop expression with captures
    fn analyzeForExpression(
        self: *SemanticAnalyzer,
        for_expr: @TypeOf(@as(ast.AstNode, undefined).data.for_expr),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Create a new scope for the for loop
        const scope = try self.enterScope();
        defer self.exitScope();
        _ = scope;

        // Analyze the iterable expression
        const iterable_type = try self.inferType(for_expr.iterable);

        // Determine what we're iterating over and infer capture types
        if (iterable_type) |_| {
            // TODO: Add proper support for different iterable types (arrays, ranges, etc.)
            // For now, assume we're iterating over arrays

            // Declare capture variables in the new scope
            for (for_expr.captures.items) |capture| {
                if (!std.mem.eql(u8, capture.name, "_")) { // Don't declare ignored captures
                    var capture_type: ast.Type = undefined;

                    switch (capture.capture_type) {
                        .value => {
                            // For array iteration, the value type is the element type
                            // TODO: Extract actual element type from array type
                            capture_type = ast.Type.initPrimitive(.{ .i32 = {} }, source_loc); // Placeholder
                        },
                        .index => {
                            // Index is always usize
                            capture_type = ast.Type.initPrimitive(.{ .u64 = {} }, source_loc); // usize -> u64
                        },
                        .ignored => continue, // Skip ignored captures
                    }

                    // Declare the capture variable
                    const symbol = Symbol{
                        .name = capture.name,
                        .symbol_type = .@"const", // Captures are immutable
                        .declared_type = capture_type,
                        .inferred_type = capture_type,
                        .source_loc = capture.source_loc,
                        .is_mutable = false,
                        .is_used = false, // Will be marked as used if accessed
                        .comptime_value = null,
                        .function_params = null,
                    };

                    self.current_scope.declare(capture.name, symbol) catch |err| {
                        if (err == error.DuplicateSymbol) {
                            const msg = try std.fmt.allocPrint(self.allocator, "Capture variable '{s}' already declared", .{capture.name});
                            defer self.allocator.free(msg);
                            try self.reportError(.duplicate_declaration, msg, capture.source_loc);
                        } else {
                            return err;
                        }
                    };
                }
            }
        }

        // Analyze the loop body
        _ = try self.analyzeNode(for_expr.body);

        // For loops return void
        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    /// Analyze range expression (e.g., 0..=10, ..<arr.length)
    fn analyzeRangeExpression(
        self: *SemanticAnalyzer,
        range_expr: @TypeOf(@as(ast.AstNode, undefined).data.range_expr),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Analyze start expression if present
        if (range_expr.start) |start_id| {
            const start_type = try self.inferType(start_id);
            if (start_type) |typ| {
                // Ensure start is numeric
                if (!self.isNumericType(typ)) {
                    try self.reportError(.type_mismatch, "Range start must be numeric", source_loc);
                }
            }
        }

        // Analyze end expression if present
        if (range_expr.end) |end_id| {
            const end_type = try self.inferType(end_id);
            if (end_type) |typ| {
                // Ensure end is numeric
                if (!self.isNumericType(typ)) {
                    try self.reportError(.type_mismatch, "Range end must be numeric", source_loc);
                }
            }
        }

        // Range expressions represent an iterable of integers
        // Return a special range type that can be iterated over
        const element_type_ptr = try self.createTrackedType(ast.Type.initPrimitive(.{ .u64 = {} }, source_loc));

        return ast.Type{
            .data = .{
                .array = .{
                    .element_type = element_type_ptr,
                    .size = 0, // Dynamic size for ranges
                },
            },
            .source_loc = source_loc,
        };
    }

    /// Get the type of a literal value
    fn getLiteralType(self: *SemanticAnalyzer, literal: ast.Literal, source_loc: ast.SourceLoc) ?ast.Type {
        return switch (literal) {
            .integer => |int_lit| ast.Type.initPrimitive(int_lit.type_hint orelse .{ .i32 = {} }, source_loc),
            .float => |float_lit| ast.Type.initPrimitive(float_lit.type_hint orelse .{ .f64 = {} }, source_loc),
            .string => ast.Type.initPrimitive(.{ .str = {} }, source_loc),
            .char => ast.Type.initPrimitive(.{ .char = {} }, source_loc),
            .bool_true, .bool_false => ast.Type.initPrimitive(.{ .bool = {} }, source_loc),
            .enum_member => |member| self.inferEnumMemberType(member, source_loc) catch null,
        };
    }

    /// Resolve a type from a type annotation node
    fn resolveType(self: *SemanticAnalyzer, type_node_id: ast.NodeId) !?ast.Type {
        const type_node = self.arena.getNode(type_node_id) orelse return null;
        
        switch (type_node.data) {
            .identifier => |ident| {
                // Check if it's a primitive type
                if (self.resolvePrimitiveType(ident.name)) |prim_type| {
                    return ast.Type.initPrimitive(prim_type, type_node.source_loc);
                }
                
                // Check if it's a registered enum type
                if (self.type_registry.get(ident.name)) |enum_type| {
                    return enum_type;
                }
                
                // Check if it's a struct type
                if (self.struct_definitions.get(ident.name)) |struct_info| {
                    // Get the fields from the struct definition
                    const fields = struct_info.struct_type.fields;
                    const is_comptime = false; // Default to runtime struct
                    return ast.Type.initCustomStruct(ident.name, fields, is_comptime, type_node.source_loc);
                }
                
                const msg = try std.fmt.allocPrint(self.allocator, "Unknown type '{s}'", .{ident.name});
                defer self.allocator.free(msg);
                try self.reportError(.undefined_variable, msg, type_node.source_loc);
                return null;
            },
            else => {
                try self.reportError(.invalid_declaration, "Invalid type annotation", type_node.source_loc);
                return null;
            }
        }
    }

    /// Analyze a match pattern and check it against the matched expression type
    fn analyzeMatchPattern(
        self: *SemanticAnalyzer, 
        pattern: ast.MatchPattern, 
        match_expr_type: ?ast.Type,
        source_loc: ast.SourceLoc
    ) !void {
        switch (pattern) {
            .wildcard => {
                // Wildcard matches anything
            },
            .literal => |literal| {
                // Check if the literal is compatible with the matched expression type
                const literal_type = self.getLiteralType(literal, source_loc);
                if (match_expr_type) |expr_type| {
                    if (literal_type) |lit_type| {
                        if (!self.typesCompatible(expr_type, lit_type)) {
                            try self.reportError(.type_mismatch, "Literal pattern type doesn't match expression type", source_loc);
                        }
                    }
                }
            },
            .identifier => |_| {
                // Variable binding - always valid
            },
            .enum_member => |member_name| {
                // Check if the matched expression type is an enum and has this member
                if (match_expr_type) |expr_type| {
                    switch (expr_type.data) {
                        .@"enum" => |enum_info| {
                            // Validate that the enum has this member
                            var found = false;
                            for (enum_info.members) |member| {
                                if (std.mem.eql(u8, member.name, member_name)) {
                                    found = true;
                                    break;
                                }
                            }
                            if (!found) {
                                const msg = try std.fmt.allocPrint(self.allocator, "Enum member '{s}' does not exist in enum '{s}'", .{member_name, enum_info.name});
                                defer self.allocator.free(msg);
                                try self.reportError(.undefined_variable, msg, source_loc);
                            }
                        },
                        else => {
                            try self.reportError(.type_mismatch, "Enum member pattern can only be used with enum types", source_loc);
                        }
                    }
                } else {
                    try self.reportError(.undefined_variable, "Cannot infer enum type for member pattern", source_loc);
                }
            },
            .range => {
                // TODO: Implement range pattern validation
            },
            .tuple => {
                // TODO: Implement tuple pattern validation
            },
            .array => {
                // TODO: Implement array pattern validation
            },
            .guard => {
                // TODO: Implement guard pattern validation
            },
        }
    }

    /// Analyze compile-time match expression (match @compile.target)
    fn analyzeMatchCompileExpression(
        self: *SemanticAnalyzer,
        match_compile: @TypeOf(@as(ast.AstNode, undefined).data.match_compile_expr),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Verify the target expression is @compile.target
        _ = try self.inferType(match_compile.target_expr);

        // Analyze each arm's body
        var result_type: ?ast.Type = null;
        for (match_compile.arms.items) |arm| {
            const arm_type = try self.inferType(arm.body);

            // For now, just use the first arm's type as the result type
            // TODO: Proper type unification of all arms
            if (result_type == null) {
                result_type = arm_type;
            }
        }

        return result_type orelse ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    /// Check if a type is numeric (integer or float)
    fn isNumericType(self: *SemanticAnalyzer, typ: ast.Type) bool {
        _ = self; // unused parameter
        return switch (typ.data) {
            .primitive => |prim| switch (prim) {
                .i8, .i16, .i32, .i64, .isize, .u8, .u16, .u32, .u64, .usize, .f32, .f64 => true,
                else => false,
            },
            else => false,
        };
    }

    /// Check if two types are compatible
    fn typesCompatible(self: *SemanticAnalyzer, type1: ast.Type, type2: ast.Type) bool {
        _ = self; // unused parameter
        
        // Handle same types
        if (std.meta.eql(type1, type2)) return true;
        
        // Handle numeric type compatibility
        return switch (type1.data) {
            .primitive => |prim1| switch (type2.data) {
                .primitive => |prim2| switch (prim1) {
                    // Integers can be compatible with other integers
                    .i8, .i16, .i32, .i64, .isize => switch (prim2) {
                        .i8, .i16, .i32, .i64, .isize => true,
                        else => false,
                    },
                    .u8, .u16, .u32, .u64, .usize => switch (prim2) {
                        .u8, .u16, .u32, .u64, .usize => true,
                        else => false,
                    },
                    // Floats can be compatible with other floats
                    .f32, .f64 => switch (prim2) {
                        .f32, .f64 => true,
                        else => false,
                    },
                    // Strings are compatible with other strings
                    .str => switch (prim2) {
                        .str => true,
                        else => false,
                    },
                    // Booleans are compatible with other booleans
                    .bool => switch (prim2) {
                        .bool => true,
                        else => false,
                    },
                    // Characters are compatible with other characters
                    .char => switch (prim2) {
                        .char => true,
                        else => false,
                    },
                    else => false,
                },
                else => false,
            },
            else => false,
        };
    }

    /// Validate that enum explicit values are in ascending order
    fn validateEnumValueOrder(self: *SemanticAnalyzer, members: []ast.EnumMember, source_loc: ast.SourceLoc) !void {
        _ = source_loc; // unused parameter
        var last_value: ?i64 = null;
        
        for (members) |member| {
            if (member.value) |value_node| {
                // Evaluate the explicit value at compile time
                const value = try self.evaluateCompileTimeInteger(value_node);
                if (value) |int_value| {
                    if (last_value) |last| {
                        if (int_value <= last) {
                            const msg = try std.fmt.allocPrint(
                                self.allocator, 
                                "Enum member '{s}' has value {d} which is not greater than previous value {d}. Enum explicit values must be in ascending order.", 
                                .{ member.name, int_value, last }
                            );
                            defer self.allocator.free(msg);
                            try self.reportError(.invalid_enum_value, msg, member.source_loc);
                            return;
                        }
                    }
                    last_value = int_value;
                } else {
                    // If we can't evaluate the value as a compile-time integer, report an error
                    const msg = try std.fmt.allocPrint(
                        self.allocator,
                        "Enum member '{s}' has non-constant explicit value. Enum values must be compile-time integer constants.",
                        .{member.name}
                    );
                    defer self.allocator.free(msg);
                    try self.reportError(.invalid_enum_value, msg, member.source_loc);
                    return;
                }
            }
            // If no explicit value, continue (implicit values will be auto-assigned)
        }
    }

    /// Evaluate an AST node as a compile-time integer constant
    fn evaluateCompileTimeInteger(self: *SemanticAnalyzer, node_id: ast.NodeId) !?i64 {
        const node = self.arena.getNode(node_id) orelse return null;
        
        switch (node.data) {
            .literal => |literal| {
                switch (literal) {
                    .integer => |int_lit| return int_lit.value,
                    else => return null, // Not an integer literal
                }
            },
            else => return null, // Not a simple literal - more complex expressions not supported yet
        }
    }

    /// Check struct initialization and return the struct type
    fn checkStructInitialization(
        self: *SemanticAnalyzer,
        struct_init: @TypeOf(@as(ast.AstNode, undefined).data.struct_init),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        // Get the struct type name
        if (struct_init.type_name) |type_name| {
            // Look up the struct type
            if (self.type_registry.get(type_name)) |struct_type| {
                // TODO: Validate field initialization
                return struct_type;
            } else {
                const msg = try std.fmt.allocPrint(self.allocator, "Unknown struct type '{s}'", .{type_name});
                defer self.allocator.free(msg);
                try self.reportError(.undefined_variable, msg, source_loc);
                return null;
            }
        } else {
            try self.reportError(.type_mismatch, "Expected struct type for initialization", source_loc);
            return null;
        }
    }

    /// Check array initialization and return the array type
    fn checkArrayInitialization(
        self: *SemanticAnalyzer,
        array_init: @TypeOf(@as(ast.AstNode, undefined).data.array_init),
        source_loc: ast.SourceLoc,
    ) !?ast.Type {
        _ = self; // unused parameter
        _ = array_init; // TODO: Implement array type checking
        
        // For now, return a generic array type
        // TODO: Infer element type from elements
        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
    }

    // ============================================================================
    // Built-in Function System
    // ============================================================================

    /// Handle built-in functions like @import, @TypeOf, @sizeOf, etc.
    fn handleBuiltinFunction(
        self: *SemanticAnalyzer,
        name: []const u8,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        // Module system
        if (std.mem.eql(u8, name, "import")) {
            return self.handleImportBuiltin(args, source_loc);
        }

        // Type introspection
        if (std.mem.eql(u8, name, "TypeOf")) {
            return self.handleTypeOfBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "sizeOf")) {
            return self.handleSizeOfBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "alignOf")) {
            return self.handleAlignOfBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "offsetOf")) {
            return self.handleOffsetOfBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "FieldType")) {
            return self.handleFieldTypeBuiltin(args, source_loc);
        }

        // SIMD and vectors
        if (std.mem.eql(u8, name, "Vector")) {
            return self.handleVectorBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "splat")) {
            return self.handleSplatBuiltin(args, source_loc);
        }

        // Safe arithmetic
        if (std.mem.eql(u8, name, "add_s")) {
            return self.handleSafeArithmeticBuiltin("add", args, source_loc);
        }
        if (std.mem.eql(u8, name, "sub_s")) {
            return self.handleSafeArithmeticBuiltin("sub", args, source_loc);
        }
        if (std.mem.eql(u8, name, "mul_s")) {
            return self.handleSafeArithmeticBuiltin("mul", args, source_loc);
        }
        if (std.mem.eql(u8, name, "div_s")) {
            return self.handleSafeArithmeticBuiltin("div", args, source_loc);
        }

        // Type casting
        if (std.mem.eql(u8, name, "castUp")) {
            return self.handleCastUpBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "castDown")) {
            return self.handleCastDownBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "truncate")) {
            return self.handleTruncateBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "intCast")) {
            return self.handleIntCastBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "floatCast")) {
            return self.handleFloatCastBuiltin(args, source_loc);
        }

        // Compilation control
        if (std.mem.eql(u8, name, "panic")) {
            return self.handlePanicBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "compileError")) {
            return self.handleCompileErrorBuiltin(args, source_loc);
        }

        // Memory operations
        if (std.mem.eql(u8, name, "memcpy")) {
            return self.handleMemcpyBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "memset")) {
            return self.handleMemsetBuiltin(args, source_loc);
        }

        // Bit operations
        if (std.mem.eql(u8, name, "clz")) {
            return self.handleBitCountBuiltin("clz", args, source_loc);
        }
        if (std.mem.eql(u8, name, "ctz")) {
            return self.handleBitCountBuiltin("ctz", args, source_loc);
        }
        if (std.mem.eql(u8, name, "popCount")) {
            return self.handleBitCountBuiltin("popCount", args, source_loc);
        }

        // ArrayList builtin functions
        if (std.mem.eql(u8, name, "ArrayList")) {
            return self.handleArrayListBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "arrayListInit")) {
            return self.handleArrayListInitBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "arrayListAppend")) {
            return self.handleArrayListAppendBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "arrayListGet")) {
            return self.handleArrayListGetBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "arrayListLen")) {
            return self.handleArrayListLenBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "arrayListClear")) {
            return self.handleArrayListClearBuiltin(args, source_loc);
        }

        // Unknown builtin function - return null instead of reporting error
        // This allows the caller to handle it as a regular function call
        return null;
    }

    /// Handle @import(path) builtin
    fn handleImportBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@import expects exactly one argument", source_loc);
            return null;
        }

        // For now, return a module type placeholder
        // TODO: Implement actual module loading and type resolution
        return ast.Type.initPrimitive(.{ .module = {} }, source_loc);
    }

    /// Handle @TypeOf(expr) builtin
    fn handleTypeOfBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@TypeOf expects exactly one argument", source_loc);
            return null;
        }

        // Analyze the argument to get its type
        const arg_type = try self.analyzeNode(args[0]);
        if (arg_type == null) {
            return null;
        }

        // @TypeOf returns a type, which itself has type 'type'
        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
    }

    /// Handle @sizeOf(T) builtin
    fn handleSizeOfBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@sizeOf expects exactly one argument", source_loc);
            return null;
        }

        // Argument should be a type
        const arg_type = try self.analyzeNode(args[0]);
        if (arg_type == null) {
            return null;
        }

        // @sizeOf returns a usize (platform-dependent unsigned integer)
        return ast.Type.initPrimitive(.{ .usize = {} }, source_loc);
    }

    /// Handle @alignOf(T) builtin
    fn handleAlignOfBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@alignOf expects exactly one argument", source_loc);
            return null;
        }

        // Argument should be a type
        const arg_type = try self.analyzeNode(args[0]);
        if (arg_type == null) {
            return null;
        }

        // @alignOf returns a usize
        return ast.Type.initPrimitive(.{ .usize = {} }, source_loc);
    }

    /// Handle @offsetOf(StructType, "field") builtin
    fn handleOffsetOfBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@offsetOf expects exactly two arguments", source_loc);
            return null;
        }

        // First argument should be a struct type
        const struct_type = try self.analyzeNode(args[0]);
        if (struct_type == null) {
            return null;
        }

        // Second argument should be a string literal (field name)
        const field_name_type = try self.analyzeNode(args[1]);
        if (field_name_type == null) {
            return null;
        }

        // @offsetOf returns a usize
        return ast.Type.initPrimitive(.{ .usize = {} }, source_loc);
    }

    /// Handle @FieldType(StructType, "field") builtin
    fn handleFieldTypeBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@FieldType expects exactly two arguments", source_loc);
            return null;
        }

        // First argument should be a struct type
        const struct_type = try self.analyzeNode(args[0]);
        if (struct_type == null) {
            return null;
        }

        // Second argument should be a string literal (field name)
        const field_name_type = try self.analyzeNode(args[1]);
        if (field_name_type == null) {
            return null;
        }

        // @FieldType returns a type
        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
    }

    /// Handle @Vector(len, T) builtin
    fn handleVectorBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@Vector expects exactly two arguments", source_loc);
            return null;
        }

        // First argument should be a compile-time integer (length)
        const len_type = try self.analyzeNode(args[0]);
        if (len_type == null) {
            return null;
        }

        // Second argument should be a type
        const element_type = try self.analyzeNode(args[1]);
        if (element_type == null) {
            return null;
        }

        // @Vector returns a vector type
        // TODO: Implement proper vector type in AST
        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
    }

    /// Handle @splat(VectorType, value) builtin
    fn handleSplatBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@splat expects exactly two arguments", source_loc);
            return null;
        }

        // First argument should be a vector type
        const vector_type = try self.analyzeNode(args[0]);
        if (vector_type == null) {
            return null;
        }

        // Second argument should be a value compatible with the vector element type
        const value_type = try self.analyzeNode(args[1]);
        if (value_type == null) {
            return null;
        }

        // @splat returns the vector type
        return vector_type;
    }

    /// Handle safe arithmetic builtins like @add_s, @sub_s, etc.
    fn handleSafeArithmeticBuiltin(
        self: *SemanticAnalyzer,
        operation: []const u8,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            const msg = try std.fmt.allocPrint(self.allocator, "@{s}_s expects exactly two arguments", .{operation});
            defer self.allocator.free(msg);
            try self.reportError(.invalid_function_call, msg, source_loc);
            return null;
        }

        // Both arguments should be numeric types
        const left_type = try self.analyzeNode(args[0]);
        const right_type = try self.analyzeNode(args[1]);

        if (left_type == null or right_type == null) {
            return null;
        }

        // TODO: Implement proper type compatibility checking
        // For now, return the left operand type
        return left_type;
    }

    /// Handle @castUp(TargetType, value) builtin
    fn handleCastUpBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@castUp expects exactly two arguments", source_loc);
            return null;
        }

        // First argument is the target type
        const target_type = try self.analyzeNode(args[0]);
        if (target_type == null) {
            return null;
        }

        // Second argument is the value to cast
        const value_type = try self.analyzeNode(args[1]);
        if (value_type == null) {
            return null;
        }

        // @castUp returns the target type
        return target_type;
    }

    /// Handle @castDown(TargetType, value) builtin
    fn handleCastDownBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@castDown expects exactly two arguments", source_loc);
            return null;
        }

        // First argument is the target type
        const target_type = try self.analyzeNode(args[0]);
        if (target_type == null) {
            return null;
        }

        // Second argument is the value to cast
        const value_type = try self.analyzeNode(args[1]);
        if (value_type == null) {
            return null;
        }

        // @castDown returns an error union of the target type
        // TODO: Implement proper error union types
        return target_type;
    }

    /// Handle @truncate(TargetType, value) builtin
    fn handleTruncateBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@truncate expects exactly two arguments", source_loc);
            return null;
        }

        // First argument is the target type
        const target_type = try self.analyzeNode(args[0]);
        if (target_type == null) {
            return null;
        }

        // Second argument is the value to truncate
        const value_type = try self.analyzeNode(args[1]);
        if (value_type == null) {
            return null;
        }

        // @truncate returns the target type
        return target_type;
    }

    /// Handle @intCast(TargetType, value) builtin
    fn handleIntCastBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@intCast expects exactly two arguments", source_loc);
            return null;
        }

        // First argument is the target type
        const target_type = try self.analyzeNode(args[0]);
        if (target_type == null) {
            return null;
        }

        // Second argument is the value to cast
        const value_type = try self.analyzeNode(args[1]);
        if (value_type == null) {
            return null;
        }

        // @intCast returns an error union of the target type
        // TODO: Implement proper error union types
        return target_type;
    }

    /// Handle @floatCast(TargetType, value) builtin
    fn handleFloatCastBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@floatCast expects exactly two arguments", source_loc);
            return null;
        }

        // First argument is the target type
        const target_type = try self.analyzeNode(args[0]);
        if (target_type == null) {
            return null;
        }

        // Second argument is the value to cast
        const value_type = try self.analyzeNode(args[1]);
        if (value_type == null) {
            return null;
        }

        // @floatCast returns the target type
        return target_type;
    }

    /// Handle @panic(message) builtin
    fn handlePanicBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@panic expects exactly one argument", source_loc);
            return null;
        }

        // Argument should be a string
        const message_type = try self.analyzeNode(args[0]);
        if (message_type == null) {
            return null;
        }

        // @panic returns noreturn type
        return ast.Type.initPrimitive(.{ .noreturn = {} }, source_loc);
    }

    /// Handle @compileError(message) builtin
    fn handleCompileErrorBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@compileError expects exactly one argument", source_loc);
            return null;
        }

        // Trigger a compile-time error
        const msg = try std.fmt.allocPrint(self.allocator, "Compile-time error triggered by @compileError", .{});
        defer self.allocator.free(msg);
        try self.reportError(.compile_error, msg, source_loc);

        // @compileError returns noreturn type (though it never actually returns)
        return ast.Type.initPrimitive(.{ .noreturn = {} }, source_loc);
    }

    /// Handle memory operation builtins like @memcpy, @memset
    fn handleMemcpyBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@memcpy expects exactly two arguments", source_loc);
            return null;
        }

        // Both arguments should be pointers
        const dest_type = try self.analyzeNode(args[0]);
        const src_type = try self.analyzeNode(args[1]);

        if (dest_type == null or src_type == null) {
            return null;
        }

        // @memcpy returns void
        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    /// Handle @memset builtin
    fn handleMemsetBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@memset expects exactly two arguments", source_loc);
            return null;
        }

        // First argument should be a pointer, second should be a value
        const ptr_type = try self.analyzeNode(args[0]);
        const value_type = try self.analyzeNode(args[1]);

        if (ptr_type == null or value_type == null) {
            return null;
        }

        // @memset returns void
        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    /// Handle bit operation builtins like @clz, @ctz, @popCount
    fn handleBitCountBuiltin(
        self: *SemanticAnalyzer,
        operation: []const u8,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            const msg = try std.fmt.allocPrint(self.allocator, "@{s} expects exactly two arguments", .{operation});
            defer self.allocator.free(msg);
            try self.reportError(.invalid_function_call, msg, source_loc);
            return null;
        }

        // First argument is the type, second is the value
        const type_arg = try self.analyzeNode(args[0]);
        const value_type = try self.analyzeNode(args[1]);

        if (type_arg == null or value_type == null) {
            return null;
        }

        // Bit operations return the same type as the input
        return value_type;
    }

    /// Handle @ArrayList(T) builtin type constructor
    fn handleArrayListBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@ArrayList expects exactly one type argument", source_loc);
            return null;
        }

        // Get the element type
        const element_type = try self.analyzeNode(args[0]);
        if (element_type == null) {
            return null;
        }

        // For now, return a simplified ArrayList type
        // In a full implementation, this would be a generic struct type
        // TODO: Implement proper generic types
        return ast.Type{
            .data = .{
                .custom_struct = .{
                    .name = "ArrayList",
                    .fields = &[_]ast.Field{}, // Empty for now
                    .is_comptime = false,
                },
            },
            .source_loc = source_loc,
        };
    }

    /// Handle @arrayListInit(T, allocator) builtin
    fn handleArrayListInitBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@arrayListInit expects exactly two arguments (type, allocator)", source_loc);
            return null;
        }

        // Return ArrayList type
        return ast.Type{
            .data = .{ .custom_struct = .{
                .name = "ArrayList",
                .fields = &[_]ast.Field{},
                .is_comptime = false,
            } },
            .source_loc = source_loc,
        };
    }

    /// Handle @arrayListAppend(list, item) builtin
    fn handleArrayListAppendBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@arrayListAppend expects exactly two arguments (list, item)", source_loc);
            return null;
        }

        // Append returns void
        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    /// Handle @arrayListGet(list, index) builtin
    fn handleArrayListGetBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 2) {
            try self.reportError(.invalid_function_call, "@arrayListGet expects exactly two arguments (list, index)", source_loc);
            return null;
        }

        // For now, return a generic type - in a full implementation this would return T
        // TODO: Implement proper generic type inference
        return ast.Type.initPrimitive(.{ .i32 = {} }, source_loc); // Placeholder
    }

    /// Handle @arrayListLen(list) builtin
    fn handleArrayListLenBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@arrayListLen expects exactly one argument (list)", source_loc);
            return null;
        }

        // Length returns usize
        return ast.Type.initPrimitive(.{ .usize = {} }, source_loc);
    }

    /// Handle @arrayListClear(list) builtin
    fn handleArrayListClearBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@arrayListClear expects exactly one argument (list)", source_loc);
            return null;
        }

        // Clear returns void
        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    // ============================================================================
    // Public API
    // ============================================================================

    /// Analyze a node and perform appropriate semantic analysis
    fn analyzeNode(self: *SemanticAnalyzer, node_id: ast.NodeId) anyerror!?ast.Type {
        const node = self.arena.getNode(node_id) orelse return null;

        switch (node.data) {
            .enum_decl => |enum_decl| {
                _ = try self.analyzeEnumDeclaration(enum_decl, node.source_loc);
                return null;
            },
            .var_decl => |var_decl| {
                _ = try self.analyzeVariableDeclaration(var_decl, node.source_loc);
                return null;
            },
            .function_decl => |func_decl| {
                _ = try self.analyzeFunctionDeclaration(func_decl, node.source_loc);
                return null;
            },
            .extern_fn_decl => |extern_fn_decl| {
                _ = try self.analyzeExternFunctionDeclaration(extern_fn_decl, node.source_loc);
                return null;
            },
            .struct_decl => |struct_decl| {
                _ = try self.analyzeStructDeclaration(struct_decl, node.source_loc);
                return null;
            },
            .type_decl => |type_decl| {
                _ = try self.analyzeTypeDeclaration(type_decl, node.source_loc);
                return null;
            },
            .import_decl => |import_decl| {
                _ = try self.analyzeImportDeclaration(import_decl, node.source_loc);
                return null;
            },
            .return_stmt => |return_stmt| {
                _ = try self.analyzeReturnStatement(return_stmt, node.source_loc);
                return null;
            },
            .for_expr => |for_expr| {
                _ = try self.analyzeForExpression(for_expr, node.source_loc);
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },
            .block => |block| {
                // Analyze all statements in the block
                for (block.statements.items) |stmt_id| {
                    _ = try self.analyzeNode(stmt_id);
                }
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },
            else => {
                // For other nodes, try to infer their type
                return self.inferType(node_id);
            },
        }
    }

    pub fn analyzeProgram(self: *SemanticAnalyzer, root_node_id: ast.NodeId) !void {
        _ = try self.analyzeNode(root_node_id);
        // Check for unused variables (warning)
        self.checkUnusedSymbols();
    }

    fn checkUnusedSymbols(self: *SemanticAnalyzer) void {
        var iterator = self.global_scope.symbols.iterator();
        while (iterator.next()) |entry| {
            const symbol = entry.value_ptr.*;
            if (!symbol.is_used and symbol.symbol_type == .variable) {
                const msg = std.fmt.allocPrint(self.allocator, "Unused variable '{s}'", .{symbol.name}) catch continue;
                defer self.allocator.free(msg);

                self.reportWarning(.undefined_variable, // Reusing error code for warning
                    msg, symbol.source_loc) catch {};
            }
        }
    }

    /// Handle generic type instantiation like std.List(T)
    fn checkGenericTypeInstantiation(
        self: *SemanticAnalyzer,
        generic_type: @TypeOf(@as(ast.AstNode, undefined).data.generic_type_expr),
        source_loc: ast.SourceLoc,
    ) anyerror!?ast.Type {
        // Get the base type (e.g., "List" from "List(T)")
        const base_node = self.arena.getNode(generic_type.base_type) orelse return null;

        // Check if it's a member expression like std.List
        if (base_node.data == .member_expr) {
            const member = base_node.data.member_expr;

            // Check if this is std.List
            if (std.mem.eql(u8, member.field, "List")) {
                const obj_node = self.arena.getNode(member.object) orelse return null;
                if (obj_node.data == .identifier) {
                    const ident = obj_node.data.identifier;
                    if (std.mem.eql(u8, ident.name, "std")) {
                        // This is std.List(T) - create a list type
                        if (generic_type.type_params.items.len != 1) {
                            try self.reportError(.type_mismatch, "std.List expects exactly one type parameter", source_loc);
                            return null;
                        }

                        // For now, return a generic list type
                        // TODO: Store the element type properly
                        return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
                    }
                }
            }
        }

        // If it's a direct identifier (e.g., List(T))
        if (base_node.data == .identifier) {
            const ident = base_node.data.identifier;
            if (std.mem.eql(u8, ident.name, "List")) {
                // This is a direct List(T) usage
                if (generic_type.type_params.items.len != 1) {
                    try self.reportError(.type_mismatch, "List expects exactly one type parameter", source_loc);
                    return null;
                }

                return ast.Type.initPrimitive(.{ .type = {} }, source_loc);
            }
        }

        // Unknown generic type
        try self.reportError(.type_mismatch, "Unknown generic type", source_loc);
        return null;
    }

    fn inferEnumMemberType(self: *SemanticAnalyzer, member: @TypeOf(@as(ast.Literal, undefined).enum_member), source_loc: ast.SourceLoc) !?ast.Type {
        // Try to infer the enum type from context
        // For now, we'll use a special enum member type that needs to be resolved during type checking

        // Search through all registered enum types to see if this member exists
        var it = self.type_registry.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.data == .@"enum") {
                const enum_data = entry.value_ptr.data.@"enum";
                // Check if this enum has the requested member
                for (enum_data.members) |enum_member| {
                    if (std.mem.eql(u8, enum_member.name, member.name)) {
                        // Found a matching member, return the enum type
                        return entry.value_ptr.*;
                    }
                }
            }
        }

        // If we couldn't resolve it, return an unknown type that will be resolved during match analysis
        // This allows for enum member inference in match expressions
        return ast.Type{ .data = .unknown, .source_loc = source_loc };
    }
};
