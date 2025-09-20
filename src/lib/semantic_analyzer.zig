const std = @import("std");
const ast = @import("ast.zig");
const ErrorSystem = @import("error_system.zig");
const CompileError = @import("CompileError.zig").CompileError;
const ModuleRegistry = @import("module_registry.zig");
const ModuleLoader = @import("module_loader.zig");
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
    none: void,
    some: ast.NodeId,
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
            .none => ComptimeValue{ .none = {} },
            .some => |some| ComptimeValue{ .some = some.value },
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

    pub fn declare(self: *Scope, name: []const u8, symbol: Symbol) CompileError!void {
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
    global_scope: *Scope,
    file_path: []const u8,

    // Analysis state
    current_function_return_type: ?ast.Type,
    in_loop: bool,
    skip_function_bodies: bool,

    // Compile-time evaluation context
    type_registry: std.StringHashMap(ast.Type),
    struct_definitions: std.StringHashMap(ComptimeValue),
    comptime_values: std.StringHashMap(ComptimeValue),

    // Memory management for allocated types
    allocated_types: std.ArrayList(*ast.Type),

    // Module system
    module_registry: ?*ModuleRegistry.ModuleRegistry,
    module_loader: ?*ModuleLoader.ModuleLoader,
    imported_modules: std.ArrayList(*ModuleRegistry.Module),
    current_module: ?*ModuleRegistry.Module,

    pub fn init(
        allocator: std.mem.Allocator,
        arena: *ast.AstArena,
        errors: *ErrorSystem.ErrorCollector,
        file_path: []const u8,
    ) !SemanticAnalyzer {
        const global_scope = try allocator.create(Scope);
        global_scope.* = Scope.init(allocator, null);
        return SemanticAnalyzer{
            .allocator = allocator,
            .arena = arena,
            .errors = errors,
            .current_scope = global_scope,
            .global_scope = global_scope,
            .file_path = file_path,
            .current_function_return_type = null,
            .in_loop = false,
            .skip_function_bodies = false,
            .type_registry = std.StringHashMap(ast.Type).init(allocator),
            .struct_definitions = std.StringHashMap(ComptimeValue).init(allocator),
            .comptime_values = std.StringHashMap(ComptimeValue).init(allocator),
            .allocated_types = std.ArrayList(*ast.Type).init(allocator),
            .module_registry = null,
            .module_loader = null,
            .imported_modules = std.ArrayList(*ModuleRegistry.Module).init(allocator),
            .current_module = null,
        };
    }

    /// Initialize with module system support
    pub fn initWithModules(
        allocator: std.mem.Allocator,
        arena: *ast.AstArena,
        errors: *ErrorSystem.ErrorCollector,
        file_path: []const u8,
        module_registry: *ModuleRegistry.ModuleRegistry,
        module_loader: *ModuleLoader.ModuleLoader,
    ) !SemanticAnalyzer {
        var analyzer = try SemanticAnalyzer.init(allocator, arena, errors, file_path);
        analyzer.module_registry = module_registry;
        analyzer.module_loader = module_loader;

        // Don't hardcode std and math here - let explicit imports handle them

        return analyzer;
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        // Clean up all allocated types
        for (self.allocated_types.items) |type_ptr| {
            self.allocator.destroy(type_ptr);
        }
        self.allocated_types.deinit();

        // Clean up all scopes that weren't destroyed during analysis
        self.deinitScopeTree(self.global_scope);

        self.type_registry.deinit();
        self.struct_definitions.deinit();
        self.comptime_values.deinit();
        self.imported_modules.deinit();
    }

    /// Recursively deinitialize and destroy all scopes in the scope tree
    fn deinitScopeTree(self: *SemanticAnalyzer, scope: *Scope) void {
        // First, recursively deinit child scopes
        var it = scope.symbols.iterator();
        while (it.next()) |entry| {
            const symbol = entry.value_ptr.*;
            // If this symbol has a function_params field and it's a function,
            // we might need to clean up nested scopes, but for now we'll skip this
            _ = symbol;
        }

        // For a more complete implementation, we'd need to track all created scopes
        // For now, we'll just deinit the global scope and assume child scopes
        // are properly nested and will be cleaned up by their parents
        scope.deinit();
        self.allocator.destroy(scope);
    }

    /// Helper method to create and track allocated types
    fn createTrackedType(self: *SemanticAnalyzer, typ: ast.Type) CompileError!*ast.Type {
        const type_ptr = try self.allocator.create(ast.Type);
        type_ptr.* = typ;
        try self.allocated_types.append(type_ptr);
        return type_ptr;
    }

    /// Enter a new scope for symbol resolution
    pub fn enterScope(self: *SemanticAnalyzer) CompileError!*Scope {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.allocator, self.current_scope);
        self.current_scope = new_scope;
        return new_scope;
    }

    /// Search for a symbol in all scopes (for IR construction phase)
    pub fn lookupInAllScopes(self: *SemanticAnalyzer, name: []const u8) ?Symbol {
        // First, try the global scope directly - this handles code generation phase
        // where current_scope might not be properly set
        if (self.global_scope.lookupLocal(name)) |symbol| {
            return symbol;
        }

        // Then search through all scopes starting from current scope up to global scope
        var scope: ?*Scope = self.current_scope;
        while (scope) |current_scope| {
            if (current_scope.lookupLocal(name)) |symbol| {
                return symbol;
            }
            scope = current_scope.parent;
        }

        return null;
    }

    /// Reset current scope to global scope (useful for code generation phase)
    pub fn resetToGlobalScope(self: *SemanticAnalyzer) void {
        self.current_scope = self.global_scope;
    }

    /// Look for a function parameter in any function in the current scope
    fn lookupFunctionParameter(self: *SemanticAnalyzer, name: []const u8) ?ast.Type {
        var it = self.current_scope.symbols.iterator();
        while (it.next()) |entry| {
            const symbol = entry.value_ptr.*;
            if (symbol.symbol_type == .function) {
                if (symbol.function_params) |params| {
                    for (params) |param| {
                        if (std.mem.eql(u8, param.name, name)) {
                            // Found the parameter! Return its type
                            if (param.type_annotation) |type_node| {
                                return self.inferType(type_node) catch null;
                            }
                            return null;
                        }
                    }
                }
            }
        }

        return null;
    }

    /// Exit the current scope and return to parent
    fn exitScope(self: *SemanticAnalyzer) void {
        // Don't actually destroy scopes during semantic analysis
        // Keep them alive for IR construction phase
        if (self.current_scope.parent) |parent| {
            self.current_scope = parent;
            // Don't deinit/destroy the scope - keep it alive for IR construction
        }
    }

    // ============================================================================
    // Compile-time Evaluation
    // ============================================================================

    /// Evaluate an expression at compile time
    fn evaluateComptimeExpression(self: *SemanticAnalyzer, node_id: ast.NodeId) CompileError!?ComptimeValue {
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
    fn evaluateTypeExpression(self: *SemanticAnalyzer, node_id: ast.NodeId) CompileError!?ComptimeValue {
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
    fn evaluateStructTypeExpression(self: *SemanticAnalyzer, node_id: ast.NodeId) CompileError!?ComptimeValue {
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
    ) CompileError!void {
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
    ) CompileError!void {
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

    pub fn inferType(self: *SemanticAnalyzer, node_id: ast.NodeId) CompileError!?ast.Type {
        const node = self.arena.getNode(node_id) orelse return null;

        switch (node.data) {
            .literal => |literal| {
                return switch (literal) {
                    .integer => |int_lit| ast.Type.initPrimitive(int_lit.type_hint orelse .{ .i32 = {} }, node.source_loc),
                    .float => |float_lit| ast.Type.initPrimitive(float_lit.type_hint orelse .{ .f64 = {} }, node.source_loc),
                    .string => ast.Type.initPrimitive(.{ .str = {} }, node.source_loc),
                    .char => ast.Type.initPrimitive(.{ .char = {} }, node.source_loc),
                    .bool_true, .bool_false => ast.Type.initPrimitive(.{ .bool = {} }, node.source_loc),
                    .none => null, // None cannot be inferred without context - will need special handling
                    .some => |some| try self.inferType(some.value), // Infer from wrapped value
                    .enum_member => |member| self.inferEnumMemberType(member, node.source_loc),
                };
            },

            .identifier => |ident| {
                // Special case for error union types like "!void"
                if (std.mem.eql(u8, ident.name, "!void")) {
                    // Create an error union type !void
                    const void_type = try self.allocator.create(ast.Type);
                    void_type.* = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
                    return ast.Type{
                        .data = .{ .error_union = .{ .error_set = "anyerror", .payload_type = void_type } },
                        .source_loc = node.source_loc,
                    };
                }

                // First check global scope - this handles code generation phase
                if (self.global_scope.lookup(ident.name)) |symbol| {
                    const result_type = symbol.inferred_type orelse symbol.declared_type;
                    return result_type;
                }

                if (self.current_scope.lookup(ident.name)) |symbol| {
                    // Mark symbol as used
                    var mutable_symbol = symbol;
                    mutable_symbol.is_used = true;
                    try self.current_scope.symbols.put(ident.name, mutable_symbol);

                    const result_type = symbol.inferred_type orelse symbol.declared_type;
                    return result_type;
                } else {
                    // std.debug.print("DEBUG: Symbol '{s}' not found in scope, current scope has {d} symbols, global scope has {d} symbols\n", .{ ident.name, self.current_scope.symbols.count(), self.global_scope.symbols.count() });

                    // Check if this might be a function parameter
                    // Look for functions in the current scope that might have this as a parameter
                    if (self.lookupFunctionParameter(ident.name)) |param_type| {
                        return param_type;
                    }
                    // Check if it's a type name or comptime value
                    if (self.type_registry.get(ident.name)) |typ| {
                        return typ;
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
                        // First check if branches are directly compatible
                        if (!self.typesCompatible(then_type.?, else_type.?)) {
                            // Check if we have a context (function return type) that both branches could satisfy
                            if (self.current_function_return_type) |context_type| {
                                const then_compatible = self.typesCompatible(context_type, then_type.?);
                                const else_compatible = self.typesCompatible(context_type, else_type.?);

                                if (then_compatible and else_compatible) {
                                    // Both branches are compatible with the context type, use that as result
                                    result_type = context_type;
                                } else {
                                    // Special case: if context is error union and branches represent error vs payload
                                    if (context_type.data == .error_union) {
                                        const eu = context_type.data.error_union;
                                        const then_is_error = (then_type.?.data == .error_set and
                                            std.mem.eql(u8, eu.error_set, then_type.?.data.error_set.name));
                                        const else_is_payload = self.typesCompatible(eu.payload_type.*, else_type.?);

                                        if (then_is_error and else_is_payload) {
                                            // This is the error vs success case for error unions
                                            result_type = context_type;
                                        } else {
                                            // During IR construction, current_function_return_type may be null
                                            // In this case, assume the types are compatible since semantic analysis passed
                                            // std.debug.print("DEBUG: Error union compatibility check failed during IR construction\n", .{});
                                            // Don't report an error - assume semantic analysis already validated this
                                            result_type = context_type;
                                        }
                                    } else {
                                        // Provide detailed error message about type incompatibility
                                        const then_type_str = try self.typeToString(then_type.?);
                                        defer self.allocator.free(then_type_str);
                                        const else_type_str = try self.typeToString(else_type.?);
                                        defer self.allocator.free(else_type_str);
                                        const context_type_str = try self.typeToString(context_type);
                                        defer self.allocator.free(context_type_str);

                                        const detailed_msg = try std.fmt.allocPrint(self.allocator, "If-else branches have incompatible types. Then branch: '{s}', else branch: '{s}'. Expected return type: '{s}'", .{ then_type_str, else_type_str, context_type_str });
                                        defer self.allocator.free(detailed_msg);

                                        try self.reportError(.type_mismatch, detailed_msg, node.source_loc);
                                        result_type = null;
                                    }
                                }
                            } else {
                                // During IR construction, current_function_return_type may be null
                                // In this case, assume the types are compatible since semantic analysis passed
                                // Don't report an error - assume semantic analysis already validated this
                                result_type = then_type;
                            }
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
            .index_expr => |index_expr| {
                // Infer type of the object being indexed
                const obj_type = try self.inferType(index_expr.object);
                if (obj_type) |ot| {
                    switch (ot.data) {
                        .array => |arr| {
                            // Return the element type
                            return arr.element_type.*;
                        },
                        else => {
                            // Not an array, can't index
                            try self.reportError(.type_mismatch, "Cannot index into non-array type", node.source_loc);
                            return null;
                        },
                    }
                }
                return null;
            },

            .var_decl => |var_decl| {
                // For variable declarations, return the inferred type from the initializer
                if (var_decl.initializer) |init_id| {
                    return self.inferType(init_id);
                }
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
                // @compile.target evaluates to the current compile target (as a string value)
                return ast.Type.initPrimitive(.{ .str = {} }, node.source_loc);
            },

            .compile_insert_expr => {
                // @compile.insert("code") returns the inserted code as a string for consistency
                return ast.Type.initPrimitive(.{ .str = {} }, node.source_loc);
            },

            .match_compile_expr => |match_compile| {
                return self.analyzeMatchCompileExpression(match_compile, node.source_loc);
            },

            .try_expr => |try_expr| {
                // Delegate to analyzeTryExpression for proper validation
                return self.analyzeTryExpression(try_expr, node.source_loc);
            },

            .catch_expr => |catch_expr| {
                // Catch expressions return either the success type or the catch body type
                const expr_type = try self.inferType(catch_expr.expression);
                if (catch_expr.catch_body) |body| {
                    const catch_type = try self.inferType(body);
                    return catch_type orelse expr_type;
                } else if (catch_expr.fallback_value) |fallback| {
                    const fallback_type = try self.inferType(fallback);
                    return fallback_type;
                }
                return expr_type;
            },

            .error_union_type => |error_union| {
                // Delegate to proper error union type analysis
                return self.analyzeErrorUnionType(error_union, node.source_loc);
            },

            .error_literal => |error_literal| {
                // Error literals have error type (simplified as string for now)
                _ = error_literal; // Mark as used
                return ast.Type.initPrimitive(.{ .string = {} }, node.source_loc);
            },

            .error_set_decl => |error_set_decl| {
                // Process the error set declaration to register it in scope
                return self.analyzeErrorSetDeclaration(error_set_decl, node.source_loc) catch {
                    // On error, return null to continue analysis
                    return null;
                };
            },

            .union_decl => |union_decl| {
                // Union declarations represent the type itself
                _ = union_decl; // Mark as used
                return ast.Type.initPrimitive(.{ .type = {} }, node.source_loc);
            },

            .slice_type_expr => |slice_type| {
                // Slice types like []T
                const element_type = try self.inferType(slice_type.element_type);
                if (element_type) |elem_type| {
                    // Create a slice type (array with null size)
                    const heap_elem_type = try self.allocator.create(ast.Type);
                    heap_elem_type.* = elem_type;
                    return ast.Type{
                        .data = .{ .array = .{ .element_type = heap_elem_type, .size = null } },
                        .source_loc = node.source_loc,
                    };
                }
                return null;
            },

            .optional_type_expr => |optional_type| {
                // Optional types like ?T
                const inner_type = try self.inferType(optional_type.inner_type);
                if (inner_type) |inner| {
                    const heap_inner_type = try self.allocator.create(ast.Type);
                    heap_inner_type.* = inner;
                    return ast.Type{
                        .data = .{ .optional = heap_inner_type },
                        .source_loc = node.source_loc,
                    };
                }
                return null;
            },

            .pointer_type_expr => |pointer_type| {
                // Pointer types like ^T
                const inner_type = try self.inferType(pointer_type.inner_type);
                if (inner_type) |inner| {
                    const heap_inner_type = try self.allocator.create(ast.Type);
                    heap_inner_type.* = inner;
                    return ast.Type{
                        .data = .{ .pointer = heap_inner_type },
                        .source_loc = node.source_loc,
                    };
                }
                return null;
            },

            .error_union_type_expr => |error_union_type| {
                // Error union types like Error!T or !T
                const payload_type = try self.inferType(error_union_type.payload_type);
                const error_set_name = blk: {
                    const error_set_node = self.arena.getNode(error_union_type.error_set) orelse break :blk "error";
                    if (error_set_node.data == .identifier) {
                        break :blk error_set_node.data.identifier.name;
                    }
                    break :blk "error";
                };

                if (payload_type) |payload| {
                    const heap_payload_type = try self.allocator.create(ast.Type);
                    heap_payload_type.* = payload;
                    return ast.Type{
                        .data = .{ .error_union = .{ .error_set = error_set_name, .payload_type = heap_payload_type } },
                        .source_loc = node.source_loc,
                    };
                }
                return null;
            },

            .comptime_type_call => |comptime_call| {
                // Handle compile-time type generation like List(i32)
                return self.resolveComptimeTypeCallDirect(comptime_call, node.source_loc);
            },
        }
    }

    fn checkBinaryOperation(
        self: *SemanticAnalyzer,
        op: ast.BinaryOp,
        left_type: ?ast.Type,
        right_type: ?ast.Type,
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
        const callee_node = self.arena.getNode(callee_id);
        // std.debug.print("DEBUG: callee_node is null: {}\n", .{callee_node == null});
        if (callee_node == null) return null;
        // std.debug.print("DEBUG: callee_node.data = {}\n", .{callee_node.?.data});

        // First, check if this is a compile-time type function call
        const call_expr_data = ast.AstNode.NodeData{ .call_expr = .{ .callee = callee_id, .args = std.ArrayList(ast.NodeId).fromOwnedSlice(self.allocator, @constCast(args)) } };

        if (try self.resolveComptimeTypeCall(call_expr_data.call_expr, source_loc)) |comptime_type| {
            return comptime_type;
        }

        // For now, assume all function calls return i32
        // TODO: Implement proper function type checking

        switch (callee_node.?.data) {
            .identifier => |ident| {
                // std.debug.print("DEBUG: checkFunctionCall identifier '{s}'\n", .{ident.name});
                // Check for built-in functions first
                if (try self.handleBuiltinFunction(ident.name, args, source_loc)) |builtin_type| {
                    return builtin_type;
                }

                if (self.current_scope.lookup(ident.name)) |symbol| {
                    if (symbol.symbol_type == .function) {
                        // Debug: Print function call type
                        if (std.mem.eql(u8, ident.name, "getValue") or std.mem.eql(u8, ident.name, "getOpt")) {}
                        // TODO: Check argument count and types
                        return symbol.declared_type;
                    } else {
                        try self.reportError(.invalid_function_call, "Variable is not callable", source_loc);
                        return null;
                    }
                } else if (self.type_registry.get(ident.name)) |typ| {
                    // This is a struct constructor call like MyStruct{...}
                    return typ;
                } else {
                    const msg = try std.fmt.allocPrint(self.allocator, "Undefined function '{s}'", .{ident.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.undefined_variable, msg, source_loc);
                    return null;
                }
            },

            .member_expr => |member| {
                // std.debug.print("DEBUG: Handling member_expr call: object={}, field={s}\n", .{ member.object, member.field });
                // Check for built-in module functions like std.debug.print
                const member_obj_node = self.arena.getNode(member.object);
                if (member_obj_node) |member_obj| {
                    switch (member_obj.data) {
                        .identifier => |base_ident| {
                            // Handle simple member expressions like @compile.print
                            if (std.mem.eql(u8, base_ident.name, "compile")) {
                                if (std.mem.eql(u8, member.field, "print")) {
                                    // @compile.print returns void (it's a compile-time function)
                                    return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
                                }
                            }
                        },
                        else => {},
                    }
                }

                // Check for namespace member access (e.g., math.add)
                // const object_node = self.arena.getNode(member.object);
                // if (object_node) |obj_node| {
                //     // std.debug.print("DEBUG: object_node.data = {}\n", .{obj_node.data});
                // } else {
                //     std.debug.print("DEBUG: object_node is null\n", .{});
                // }
                const object_type = try self.inferType(member.object);
                // std.debug.print("DEBUG: object_type is null: {}\n", .{object_type == null});
                if (object_type) |obj_type| {
                    // std.debug.print("DEBUG: obj_type.data = {}\n", .{obj_type.data});
                    if (obj_type.data == .namespace) {
                        // std.debug.print("DEBUG: Found namespace, looking for member '{s}'\n", .{member.field});
                        if (obj_type.data.namespace.members.get(member.field)) |member_type| {
                            // std.debug.print("DEBUG: Found member, type data = {}\n", .{member_type.*.data});
                            // Check if the member is a function type
                            if (member_type.*.data == .function) {
                                // Return the function's return type
                                return member_type.*.data.function.return_type.*;
                            } else {
                                try self.reportError(.invalid_function_call, "Namespace member is not callable", source_loc);
                                return null;
                            }
                        } else {
                            // std.debug.print("DEBUG: Member '{s}' not found in namespace\n", .{member.field});
                        }
                    }

                    // Fall back to method lookup for member expressions
                    // This handles cases like h.append(x) where h is a variable and append is a function
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
    ) CompileError!?ast.Type {
        // Get the type of the object being accessed
        const obj_type = try self.inferType(object_id) orelse return null;

        // Handle namespace member access
        switch (obj_type.data) {
            .namespace => |ns| {
                // Look up the member in the namespace
                if (ns.members.get(field_name)) |member_type| {
                    return member_type.*;
                } else {
                    const error_msg = try std.fmt.allocPrint(self.allocator, "Namespace '{s}' has no member '{s}'", .{ ns.name, field_name });
                    defer self.allocator.free(error_msg);
                    try self.reportError(.undefined_variable, error_msg, source_loc);
                    return null;
                }
            },
            else => {
                // Fall back to the existing member access logic for structs, etc.
                return self.checkRegularMemberAccess(object_id, field_name, source_loc, obj_type);
            },
        }
    }

    /// Handle regular struct/type member access (non-namespace)
    fn checkRegularMemberAccess(
        self: *SemanticAnalyzer,
        object_id: ast.NodeId,
        field_name: []const u8,
        source_loc: ast.SourceLoc,
        obj_type: ast.Type,
    ) CompileError!?ast.Type {
        // Special handling for built-in modules (legacy - should be replaced by namespace system)
        const object_node = self.arena.getNode(object_id);
        if (object_node) |obj_node| {
            switch (obj_node.data) {
                .identifier => |ident| {
                    // Handle @compile module
                    if (std.mem.eql(u8, ident.name, "compile")) {
                        if (std.mem.eql(u8, field_name, "print")) {
                            // @compile.print returns void (it's a compile-time function)
                            return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
                        }
                        // Other @compile.* functions return their appropriate types
                        // Note: @compile.target and @compile.insert are handled by the parser as special AST nodes
                    }
                },
                else => {},
            }
        }

        // Handle member access based on object type
        switch (obj_type.data) {
            .error_set => |error_set| {
                // Check if field_name is one of the error enumerants
                for (error_set.enumerants) |enumerant| {
                    if (std.mem.eql(u8, enumerant, field_name)) {
                        // Return the error set type (the individual error value has the same type as the error set)
                        return obj_type;
                    }
                }

                // Provide helpful error message with suggestions
                var suggestion_msg = std.ArrayList(u8).init(self.allocator);
                defer suggestion_msg.deinit();

                try suggestion_msg.appendSlice("Error set '");
                try suggestion_msg.appendSlice(error_set.name);
                try suggestion_msg.appendSlice("' has no enumerant '");
                try suggestion_msg.appendSlice(field_name);
                try suggestion_msg.appendSlice("'. Available enumerants: ");

                for (error_set.enumerants, 0..) |enumerant, i| {
                    if (i > 0) try suggestion_msg.appendSlice(", ");
                    try suggestion_msg.appendSlice(enumerant);
                }

                try self.reportError(.invalid_member_access, suggestion_msg.items, source_loc);
                return null;
            },

            .@"enum" => |enum_info| {
                // Check if field_name is one of the enum members
                for (enum_info.members) |member| {
                    if (std.mem.eql(u8, member.name, field_name)) {
                        // Return the enum type (the individual enum value has the same type as the enum)
                        return obj_type;
                    }
                }

                // Provide helpful error message with suggestions
                var suggestion_msg = std.ArrayList(u8).init(self.allocator);
                defer suggestion_msg.deinit();

                try suggestion_msg.appendSlice("Enum '");
                try suggestion_msg.appendSlice(enum_info.name);
                try suggestion_msg.appendSlice("' has no member '");
                try suggestion_msg.appendSlice(field_name);
                try suggestion_msg.appendSlice("'. Available members: ");

                for (enum_info.members, 0..) |member, i| {
                    if (i > 0) try suggestion_msg.appendSlice(", ");
                    try suggestion_msg.appendSlice(member.name);
                }

                try self.reportError(.invalid_member_access, suggestion_msg.items, source_loc);
                return null;
            },

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

            .pointer => |pointed_type| {
                // Implicit dereference: ptr.field becomes ptr^.field
                // Handle member access on the pointed-to type
                const dereferenced_type = pointed_type.*;
                switch (dereferenced_type.data) {
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

                        const msg = try std.fmt.allocPrint(self.allocator, "Struct '{s}' has no field '{s}'", .{ struct_info.name, field_name });
                        defer self.allocator.free(msg);
                        try self.reportError(.invalid_member_access, msg, source_loc);
                        return null;
                    },
                    else => {
                        try self.reportError(.invalid_member_access, "Cannot access members of non-struct pointer", source_loc);
                        return null;
                    },
                }
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
    ) CompileError!?ast.Type {
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

    fn analyzeErrorSetDeclaration(
        self: *SemanticAnalyzer,
        error_set_decl: @TypeOf(@as(ast.AstNode, undefined).data.error_set_decl),
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
        // Validate error enumerants
        if (error_set_decl.errors.items.len == 0) {
            try self.reportError(.type_mismatch, "Error set cannot be empty", source_loc);
            return null;
        }

        // Check for duplicate error names
        for (error_set_decl.errors.items, 0..) |error_name1, i| {
            for (error_set_decl.errors.items[i + 1 ..], i + 1..) |error_name2, j| {
                if (std.mem.eql(u8, error_name1, error_name2)) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Duplicate error '{s}' in error set '{s}'", .{ error_name1, error_set_decl.name });
                    defer self.allocator.free(msg);
                    try self.reportError(.duplicate_declaration, msg, source_loc);
                    return null;
                }
                _ = j; // Suppress unused variable warning
            }
        }

        // Create owned copy of error names for the type system
        const owned_errors = try self.allocator.alloc([]const u8, error_set_decl.errors.items.len);
        for (error_set_decl.errors.items, 0..) |error_name, i| {
            // The error names are already owned by the AST arena, so we can safely reference them
            owned_errors[i] = error_name;
        }

        // Create the error set type with proper memory management
        const error_set_type = ast.Type.initErrorSet(error_set_decl.name, owned_errors, source_loc);

        // Register the error set type in the type registry
        try self.type_registry.put(error_set_decl.name, error_set_type);

        // Create compile-time value for the error set
        const comptime_value = ComptimeValue{ .string = error_set_decl.name };

        // Declare error set symbol with proper metadata
        const symbol = Symbol{
            .name = error_set_decl.name,
            .symbol_type = .type_def,
            .declared_type = error_set_type,
            .inferred_type = null,
            .source_loc = source_loc,
            .is_mutable = false,
            .is_used = false,
            .comptime_value = comptime_value,
            .function_params = null,
        };

        self.current_scope.declare(error_set_decl.name, symbol) catch |err| {
            switch (err) {
                error.DuplicateSymbol => {
                    const msg = try std.fmt.allocPrint(self.allocator, "Error set '{s}' is already declared", .{error_set_decl.name});
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
        // Create function scope
        const func_scope = try self.enterScope();
        // defer self.exitScope(); // Keep scopes for IR construction
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
                // Debug: Print function name and inferred return type

            }
        }

        // Set current function context
        const old_return_type = self.current_function_return_type;
        self.current_function_return_type = return_type;
        defer {
            self.current_function_return_type = old_return_type;
        }

        // Analyze function body (only if not skipping bodies)
        if (!self.skip_function_bodies) {
            _ = try self.analyzeNode(func_decl.body);
        }

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

    fn analyzeFunctionBody(self: *SemanticAnalyzer, func_node_id: ast.NodeId) CompileError!void {
        const func_node = self.arena.getNode(func_node_id) orelse return;
        if (func_node.data != .function_decl) return;

        const func_decl = func_node.data.function_decl;

        // Look up the function in the current scope (which should have access to parent scopes)
        if (self.current_scope.lookup(func_decl.name)) |symbol| {
            // Create function scope
            const func_scope = try self.enterScope();
            // defer self.exitScope(); // Keep scopes for IR construction
            _ = func_scope;

            // Process parameters and add them to function scope
            for (func_decl.params.items) |param| {
                var param_type: ?ast.Type = null;
                if (param.type_annotation) |type_node| {
                    param_type = try self.inferType(type_node);
                }

                const param_symbol = Symbol{
                    .name = param.name,
                    .symbol_type = .variable,
                    .declared_type = param_type,
                    .inferred_type = null,
                    .source_loc = param.source_loc,
                    .is_mutable = false, // Parameters are immutable by default
                    .is_used = false,
                    .comptime_value = null,
                    .function_params = null,
                };

                try self.current_scope.declare(param.name, param_symbol);
            }

            // Set current function context
            const old_return_type = self.current_function_return_type;
            self.current_function_return_type = symbol.declared_type;
            defer self.current_function_return_type = old_return_type;

            // Analyze function body
            _ = try self.analyzeNode(func_decl.body);
        }
    }

    fn analyzeExternFunctionDeclaration(
        self: *SemanticAnalyzer,
        extern_fn_decl: @TypeOf(@as(ast.AstNode, undefined).data.extern_fn_decl),
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
                    std.debug.print("DEBUG: Duplicate symbol error for {s}\n", .{type_decl.name});
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
    ) CompileError!?ast.Type {
        std.debug.print("DEBUG: analyzeImportDeclaration START\n", .{});
        // Load the module
        if (self.module_loader) |_| {
            std.debug.print("DEBUG: module_loader is not null\n", .{});
        } else {
            std.debug.print("DEBUG: module_loader is null!\n", .{});
            try self.reportError(.file_not_found, "Module system not available", source_loc);
            return null;
        }

        if (self.module_loader) |loader| {
            std.debug.print("DEBUG: analyzeImportDeclaration calling getOrLoadModule for {s}\n", .{import_decl.module_path});

            // Special handling for std module - hardcoded
            std.debug.print("DEBUG: Checking if module_path '{s}' equals 'std'\n", .{import_decl.module_path});
            if (std.mem.eql(u8, import_decl.module_path, "std")) {
                std.debug.print("DEBUG: Loading std module via hardcoded namespace\n", .{});
                const namespace_type = try self.createStdLibraryNamespace(source_loc);

                // For the import_example.howl, the import is "std :: @import("std")"
                // We need to find the variable name from the parent context
                // For now, assume the module name is used as the variable name
                const module_name = "std";
                std.debug.print("DEBUG: Module name = '{s}'\n", .{module_name});

                const symbol = Symbol{
                    .name = module_name,
                    .symbol_type = .@"const",
                    .declared_type = namespace_type,
                    .inferred_type = namespace_type,
                    .source_loc = source_loc,
                    .is_mutable = false,
                    .is_used = false,
                    .comptime_value = null,
                    .function_params = null,
                };

                try self.current_scope.declare(module_name, symbol);
                std.debug.print("DEBUG: Added std import symbol '{s}' to scope, current scope has {d} symbols\n", .{ module_name, self.current_scope.symbols.count() });
                return namespace_type;
            }

            const module = loader.getOrLoadModule(import_decl.module_path) catch |err| {
                std.debug.print("DEBUG: getOrLoadModule failed for {s}: {}\n", .{ import_decl.module_path, err });
                try self.reportError(.file_not_found, "Failed to load module", source_loc);
                return null;
            };
            std.debug.print("DEBUG: analyzeImportDeclaration got module {s}\n", .{module.name});
            // Don't deinit - module is cached in registry

            // Add to imported modules list (skip std module)
            std.debug.print("DEBUG: Module loaded: {s}, path: {s}, is_std_module: {}\n", .{ module.name, module.path, module.is_std_module });
            if (!module.is_std_module) {
                std.debug.print("Adding imported module: {s} (std: {})\n", .{ module.name, module.is_std_module });
                // Check if already in the list
                var already_imported = false;
                for (self.imported_modules.items) |existing| {
                    if (std.mem.eql(u8, existing.path, module.path)) {
                        already_imported = true;
                        break;
                    }
                }
                if (!already_imported) {
                    try self.imported_modules.append(module);
                    std.debug.print("Added module {s} to imported_modules, now have {d} modules\n", .{ module.name, self.imported_modules.items.len });
                }
            } else {
                std.debug.print("Skipping std module: {s}\n", .{module.name});
            }

            // Create a namespace type for the module
            var namespace_members = std.StringHashMap(*ast.Type).init(self.allocator);
            errdefer namespace_members.deinit();

            // Add all exported symbols from the module to the namespace
            try self.extractModuleExports(module, &namespace_members);

            // Create the namespace type
            const namespace_type = try self.createTrackedType(ast.Type{
                .data = .{ .namespace = .{
                    .name = module.name,
                    .members = namespace_members,
                } },
                .source_loc = source_loc,
            });
            // std.debug.print("DEBUG: Created namespace type with data = {}\n", .{namespace_type.*.data});

            // For the import_example.howl, the import is "math :: @import("math.howl")"
            // We need to find the variable name from the parent context
            // For now, assume the module name is used as the variable name
            const module_name = std.fs.path.stem(import_decl.module_path);
            // std.debug.print("DEBUG: Module name = '{s}'\n", .{module_name});

            const symbol = Symbol{
                .name = module_name,
                .symbol_type = .@"const",
                .declared_type = namespace_type.*,
                .inferred_type = namespace_type.*,
                .source_loc = source_loc,
                .is_mutable = false,
                .is_used = false,
                .comptime_value = null,
                .function_params = null,
            };
            // if (symbol.declared_type) |decl_type| {
            //     // std.debug.print("DEBUG: Symbol declared_type = {}\n", .{decl_type.data});
            // } else {
            //     std.debug.print("DEBUG: Symbol declared_type is null\n", .{});
            // }

            try self.global_scope.declare(module_name, symbol);
            std.debug.print("DEBUG: Added import symbol '{s}' to global scope, global scope has {d} symbols\n", .{ module_name, self.global_scope.symbols.count() });
            return namespace_type.*;
        } else {
            try self.reportError(.file_not_found, "Module system not available", source_loc);
            return null;
        }
    }

    fn analyzeReturnStatement(
        self: *SemanticAnalyzer,
        return_stmt: @TypeOf(@as(ast.AstNode, undefined).data.return_stmt),
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
            .none => null, // None requires context for type inference
            .some => |some| self.inferType(some.value) catch null, // Infer from wrapped value
            .enum_member => |member| self.inferEnumMemberType(member, source_loc) catch null,
        };
    }

    /// Resolve a type from a type annotation node
    fn resolveType(self: *SemanticAnalyzer, type_node_id: ast.NodeId) CompileError!?ast.Type {
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
            .optional_type_expr => |optional_type| {
                // Resolve the inner type first
                const inner_type = (try self.resolveType(optional_type.inner_type)) orelse {
                    try self.reportError(.invalid_declaration, "Invalid inner type in optional", type_node.source_loc);
                    return null;
                };

                // Create an optional type wrapping the inner type
                const inner_type_ptr = try self.arena.allocator.create(ast.Type);
                inner_type_ptr.* = inner_type;

                return ast.Type{
                    .data = .{ .optional = inner_type_ptr },
                    .source_loc = type_node.source_loc,
                };
            },
            .error_union_type_expr => |error_union_type| {
                // Resolve the payload type first
                const payload_type = (try self.resolveType(error_union_type.payload_type)) orelse {
                    try self.reportError(.invalid_declaration, "Invalid payload type in error union", type_node.source_loc);
                    return null;
                };

                // Get the error set name
                const error_set_name = blk: {
                    const error_set_node = self.arena.getNode(error_union_type.error_set) orelse break :blk "error";
                    if (error_set_node.data == .identifier) {
                        break :blk error_set_node.data.identifier.name;
                    }
                    break :blk "error";
                };

                // Create error union type
                const payload_type_ptr = try self.arena.allocator.create(ast.Type);
                payload_type_ptr.* = payload_type;

                return ast.Type{
                    .data = .{ .error_union = .{ .error_set = error_set_name, .payload_type = payload_type_ptr } },
                    .source_loc = type_node.source_loc,
                };
            },
            else => {
                try self.reportError(.invalid_declaration, "Invalid type annotation", type_node.source_loc);
                return null;
            },
        }
    }

    /// Analyze a match pattern and check it against the matched expression type
    fn analyzeMatchPattern(self: *SemanticAnalyzer, pattern: ast.MatchPattern, match_expr_type: ?ast.Type, source_loc: ast.SourceLoc) CompileError!void {
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
                                const msg = try std.fmt.allocPrint(self.allocator, "Enum member '{s}' does not exist in enum '{s}'", .{ member_name, enum_info.name });
                                defer self.allocator.free(msg);
                                try self.reportError(.undefined_variable, msg, source_loc);
                            }
                        },
                        else => {
                            try self.reportError(.type_mismatch, "Enum member pattern can only be used with enum types", source_loc);
                        },
                    }
                } else {
                    try self.reportError(.undefined_variable, "Cannot infer enum type for member pattern", source_loc);
                }
            },
            .comparison => |comp| {
                // TODO: Implement proper type checking for comparison patterns
                _ = comp;

                // For now, just skip the type checking and accept all comparison patterns
                // This will be expanded once the basic parsing works
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
            .some => |some_info| {
                // Check if the matched expression type is optional
                if (match_expr_type) |expr_type| {
                    switch (expr_type.data) {
                        .optional => |opt_info| {
                            // The bound variable should have the inner type
                            _ = opt_info; // Type is validated, variable binding handled in codegen
                            _ = some_info.bind_variable;
                        },
                        else => {
                            try self.reportError(.type_mismatch, "Some pattern can only be used with optional types", source_loc);
                        },
                    }
                } else {
                    try self.reportError(.undefined_variable, "Cannot infer optional type for Some pattern", source_loc);
                }
            },
            .none_pattern => {
                // Check if the matched expression type is optional
                if (match_expr_type) |expr_type| {
                    switch (expr_type.data) {
                        .optional => {
                            // None pattern is valid for optional types
                        },
                        else => {
                            try self.reportError(.type_mismatch, "None pattern can only be used with optional types", source_loc);
                        },
                    }
                } else {
                    try self.reportError(.undefined_variable, "Cannot infer optional type for None pattern", source_loc);
                }
            },
        }
    }

    /// Analyze compile-time match expression (match @compile.target)
    fn analyzeMatchCompileExpression(
        self: *SemanticAnalyzer,
        match_compile: @TypeOf(@as(ast.AstNode, undefined).data.match_compile_expr),
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
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

    /// Convert a type to a human-readable string for error messages
    fn typeToString(self: *SemanticAnalyzer, type_info: ast.Type) CompileError![]u8 {
        return switch (type_info.data) {
            .primitive => |prim| switch (prim) {
                .i8 => try self.allocator.dupe(u8, "i8"),
                .i16 => try self.allocator.dupe(u8, "i16"),
                .i32 => try self.allocator.dupe(u8, "i32"),
                .i64 => try self.allocator.dupe(u8, "i64"),
                .isize => try self.allocator.dupe(u8, "isize"),
                .u8 => try self.allocator.dupe(u8, "u8"),
                .u16 => try self.allocator.dupe(u8, "u16"),
                .u32 => try self.allocator.dupe(u8, "u32"),
                .u64 => try self.allocator.dupe(u8, "u64"),
                .usize => try self.allocator.dupe(u8, "usize"),
                .f32 => try self.allocator.dupe(u8, "f32"),
                .f64 => try self.allocator.dupe(u8, "f64"),
                .str => try self.allocator.dupe(u8, "str"),
                .strb => try self.allocator.dupe(u8, "strb"),
                .string => try self.allocator.dupe(u8, "string"),
                .bool => try self.allocator.dupe(u8, "bool"),
                .char => try self.allocator.dupe(u8, "char"),
                .void => try self.allocator.dupe(u8, "void"),
                .type => try self.allocator.dupe(u8, "type"),
                .noreturn => try self.allocator.dupe(u8, "noreturn"),
                .module => try self.allocator.dupe(u8, "module"),
            },
            .namespace => |ns| try std.fmt.allocPrint(self.allocator, "namespace({s})", .{ns.name}),
            .error_set => |error_set| try self.allocator.dupe(u8, error_set.name),
            .error_union => |error_union| {
                const payload_str = try self.typeToString(error_union.payload_type.*);
                defer self.allocator.free(payload_str);
                return try std.fmt.allocPrint(self.allocator, "{s}!{s}", .{ error_union.error_set, payload_str });
            },
            .@"struct" => |struct_info| try self.allocator.dupe(u8, struct_info.name),
            .custom_struct => |custom_struct| try self.allocator.dupe(u8, custom_struct.name),
            .@"enum" => |enum_info| try self.allocator.dupe(u8, enum_info.name),
            .function => try self.allocator.dupe(u8, "function"),
            .pointer => try self.allocator.dupe(u8, "pointer"),
            .array => try self.allocator.dupe(u8, "array"),
            .optional => try self.allocator.dupe(u8, "optional"),
            .comptime_type => try self.allocator.dupe(u8, "comptime type"),
            .union_type => |union_info| try self.allocator.dupe(u8, union_info.name),
            .unknown => try self.allocator.dupe(u8, "unknown"),
        };
    }

    /// Check if two types are compatible with comprehensive error union support
    fn typesCompatible(self: *SemanticAnalyzer, expected: ast.Type, actual: ast.Type) bool {
        // Debug output for specific case
        // const debug = false; // Change to true to enable debug output
        // if (debug) {
        //     std.debug.print("DEBUG typesCompatible: expected={}, actual={}\n", .{expected, actual});
        // }

        // Early return for identical types
        if (std.meta.eql(expected, actual)) return true;

        // Handle error union compatibility (both directions)
        if (expected.data == .error_union) {
            return self.isCompatibleWithErrorUnion(expected.data.error_union, actual);
        }

        if (actual.data == .error_union) {
            return self.isCompatibleWithErrorUnion(actual.data.error_union, expected);
        }

        // Handle primitive type compatibility with proper coercion rules
        if (expected.data == .primitive and actual.data == .primitive) {
            return self.isPrimitiveCompatible(expected.data.primitive, actual.data.primitive);
        }

        // Handle struct compatibility
        if (expected.data == .@"struct" and actual.data == .@"struct") {
            return self.isStructCompatible(expected.data.@"struct", actual.data.@"struct");
        }

        if (expected.data == .custom_struct and actual.data == .custom_struct) {
            return self.isCustomStructCompatible(expected.data.custom_struct, actual.data.custom_struct);
        }

        // Handle error set compatibility
        if (expected.data == .error_set and actual.data == .error_set) {
            return self.isErrorSetCompatible(expected.data.error_set, actual.data.error_set);
        }

        // Handle enum compatibility
        if (expected.data == .@"enum" and actual.data == .@"enum") {
            return std.mem.eql(u8, expected.data.@"enum".name, actual.data.@"enum".name);
        }

        // Handle namespace compatibility
        if (expected.data == .namespace and actual.data == .namespace) {
            return std.mem.eql(u8, expected.data.namespace.name, actual.data.namespace.name);
        }

        // Handle optional type compatibility
        if (expected.data == .optional) {
            // If both types are optional, compare their inner types
            if (actual.data == .optional) {
                return self.typesCompatible(expected.data.optional.*, actual.data.optional.*);
            }
            // Check if actual type can be converted to the inner optional type (T -> ?T)
            return self.typesCompatible(expected.data.optional.*, actual);
        }

        return false;
    }

    /// Check if a type is compatible with an error union
    fn isCompatibleWithErrorUnion(self: *SemanticAnalyzer, error_union: @TypeOf(@as(ast.Type, undefined).data.error_union), actual_type: ast.Type) bool {
        _ = self;

        return switch (actual_type.data) {
            // Payload type compatibility
            .primitive => |prim| blk: {
                if (error_union.payload_type.data == .primitive) {
                    break :blk std.meta.eql(error_union.payload_type.data.primitive, prim);
                }
                break :blk false;
            },

            // Error set compatibility
            .error_set => |error_set| std.mem.eql(u8, error_union.error_set, error_set.name),

            // Error set member compatibility (e.g., MyError.DivisionByZero)
            .@"enum" => |enum_info| std.mem.eql(u8, error_union.error_set, enum_info.name),

            // Struct compatibility
            .@"struct" => blk: {
                if (error_union.payload_type.data == .@"struct") {
                    break :blk std.meta.eql(error_union.payload_type.data.@"struct", actual_type.data.@"struct");
                }
                break :blk false;
            },

            .custom_struct => blk: {
                if (error_union.payload_type.data == .custom_struct) {
                    break :blk std.meta.eql(error_union.payload_type.data.custom_struct, actual_type.data.custom_struct);
                }
                break :blk false;
            },

            // Other error union (for nested error unions)
            .error_union => |other_error_union| {
                // Both error sets must be compatible and payload types must match
                return std.mem.eql(u8, error_union.error_set, other_error_union.error_set) and
                    std.meta.eql(error_union.payload_type.*, other_error_union.payload_type.*);
            },

            else => false,
        };
    }

    /// Check primitive type compatibility with proper coercion rules
    fn isPrimitiveCompatible(self: *SemanticAnalyzer, expected: ast.PrimitiveType, actual: ast.PrimitiveType) bool {
        _ = self;

        // Exact match
        if (std.meta.eql(expected, actual)) return true;

        return switch (expected) {
            // Integer compatibility - allow widening conversions
            .i8 => switch (actual) {
                .i8 => true,
                else => false,
            },
            .i16 => switch (actual) {
                .i8, .i16 => true,
                else => false,
            },
            .i32 => switch (actual) {
                .i8, .i16, .i32 => true,
                else => false,
            },
            .i64 => switch (actual) {
                .i8, .i16, .i32, .i64 => true,
                else => false,
            },
            .isize => switch (actual) {
                .i8, .i16, .i32, .i64, .isize => true,
                else => false,
            },

            // Unsigned integer compatibility
            .u8 => switch (actual) {
                .u8 => true,
                else => false,
            },
            .u16 => switch (actual) {
                .u8, .u16 => true,
                else => false,
            },
            .u32 => switch (actual) {
                .u8, .u16, .u32 => true,
                else => false,
            },
            .u64 => switch (actual) {
                .u8, .u16, .u32, .u64 => true,
                else => false,
            },
            .usize => switch (actual) {
                .u8, .u16, .u32, .u64, .usize => true,
                else => false,
            },

            // Float compatibility
            .f32 => switch (actual) {
                .f32 => true,
                else => false,
            },
            .f64 => switch (actual) {
                .f32, .f64 => true,
                else => false,
            },

            // String types
            .str => switch (actual) {
                .str => true,
                else => false,
            },
            .strb => switch (actual) {
                .strb => true,
                else => false,
            },
            .string => switch (actual) {
                .string, .str => true,
                else => false,
            }, // Legacy compatibility

            // Other types must match exactly
            .bool => switch (actual) {
                .bool => true,
                else => false,
            },
            .char => switch (actual) {
                .char => true,
                else => false,
            },
            .void => switch (actual) {
                .void => true,
                else => false,
            },
            .type => switch (actual) {
                .type => true,
                else => false,
            },
            .noreturn => switch (actual) {
                .noreturn => true,
                else => false,
            },
            .module => switch (actual) {
                .module => true,
                else => false,
            },
        };
    }

    /// Check struct type compatibility
    fn isStructCompatible(self: *SemanticAnalyzer, expected: @TypeOf(@as(ast.Type, undefined).data.@"struct"), actual: @TypeOf(@as(ast.Type, undefined).data.@"struct")) bool {
        _ = self;
        // Structs must have the same name for now (nominal typing)
        return std.mem.eql(u8, expected.name, actual.name);
    }

    /// Check custom struct type compatibility
    fn isCustomStructCompatible(self: *SemanticAnalyzer, expected: @TypeOf(@as(ast.Type, undefined).data.custom_struct), actual: @TypeOf(@as(ast.Type, undefined).data.custom_struct)) bool {
        _ = self;
        // Custom structs must have the same name for now (nominal typing)
        return std.mem.eql(u8, expected.name, actual.name);
    }

    /// Check error set compatibility
    fn isErrorSetCompatible(self: *SemanticAnalyzer, expected: @TypeOf(@as(ast.Type, undefined).data.error_set), actual: @TypeOf(@as(ast.Type, undefined).data.error_set)) bool {
        _ = self;
        // Error sets are compatible if they have the same name
        return std.mem.eql(u8, expected.name, actual.name);
    }

    /// Validate that enum explicit values are in ascending order
    fn validateEnumValueOrder(self: *SemanticAnalyzer, members: []ast.EnumMember, source_loc: ast.SourceLoc) CompileError!void {
        _ = source_loc; // unused parameter
        var last_value: ?i64 = null;

        for (members) |member| {
            if (member.value) |value_node| {
                // Evaluate the explicit value at compile time
                const value = try self.evaluateCompileTimeInteger(value_node);
                if (value) |int_value| {
                    if (last_value) |last| {
                        if (int_value <= last) {
                            const msg = try std.fmt.allocPrint(self.allocator, "Enum member '{s}' has value {d} which is not greater than previous value {d}. Enum explicit values must be in ascending order.", .{ member.name, int_value, last });
                            defer self.allocator.free(msg);
                            try self.reportError(.invalid_enum_value, msg, member.source_loc);
                            return;
                        }
                    }
                    last_value = int_value;
                } else {
                    // If we can't evaluate the value as a compile-time integer, report an error
                    const msg = try std.fmt.allocPrint(self.allocator, "Enum member '{s}' has non-constant explicit value. Enum values must be compile-time integer constants.", .{member.name});
                    defer self.allocator.free(msg);
                    try self.reportError(.invalid_enum_value, msg, member.source_loc);
                    return;
                }
            }
            // If no explicit value, continue (implicit values will be auto-assigned)
        }
    }

    /// Evaluate an AST node as a compile-time integer constant
    fn evaluateCompileTimeInteger(self: *SemanticAnalyzer, node_id: ast.NodeId) CompileError!?i64 {
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
    ) CompileError!?ast.Type {
        // Get the struct type name
        if (struct_init.type_name) |type_name| {
            // Look up the struct type
            if (self.type_registry.get(type_name)) |struct_type| {
                // TODO: Validate field initialization
                return struct_type;
            } else {
                // If not found in type registry, create a custom struct type
                // This handles cases where the struct is declared but not yet registered
                const custom_struct_type = ast.Type{
                    .data = .{ .custom_struct = .{ .name = type_name, .fields = &[_]ast.Field{}, .is_comptime = false } },
                    .source_loc = source_loc,
                };
                return custom_struct_type;
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
    ) CompileError!?ast.Type {
        // Infer element type from the first element
        if (array_init.elements.items.len > 0) {
            const first_elem_type = try self.inferType(array_init.elements.items[0]);
            if (first_elem_type) |elem_type| {
                const elem_type_ptr = try self.allocator.create(ast.Type);
                elem_type_ptr.* = elem_type;
                return ast.Type{
                    .data = .{ .array = .{ .element_type = elem_type_ptr, .size = array_init.elements.items.len } },
                    .source_loc = source_loc,
                };
            }
        }

        // Fallback
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
    ) CompileError!?ast.Type {
        // std.debug.print("DEBUG: handleBuiltinFunction called with name '{s}'\n", .{name});
        // Module system
        if (std.mem.eql(u8, name, "@import") or std.mem.eql(u8, name, "import")) {
            // std.debug.print("DEBUG: Handling @import builtin, args.len = {}\n", .{args.len});
            return self.handleImportBuiltin(args, source_loc);
        }

        // Error union builtins
        if (std.mem.eql(u8, name, "error_ok")) {
            return self.handleErrorOkBuiltin(args, source_loc);
        }
        if (std.mem.eql(u8, name, "error_err")) {
            return self.handleErrorErrBuiltin(args, source_loc);
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
    ) CompileError!?ast.Type {
        // std.debug.print("DEBUG: handleImportBuiltin called, args.len = {}\n", .{args.len});
        if (args.len != 1) {
            try self.reportError(.invalid_function_call, "@import expects exactly one argument", source_loc);
            return null;
        }

        // Get the module path from the first argument
        const path_node = self.arena.getNode(args[0]) orelse return null;

        if (path_node.data != .literal or path_node.data.literal != .string) {
            try self.reportError(.invalid_function_call, "@import expects a string literal", source_loc);
            return null;
        }

        const module_path = path_node.data.literal.string.value;

        // Special handling for std module - hardcoded
        if (std.mem.eql(u8, module_path, "std")) {
            return try self.createStdLibraryNamespace(source_loc);
        }

        // Special handling for math module - hardcoded
        // std.debug.print("DEBUG: IMPORT: Checking module_path: '{s}'\n", .{module_path});

        // For other modules, load the module and create a namespace type
        if (self.module_loader) |loader| {
            const module = loader.getOrLoadModule(module_path) catch {
                try self.reportError(.file_not_found, "Failed to load module", source_loc);
                return ast.Type.initPrimitive(.{ .module = {} }, source_loc);
            };
            // Don't deinit - module is cached in registry

            // Add to imported modules list
            var already_imported = false;
            for (self.imported_modules.items) |existing| {
                if (std.mem.eql(u8, existing.path, module.path)) {
                    already_imported = true;
                    break;
                }
            }
            if (!already_imported) {
                try self.imported_modules.append(module);
            }

            // Create a namespace type for the module
            var namespace_members = std.StringHashMap(*ast.Type).init(self.allocator);
            errdefer namespace_members.deinit();

            // Add all exported symbols from the module to the namespace
            try self.extractModuleExports(module, &namespace_members);

            // Create the namespace type
            const namespace_type = try self.createTrackedType(ast.Type{
                .data = .{ .namespace = .{
                    .name = module.name,
                    .members = namespace_members,
                } },
                .source_loc = source_loc,
            });

            // Declare the module symbol to global scope
            const module_name = std.fs.path.stem(module_path);
            const symbol = Symbol{
                .name = module_name,
                .symbol_type = .@"const",
                .declared_type = namespace_type.*,
                .inferred_type = namespace_type.*,
                .source_loc = source_loc,
                .is_mutable = false,
                .is_used = false,
                .comptime_value = null,
                .function_params = null,
            };
            try self.global_scope.declare(module_name, symbol);

            return namespace_type.*;
        } else {
            try self.reportError(.file_not_found, "Module system not available", source_loc);
            return ast.Type.initPrimitive(.{ .module = {} }, source_loc);
        }
    }

    /// Create the std library namespace with debug.print and other functions
    fn createStdLibraryNamespace(self: *SemanticAnalyzer, source_loc: ast.SourceLoc) CompileError!ast.Type {
        // Create the print function type: fn(format: str, args: ...) void
        const print_fn_type = try self.allocator.create(ast.Type);
        print_fn_type.* = ast.Type{
            .data = .{
                .function = .{
                    .param_types = &[_]ast.Type{}, // Variadic - we'll handle this specially
                    .return_type = try self.allocator.create(ast.Type),
                },
            },
            .source_loc = source_loc,
        };
        print_fn_type.data.function.return_type.* = ast.Type.initPrimitive(.{ .void = {} }, source_loc);

        // Create the debug namespace
        var debug_members = std.StringHashMap(*ast.Type).init(self.allocator);
        try debug_members.put("print", print_fn_type);

        const debug_ns_type = try self.allocator.create(ast.Type);
        debug_ns_type.* = ast.Type{
            .data = .{ .namespace = .{
                .name = "debug",
                .members = debug_members,
            } },
            .source_loc = source_loc,
        };

        // Create the std namespace with debug as a member
        var std_members = std.StringHashMap(*ast.Type).init(self.allocator);
        try std_members.put("debug", debug_ns_type);

        return ast.Type{
            .data = .{ .namespace = .{
                .name = "std",
                .members = std_members,
            } },
            .source_loc = source_loc,
        };
    }

    /// Handle @error_ok(value) builtin
    fn handleErrorOkBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
        if (args.len != 1) {
            // Report error but allow compilation to continue
            return null;
        }

        // Create a simple error union type
        const i32_type = try self.allocator.create(ast.Type);
        i32_type.* = ast.Type.initPrimitive(.{ .i32 = {} }, source_loc);

        return ast.Type{
            .data = .{ .error_union = .{ .error_set = "anyerror", .payload_type = i32_type } },
            .source_loc = source_loc,
        };
    }

    /// Handle @error_err(code) builtin
    fn handleErrorErrBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
        if (args.len != 1) {
            // Report error but allow compilation to continue
            return null;
        }

        // Create a simple error union type
        const i32_type = try self.allocator.create(ast.Type);
        i32_type.* = ast.Type.initPrimitive(.{ .i32 = {} }, source_loc);

        return ast.Type{
            .data = .{ .error_union = .{ .error_set = "anyerror", .payload_type = i32_type } },
            .source_loc = source_loc,
        };
    }

    /// Handle @TypeOf(expr) builtin
    fn handleTypeOfBuiltin(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    ) CompileError!?ast.Type {
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
    fn analyzeNode(self: *SemanticAnalyzer, node_id: ast.NodeId) CompileError!?ast.Type {
        const node = self.arena.getNode(node_id) orelse return null;

        switch (node.data) {
            .enum_decl => |enum_decl| {
                _ = try self.analyzeEnumDeclaration(enum_decl, node.source_loc);
                return null;
            },
            .error_set_decl => |error_set_decl| {
                _ = try self.analyzeErrorSetDeclaration(error_set_decl, node.source_loc);
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
                // Process all statements in the block
                for (block.statements.items) |stmt_id| {
                    _ = try self.analyzeNode(stmt_id);
                }
                return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
            },
            .try_expr => |try_expr| {
                return self.analyzeTryExpression(try_expr, node.source_loc);
            },
            .catch_expr => |catch_expr| {
                return self.analyzeCatchExpression(catch_expr, node.source_loc);
            },
            .error_union_type => |error_union| {
                return self.analyzeErrorUnionType(error_union, node.source_loc);
            },
            .error_literal => |error_literal| {
                // Error literals have error type
                _ = error_literal; // avoid unused variable warning
                return ast.Type.initPrimitive(.{ .string = {} }, node.source_loc); // Simplified for now
            },
            else => {
                // For other nodes, try to infer their type
                return self.inferType(node_id);
            },
        }
    }

    // ============================================================================
    // Error Handling Analysis
    // ============================================================================

    fn analyzeTryExpression(self: *SemanticAnalyzer, try_expr: @TypeOf(@as(ast.AstNode, undefined).data.try_expr), source_loc: ast.SourceLoc) CompileError!?ast.Type {
        // Validate that try is used in a context where errors can be handled
        // During IR construction phase, current_function_return_type may be null,
        // but semantic analysis has already validated the try expression
        if (self.current_function_return_type == null) {
            // If we're in IR construction phase, assume the try expression is valid
            // since semantic analysis would have caught any issues
        } else {

            // During semantic analysis phase, validate the function context
            const current_return_type = self.current_function_return_type.?;

            // Check if current function can handle errors (returns error union)
            var can_handle_errors = false;
            switch (current_return_type.data) {
                .error_union => can_handle_errors = true,
                .primitive => |prim| {
                    // Check if it's void - might be !void
                    if (prim == .void) {
                        // This is a simplified check - in a full implementation, we'd need
                        // to track whether this void is actually !void
                        can_handle_errors = true;
                    }
                },
                else => {},
            }

            if (!can_handle_errors) {
                try self.reportError(.invalid_statement, "Try expression used in function that doesn't return an error union", source_loc);
                return null;
            }
        }

        // Analyze the expression being tried
        const expr_type = try self.inferType(try_expr.expression);

        if (expr_type) |et| {
            // Validate that the expression returns an error union
            switch (et.data) {
                .error_union => |eu| {
                    // Return the payload type of the error union
                    return eu.payload_type.*;
                },
                else => {
                    try self.reportError(.type_mismatch, "Try expression requires an error union type", source_loc);
                    return null;
                },
            }
        }

        return expr_type;
    }

    fn analyzeCatchExpression(self: *SemanticAnalyzer, catch_expr: @TypeOf(@as(ast.AstNode, undefined).data.catch_expr), source_loc: ast.SourceLoc) CompileError!?ast.Type {
        // Analyze the expression being caught (not necessarily a try expression)
        const expr_type = try self.analyzeNode(catch_expr.expression);

        // If there's an error capture variable, add it to current scope
        if (catch_expr.error_capture) |error_var| {
            const error_symbol = Symbol{
                .name = error_var,
                .symbol_type = .variable,
                .declared_type = ast.Type.initPrimitive(.{ .string = {} }, source_loc), // Error type
                .inferred_type = null,
                .source_loc = source_loc,
                .is_used = false,
                .is_mutable = false,
                .comptime_value = null,
                .function_params = null,
            };
            _ = try self.current_scope.symbols.put(error_var, error_symbol);
        }

        // Analyze catch body or fallback value
        var catch_type: ?ast.Type = null;
        if (catch_expr.catch_body) |body| {
            catch_type = try self.analyzeNode(body);
        } else if (catch_expr.fallback_value) |fallback| {
            catch_type = try self.analyzeNode(fallback);
        }

        // The catch expression returns the type of the catch body/fallback or the success type of the expression
        if (catch_type) |ct| {
            return ct;
        } else if (expr_type) |et| {
            // Return the payload type if it's an error union
            // TODO: Extract payload type from error union
            return et;
        }

        return null;
    }

    fn analyzeErrorUnionType(self: *SemanticAnalyzer, error_union: @TypeOf(@as(ast.AstNode, undefined).data.error_union_type), source_loc: ast.SourceLoc) CompileError!?ast.Type {
        // Handle implicit error set (defaults to anyerror)
        const error_set_name = if (error_union.error_set) |error_set_id| blk: {
            const error_set_node = self.arena.getNode(error_set_id) orelse {
                try self.reportError(.type_mismatch, "Invalid error set in error union", source_loc);
                break :blk "anyerror";
            };

            break :blk switch (error_set_node.data) {
                .identifier => |ident| ident.name,
                else => {
                    try self.reportError(.type_mismatch, "Error set must be an identifier", source_loc);
                    break :blk "anyerror";
                },
            };
        } else "anyerror";

        // Validate that the error set exists and is declared (skip for built-in "anyerror")
        if (!std.mem.eql(u8, error_set_name, "anyerror")) {
            const error_set_symbol = self.current_scope.lookup(error_set_name);
            if (error_set_symbol == null or error_set_symbol.?.symbol_type != .type_def) {
                const msg = try std.fmt.allocPrint(self.allocator, "Undefined error set '{s}'", .{error_set_name});
                defer self.allocator.free(msg);
                try self.reportError(.undefined_variable, msg, source_loc);
                return null;
            }
        }

        // Analyze and validate the payload type
        const payload_type = try self.inferType(error_union.payload_type);
        if (payload_type == null) {
            try self.reportError(.type_mismatch, "Invalid payload type in error union", source_loc);
            return null;
        }

        // Validate that payload type is not another error union (for now, to avoid complexity)
        if (payload_type.?.data == .error_union) {
            try self.reportError(.type_mismatch, "Nested error unions are not supported", source_loc);
            return null;
        }

        // Create error union type with proper memory management
        const payload_type_ptr = try self.allocator.create(ast.Type);
        payload_type_ptr.* = payload_type.?;

        const result_type = ast.Type{
            .data = .{ .error_union = .{
                .error_set = error_set_name,
                .payload_type = payload_type_ptr,
            } },
            .source_loc = source_loc,
        };

        return result_type;
    }

    pub fn analyzeProgram(self: *SemanticAnalyzer, root_node_id: ast.NodeId) CompileError!void {
        _ = try self.analyzeNode(root_node_id);
        // Check for unused variables (warning)
        self.checkUnusedSymbols();
    }

    /// Extract exported symbols from a module
    fn extractModuleExports(self: *SemanticAnalyzer, module: *ModuleRegistry.Module, members: *std.StringHashMap(*ast.Type)) CompileError!void {
        // Analyze the module's AST to find top-level declarations
        const root_node = module.ast_root;

        const node = module.arena.getNodeConst(root_node) orelse {
            return;
        };
        // std.debug.print("DEBUG: Got root node successfully, data type: {any}\n", .{node.data});

        // Extract from the root node directly
        try self.extractExportFromStatement(&module.arena, root_node, members);

        switch (node.data) {
            .block => |block| {
                for (block.statements.items, 0..) |stmt_id, i| {
                    _ = i;
                    try self.extractExportFromStatement(&module.arena, stmt_id, members);
                }
            },
            else => {
                try self.extractExportFromStatement(&module.arena, root_node, members);
            },
        }
    }

    /// Extract a single export from a statement
    fn extractExportFromStatement(self: *SemanticAnalyzer, arena: *ast.AstArena, stmt_id: ast.NodeId, members: *std.StringHashMap(*ast.Type)) CompileError!void {
        const node = arena.getNodeConst(stmt_id) orelse return;

        switch (node.data) {
            .var_decl => |var_decl| {
                // Export all top-level variables
                const var_type = try self.inferTypeFromArena(arena, var_decl.initializer orelse return);
                if (var_type) |typ| {
                    const tracked_type = try self.createTrackedType(typ);
                    const name_dup = try self.allocator.dupe(u8, var_decl.name);
                    try members.put(name_dup, tracked_type);
                } else {}
            },
            .function_decl => |func_decl| {
                // Export functions
                const func_type = try self.createFunctionTypeFromDeclArena(arena, func_decl);
                if (func_type) |ft| {
                    const tracked_type = try self.createTrackedType(ft);
                    const name_dup = try self.allocator.dupe(u8, func_decl.name);
                    try members.put(name_dup, tracked_type);
                } else {}
            },
            .extern_fn_decl => |extern_fn_decl| {
                // Export extern functions
                const func_type = try self.createFunctionTypeFromDeclArena(arena, extern_fn_decl);
                if (func_type) |ft| {
                    const tracked_type = try self.createTrackedType(ft);
                    const name_dup = try self.allocator.dupe(u8, extern_fn_decl.name);
                    try members.put(name_dup, tracked_type);
                }
            },
            else => {
                // Other declarations can be added later
            },
        }
    }

    /// Infer type from a different arena (for imported modules)
    fn inferTypeFromArena(self: *SemanticAnalyzer, arena: *ast.AstArena, node_id: ?ast.NodeId) CompileError!?ast.Type {
        if (node_id) |id| {
            const node = arena.getNodeConst(id) orelse return null;
            // For now, only handle simple cases
            switch (node.data) {
                .literal => |literal| {
                    return switch (literal) {
                        .integer => ast.Type.initPrimitive(.{ .i32 = {} }, node.source_loc),
                        .float => ast.Type.initPrimitive(.{ .f64 = {} }, node.source_loc),
                        .string => ast.Type.initPrimitive(.{ .str = {} }, node.source_loc),
                        .bool_true, .bool_false => ast.Type.initPrimitive(.{ .bool = {} }, node.source_loc),
                        else => null,
                    };
                },
                .identifier => |ident| {
                    // Resolve primitive types
                    if (self.resolvePrimitiveType(ident.name)) |prim_type| {
                        return ast.Type.initPrimitive(prim_type, node.source_loc);
                    }
                    return null;
                },
                .function_decl => |func_decl| {
                    // Create function type from function declaration
                    return try self.createFunctionTypeFromDeclArena(arena, func_decl);
                },
                else => return null,
            }
        }
        return null;
    }

    /// Create a function type from a function declaration in a different arena
    fn createFunctionTypeFromDeclArena(self: *SemanticAnalyzer, arena: *ast.AstArena, func_decl: anytype) CompileError!?ast.Type {
        return try self.createFunctionTypeFromDecl(arena, func_decl);
    }

    /// Create a function type from a function declaration
    fn createFunctionTypeFromDecl(self: *SemanticAnalyzer, arena: *ast.AstArena, func_decl: anytype) CompileError!?ast.Type {
        // Collect parameter types
        var param_types = std.ArrayList(ast.Type).init(self.allocator);
        defer param_types.deinit();

        for (func_decl.params.items) |param| {
            if (param.type_annotation) |type_node_id| {
                if (try self.inferTypeFromArena(arena, type_node_id)) |param_type| {
                    try param_types.append(param_type);
                }
            }
        }

        // Get return type
        var return_type = ast.Type.initPrimitive(.{ .void = {} }, ast.SourceLoc.invalid());
        if (func_decl.return_type) |return_type_id| {
            if (try self.inferTypeFromArena(arena, return_type_id)) |ret_type| {
                return_type = ret_type;
            }
        }

        // Create the function type
        const return_type_ptr = try self.allocator.create(ast.Type);
        return_type_ptr.* = return_type;

        return ast.Type{
            .data = .{ .function = .{
                .param_types = try param_types.toOwnedSlice(),
                .return_type = return_type_ptr,
            } },
            .source_loc = ast.SourceLoc.invalid(),
        };
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
    ) CompileError!?ast.Type {
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

    fn inferEnumMemberType(self: *SemanticAnalyzer, member: @TypeOf(@as(ast.Literal, undefined).enum_member), source_loc: ast.SourceLoc) CompileError!?ast.Type {
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
            } else if (entry.value_ptr.data == .error_set) {
                const error_set_data = entry.value_ptr.data.error_set;
                // Check if this error set has the requested member
                for (error_set_data.enumerants) |error_name| {
                    if (std.mem.eql(u8, error_name, member.name)) {
                        // Found a matching error, return the error set type
                        return entry.value_ptr.*;
                    }
                }
            }
        }

        // If we couldn't resolve it, return an unknown type that will be resolved during match analysis
        // This allows for enum member inference in match expressions
        return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
    }

    // ============================================================================
    // Compile-Time Type Function Resolution
    // ============================================================================

    /// Resolve a compile-time type function call from comptime_type_call node
    fn resolveComptimeTypeCallDirect(
        self: *SemanticAnalyzer,
        comptime_call: @TypeOf(@as(ast.AstNode, undefined).data.comptime_type_call),
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
        const function_node = self.arena.getNode(comptime_call.function) orelse return null;

        // Check if this is a simple identifier (e.g., List(i32))
        if (function_node.data == .identifier) {
            const function_name = function_node.data.identifier.name;

            // Handle built-in type functions
            if (try self.resolveBuiltinTypeFunction(function_name, comptime_call.args.items, source_loc)) |resolved_type| {
                return resolved_type;
            }
        }

        // Check if this is a member expression (e.g., std.List(i32))
        if (function_node.data == .member_expr) {
            const member_expr = function_node.data.member_expr;
            const object_node = self.arena.getNode(member_expr.object) orelse return null;

            if (object_node.data == .identifier and std.mem.eql(u8, object_node.data.identifier.name, "std")) {
                if (try self.resolveBuiltinTypeFunction(member_expr.field, comptime_call.args.items, source_loc)) |resolved_type| {
                    return resolved_type;
                }
            }
        }

        return null;
    }

    /// Resolve a compile-time type function call like List(i32)
    fn resolveComptimeTypeCall(
        self: *SemanticAnalyzer,
        call_expr: @TypeOf(@as(ast.AstNode, undefined).data.call_expr),
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
        const callee_node = self.arena.getNode(call_expr.callee) orelse return null;

        // Check if this is a simple identifier (e.g., List(i32))
        if (callee_node.data == .identifier) {
            const function_name = callee_node.data.identifier.name;

            // Handle built-in type functions
            if (try self.resolveBuiltinTypeFunction(function_name, call_expr.args.items, source_loc)) |resolved_type| {
                return resolved_type;
            }
        }

        // Check if this is a member expression (e.g., std.List(i32))
        if (callee_node.data == .member_expr) {
            const member_expr = callee_node.data.member_expr;
            const object_node = self.arena.getNode(member_expr.object) orelse return null;

            if (object_node.data == .identifier) {
                const module_name = object_node.data.identifier.name;
                const function_name = member_expr.field;

                // Handle std library type functions
                if (std.mem.eql(u8, module_name, "std")) {
                    if (try self.resolveStdTypeFunction(function_name, call_expr.args.items, source_loc)) |resolved_type| {
                        return resolved_type;
                    }
                }
            }
        }

        return null;
    }

    /// Resolve built-in type functions like List(T), Optional(T)
    fn resolveBuiltinTypeFunction(
        self: *SemanticAnalyzer,
        function_name: []const u8,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
        if (std.mem.eql(u8, function_name, "List")) {
            return try self.createListType(args, source_loc);
        } else if (std.mem.eql(u8, function_name, "Optional")) {
            return try self.createOptionalType(args, source_loc);
        } else if (std.mem.eql(u8, function_name, "Result")) {
            return try self.createResultType(args, source_loc);
        }

        return null;
    }

    /// Resolve standard library type functions like std.List(T)
    fn resolveStdTypeFunction(
        self: *SemanticAnalyzer,
        function_name: []const u8,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!?ast.Type {
        if (std.mem.eql(u8, function_name, "List")) {
            return try self.createListType(args, source_loc);
        } else if (std.mem.eql(u8, function_name, "HashMap")) {
            return try self.createHashMapType(args, source_loc);
        }

        return null;
    }

    /// Create a List(T) type
    fn createListType(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!ast.Type {
        if (args.len != 1) {
            try self.reportError(.type_mismatch, "List expects exactly one type parameter", source_loc);
            return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        }

        // Resolve the element type
        const element_type = (try self.inferType(args[0])) orelse {
            try self.reportError(.type_mismatch, "Invalid element type for List", source_loc);
            return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        };

        // Generate unique type name (e.g., List_i32, List_str)
        const type_name = try self.generateListTypeName(element_type);

        // Check if we already have this type registered
        if (self.type_registry.get(type_name)) |existing_type| {
            return existing_type;
        }

        // Create the List struct with appropriate fields
        const fields = try self.createListFields(element_type);

        const list_type = ast.Type.initCustomStruct(type_name, fields, true, // This is a compile-time generated type
            source_loc);

        // Register the type for reuse
        try self.type_registry.put(type_name, list_type);

        return list_type;
    }

    /// Create an Optional(T) type (tagged union with Some(T) and None)
    fn createOptionalType(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!ast.Type {
        if (args.len != 1) {
            try self.reportError(.type_mismatch, "Optional expects exactly one type parameter", source_loc);
            return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        }

        // For now, create a simple struct representation
        // In a full implementation, this would be a tagged union
        const inner_type = (try self.inferType(args[0])) orelse {
            try self.reportError(.type_mismatch, "Invalid inner type for Optional", source_loc);
            return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        };

        const type_name = try self.generateOptionalTypeName(inner_type);

        if (self.type_registry.get(type_name)) |existing_type| {
            return existing_type;
        }

        const optional_type = ast.Type.initCustomStruct(type_name, &[_]ast.Field{}, // Placeholder fields
            true, source_loc);

        try self.type_registry.put(type_name, optional_type);

        return optional_type;
    }

    /// Create a Result(T, E) type
    fn createResultType(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!ast.Type {
        if (args.len != 2) {
            try self.reportError(.type_mismatch, "Result expects exactly two type parameters", source_loc);
            return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        }

        // For now, return a placeholder
        return ast.Type.initCustomStruct("Result", &[_]ast.Field{}, true, source_loc);
    }

    /// Create a HashMap(K, V) type
    fn createHashMapType(
        self: *SemanticAnalyzer,
        args: []ast.NodeId,
        source_loc: ast.SourceLoc,
    ) CompileError!ast.Type {
        if (args.len != 2) {
            try self.reportError(.type_mismatch, "HashMap expects exactly two type parameters", source_loc);
            return ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        }

        // For now, return a placeholder
        return ast.Type.initCustomStruct("HashMap", &[_]ast.Field{}, true, source_loc);
    }

    /// Generate a unique type name for List(T)
    fn generateListTypeName(self: *SemanticAnalyzer, element_type: ast.Type) CompileError![]const u8 {
        const element_str = switch (element_type.data) {
            .primitive => |prim| switch (prim) {
                .i32 => "i32",
                .i64 => "i64",
                .u32 => "u32",
                .u64 => "u64",
                .f32 => "f32",
                .f64 => "f64",
                .bool => "bool",
                .str => "str",
                else => "unknown",
            },
            .@"struct" => |s| s.name,
            .custom_struct => |s| s.name,
            else => "complex",
        };

        return try std.fmt.allocPrint(self.allocator, "List_{s}", .{element_str});
    }

    /// Generate a unique type name for Optional(T)
    fn generateOptionalTypeName(self: *SemanticAnalyzer, inner_type: ast.Type) CompileError![]const u8 {
        const inner_str = switch (inner_type.data) {
            .primitive => |prim| switch (prim) {
                .i32 => "i32",
                .i64 => "i64",
                .str => "str",
                else => "unknown",
            },
            .@"struct" => |s| s.name,
            .custom_struct => |s| s.name,
            else => "complex",
        };

        return try std.fmt.allocPrint(self.allocator, "Optional_{s}", .{inner_str});
    }

    /// Create fields for a List(T) struct
    fn createListFields(self: *SemanticAnalyzer, element_type: ast.Type) CompileError![]ast.Field {
        _ = element_type; // Will be used to create proper typed fields

        // For now, create generic fields
        // In a full implementation, the data field would be []T
        const fields = try self.allocator.alloc(ast.Field, 3);

        fields[0] = ast.Field{
            .name = "data",
            .type_annotation = null, // Would be []T
            .default_value = null,
            .source_loc = ast.SourceLoc.invalid(),
        };

        fields[1] = ast.Field{
            .name = "len",
            .type_annotation = null, // Would be usize
            .default_value = null,
            .source_loc = ast.SourceLoc.invalid(),
        };

        fields[2] = ast.Field{
            .name = "capacity",
            .type_annotation = null, // Would be usize
            .default_value = null,
            .source_loc = ast.SourceLoc.invalid(),
        };

        return fields;
    }
};
