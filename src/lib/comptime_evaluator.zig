const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

/// Compile-time value that can be computed during compilation
pub const ComptimeValue = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    bool: bool,
    type: ast.Type,
    none: void,
    some: ast.NodeId, // NodeId of the wrapped value

    pub fn equals(self: ComptimeValue, other: ComptimeValue) bool {
        return switch (self) {
            .int => |i| switch (other) {
                .int => |j| i == j,
                else => false,
            },
            .float => |f| switch (other) {
                .float => |g| f == g,
                else => false,
            },
            .string => |s| switch (other) {
                .string => |t| std.mem.eql(u8, s, t),
                else => false,
            },
            .bool => |b| switch (other) {
                .bool => |c| b == c,
                else => false,
            },
            .type => false, // Type comparison is complex, skip for now
            .none => switch (other) {
                .none => true,
                else => false,
            },
            .some => |s| switch (other) {
                .some => |t| s == t, // Compare NodeIds
                else => false,
            },
        };
    }
};

/// Arguments to compile-time function calls
pub const ComptimeArg = union(enum) {
    type_arg: ast.Type,
    value_arg: ComptimeValue,

    pub fn fromType(type_info: ast.Type) ComptimeArg {
        return ComptimeArg{ .type_arg = type_info };
    }

    pub fn fromValue(value: ComptimeValue) ComptimeArg {
        return ComptimeArg{ .value_arg = value };
    }
};

/// Key for caching compile-time function results
const ComptimeCallKey = struct {
    function_name: []const u8,
    args_hash: u64, // Hash of the arguments

    pub fn hash(self: ComptimeCallKey) u64 {
        var hasher = std.hash_map.DefaultHasher.init();
        hasher.update(self.function_name);
        hasher.update(std.mem.asBytes(&self.args_hash));
        return hasher.final();
    }

    pub fn eql(self: ComptimeCallKey, other: ComptimeCallKey) bool {
        return std.mem.eql(u8, self.function_name, other.function_name) and
            self.args_hash == other.args_hash;
    }
};

/// Environment for compile-time execution
const ComptimeEnvironment = struct {
    bindings: std.StringHashMap(ComptimeValue),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) ComptimeEnvironment {
        return ComptimeEnvironment{
            .bindings = std.StringHashMap(ComptimeValue).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ComptimeEnvironment) void {
        self.bindings.deinit();
    }

    pub fn bind(self: *ComptimeEnvironment, name: []const u8, value: ComptimeValue) !void {
        try self.bindings.put(name, value);
    }

    pub fn lookup(self: *const ComptimeEnvironment, name: []const u8) ?ComptimeValue {
        return self.bindings.get(name);
    }
};

/// The main compile-time evaluator
pub const ComptimeEvaluator = struct {
    allocator: std.mem.Allocator,
    arena: *const ast.AstArena,
    semantic_analyzer: *SemanticAnalyzer.SemanticAnalyzer,
    type_cache: std.HashMap(ComptimeCallKey, ast.Type, ComptimeCallContext, std.hash_map.default_max_load_percentage),

    const ComptimeCallContext = struct {
        pub fn hash(self: @This(), key: ComptimeCallKey) u64 {
            _ = self;
            return key.hash();
        }

        pub fn eql(self: @This(), a: ComptimeCallKey, b: ComptimeCallKey) bool {
            _ = self;
            return a.eql(b);
        }
    };

    pub fn init(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *SemanticAnalyzer.SemanticAnalyzer) ComptimeEvaluator {
        return ComptimeEvaluator{
            .allocator = allocator,
            .arena = arena,
            .semantic_analyzer = semantic_analyzer,
            .type_cache = std.HashMap(ComptimeCallKey, ast.Type, ComptimeCallContext, std.hash_map.default_max_load_percentage).init(allocator),
        };
    }

    pub fn deinit(self: *ComptimeEvaluator) void {
        self.type_cache.deinit();
    }

    /// Evaluate a function call that returns a type (like std.List(i32))
    pub fn evaluateTypeFunction(self: *ComptimeEvaluator, function_name: []const u8, args: []ComptimeArg) !ast.Type {
        // Create cache key
        const args_hash = self.hashArgs(args);
        const key = ComptimeCallKey{
            .function_name = function_name,
            .args_hash = args_hash,
        };

        // Check cache first
        if (self.type_cache.get(key)) |cached_type| {
            return cached_type;
        }

        // Find the function definition
        const func_node_id = self.findTypeFunctionDefinition(function_name) orelse {
            return error.TypeFunctionNotFound;
        };

        // Execute the function
        const result_type = try self.executeTypeFunction(func_node_id, args);

        // Cache the result
        try self.type_cache.put(key, result_type);

        return result_type;
    }

    /// Execute a compile-time type function
    fn executeTypeFunction(self: *ComptimeEvaluator, func_node_id: ast.NodeId, args: []ComptimeArg) !ast.Type {
        const func_node = self.arena.getNodeConst(func_node_id) orelse return error.InvalidFunction;

        if (func_node.data != .function_decl) {
            return error.NotAFunction;
        }

        const func_decl = func_node.data.function_decl;

        // Create execution environment
        var env = ComptimeEnvironment.init(self.allocator);
        defer env.deinit();

        // Bind parameters to arguments
        try self.bindParameters(&env, func_decl, args);

        // Execute the function body
        const result = try self.evaluateComptimeExpression(&env, func_decl.body);

        return switch (result) {
            .type => |t| t,
            else => error.FunctionDidNotReturnType,
        };
    }

    /// Evaluate a compile-time expression in the given environment
    fn evaluateComptimeExpression(self: *ComptimeEvaluator, env: *ComptimeEnvironment, node_id: ast.NodeId) anyerror!ComptimeValue {
        const node = self.arena.getNodeConst(node_id) orelse return error.InvalidNode;

        return switch (node.data) {
            .literal => |literal| try self.evaluateLiteral(literal),
            .identifier => |identifier| self.evaluateIdentifier(env, identifier.name),
            .return_stmt => |return_stmt| {
                if (return_stmt.value) |value_id| {
                    return try self.evaluateComptimeExpression(env, value_id);
                } else {
                    return error.EmptyReturn;
                }
            },
            .struct_decl => |struct_decl| try self.createStructType(env, struct_decl),
            .block => |block| {
                // Execute all statements, return the last expression
                var last_value: ?ComptimeValue = null;
                for (block.statements.items) |stmt_id| {
                    last_value = try self.evaluateComptimeExpression(env, stmt_id);
                }
                return last_value orelse ComptimeValue{ .type = ast.Type.initPrimitive(.{ .void = {} }, node.source_loc) };
            },
            else => {
                std.debug.print("Unsupported compile-time expression: {}\n", .{node.data});
                return error.UnsupportedComptimeExpression;
            },
        };
    }

    /// Evaluate a literal to a compile-time value
    fn evaluateLiteral(self: *ComptimeEvaluator, literal: ast.Literal) !ComptimeValue {
        _ = self;
        return switch (literal) {
            .integer => |int_lit| ComptimeValue{ .int = int_lit.value },
            .float => |float_lit| ComptimeValue{ .float = float_lit.value },
            .string => |str_lit| ComptimeValue{ .string = str_lit.value },
            .bool_true => ComptimeValue{ .bool = true },
            .bool_false => ComptimeValue{ .bool = false },
            else => error.UnsupportedLiteral,
        };
    }

    /// Evaluate an identifier in the environment
    fn evaluateIdentifier(self: *ComptimeEvaluator, env: *ComptimeEnvironment, name: []const u8) !ComptimeValue {

        // First check environment bindings
        if (env.lookup(name)) |value| {
            return value;
        }

        // Then check for built-in types
        return self.lookupBuiltinType(name);
    }

    /// Look up built-in types like i32, str, etc.
    fn lookupBuiltinType(self: *ComptimeEvaluator, name: []const u8) !ComptimeValue {
        _ = self;

        const type_info = if (std.mem.eql(u8, name, "i32"))
            ast.Type.initPrimitive(.{ .i32 = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "i64"))
            ast.Type.initPrimitive(.{ .i64 = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "u32"))
            ast.Type.initPrimitive(.{ .u32 = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "u64"))
            ast.Type.initPrimitive(.{ .u64 = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "f32"))
            ast.Type.initPrimitive(.{ .f32 = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "f64"))
            ast.Type.initPrimitive(.{ .f64 = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "bool"))
            ast.Type.initPrimitive(.{ .bool = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "str"))
            ast.Type.initPrimitive(.{ .str = {} }, ast.SourceLoc.invalid())
        else if (std.mem.eql(u8, name, "void"))
            ast.Type.initPrimitive(.{ .void = {} }, ast.SourceLoc.invalid())
        else
            return error.UnknownType;

        return ComptimeValue{ .type = type_info };
    }

    /// Create a struct type from a compile-time struct declaration
    fn createStructType(self: *ComptimeEvaluator, env: *ComptimeEnvironment, struct_decl: anytype) !ComptimeValue {
        _ = self; // May be used later for name generation
        _ = env; // May be used later for field evaluation

        // Create a custom struct type
        const struct_type = ast.Type.initCustomStruct(struct_decl.name, struct_decl.fields.items, true, // This is a compile-time generated struct
            ast.SourceLoc.invalid());

        // For now, just return the type without generating unique names
        // In a full implementation, this would include type parameter info
        return ComptimeValue{ .type = struct_type };
    }

    /// Generate unique name for instantiated types (e.g., List_i32, Optional_str)
    fn generateUniqueStructName(self: *ComptimeEvaluator, base_name: []const u8) ![]const u8 {
        // For now, just return the base name
        // In a full implementation, this would include type parameter info
        return try self.allocator.dupe(u8, base_name);
    }

    /// Bind function parameters to arguments
    fn bindParameters(self: *ComptimeEvaluator, env: *ComptimeEnvironment, func_decl: anytype, args: []ComptimeArg) !void {
        _ = self;

        if (func_decl.params.items.len != args.len) {
            return error.ArgumentCountMismatch;
        }

        for (func_decl.params.items, args) |param, arg| {
            const value = switch (arg) {
                .type_arg => |t| ComptimeValue{ .type = t },
                .value_arg => |v| v,
            };

            try env.bind(param.name, value);
        }
    }

    /// Find a type function definition by name
    fn findTypeFunctionDefinition(self: *ComptimeEvaluator, name: []const u8) ?ast.NodeId {
        // This would search through the AST for function definitions
        // For now, return null as a placeholder
        _ = self;
        _ = name;
        return null;
    }

    /// Hash arguments for caching
    fn hashArgs(self: *ComptimeEvaluator, args: []ComptimeArg) u64 {
        _ = self;
        var hasher = std.hash_map.DefaultHasher.init();

        for (args) |arg| {
            switch (arg) {
                .type_arg => |t| {
                    // Hash type info - simplified for now
                    hasher.update(std.mem.asBytes(&t.data));
                },
                .value_arg => |v| {
                    switch (v) {
                        .int => |i| hasher.update(std.mem.asBytes(&i)),
                        .float => |f| hasher.update(std.mem.asBytes(&f)),
                        .string => |s| hasher.update(s),
                        .bool => |b| hasher.update(std.mem.asBytes(&b)),
                        .type => |t| hasher.update(std.mem.asBytes(&t.data)),
                    }
                },
            }
        }

        return hasher.final();
    }
};

/// Built-in type functions that can be used like std.List(T)
pub const BuiltinTypeFunctions = struct {
    /// Create a List type for the given element type
    pub fn createListType(element_type: ast.Type, allocator: std.mem.Allocator) !ast.Type {
        _ = allocator; // May be used for field allocation later

        // Create fields for the List struct
        const fields = [_]ast.Field{
            ast.Field{
                .name = "data",
                .type_annotation = null, // Would be []T in a full implementation
                .default_value = null,
                .source_loc = ast.SourceLoc.invalid(),
            },
            ast.Field{
                .name = "len",
                .type_annotation = null, // Would be usize
                .default_value = null,
                .source_loc = ast.SourceLoc.invalid(),
            },
            ast.Field{
                .name = "capacity",
                .type_annotation = null, // Would be usize
                .default_value = null,
                .source_loc = ast.SourceLoc.invalid(),
            },
        };

        // Create the list type name (e.g., List_i32)
        const type_name = switch (element_type.data) {
            .primitive => |p| switch (p) {
                .i32 => "List_i32",
                .i64 => "List_i64",
                .str => "List_str",
                else => "List_unknown",
            },
            else => "List_complex",
        };

        return ast.Type.initCustomStruct(type_name, &fields, true, // This is compile-time generated
            ast.SourceLoc.invalid());
    }

    /// Create an Optional type for the given inner type
    pub fn createOptionalType(inner_type: ast.Type, allocator: std.mem.Allocator) !ast.Type {
        _ = allocator;
        _ = inner_type;

        // For now, return a placeholder
        // In a full implementation, this would create a tagged union
        return ast.Type.initCustomStruct("Optional", &[_]ast.Field{}, true, ast.SourceLoc.invalid());
    }
};
