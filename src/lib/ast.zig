const std = @import("std");
const ErrorSystem = @import("error_system.zig");
const Token = @import("token.zig").Token;

// ============================================================================
// Source Location and Error Integration
// ============================================================================

/// Source location for precise error reporting
pub const SourceLoc = struct {
    file_path: []const u8,
    start_pos: usize,
    end_pos: usize,
    line: usize,
    column: usize,

    pub fn invalid() SourceLoc {
        return SourceLoc{
            .file_path = "<invalid>",
            .start_pos = 0,
            .end_pos = 0,
            .line = 0,
            .column = 0,
        };
    }

    pub fn single(file_path: []const u8, pos: usize, line: usize, column: usize) SourceLoc {
        return SourceLoc{
            .file_path = file_path,
            .start_pos = pos,
            .end_pos = pos,
            .line = line,
            .column = column,
        };
    }

    pub fn range(file_path: []const u8, start_pos: usize, end_pos: usize, start_line: usize, start_column: usize) SourceLoc {
        return SourceLoc{
            .file_path = file_path,
            .start_pos = start_pos,
            .end_pos = end_pos,
            .line = start_line,
            .column = start_column,
        };
    }

    pub fn toSourceSpan(self: SourceLoc) ErrorSystem.SourceSpan {
        return ErrorSystem.SourceSpan{
            .file_path = self.file_path,
            .start_pos = self.start_pos,
            .end_pos = self.end_pos,
            .line = self.line,
            .column = self.column,
        };
    }
};

// ============================================================================
// NodeId System for Memory-Efficient AST
// ============================================================================

/// Unique identifier for AST nodes
pub const NodeId = u32;
pub const INVALID_NODE_ID: NodeId = std.math.maxInt(NodeId);

/// Memory-efficient arena for AST nodes
pub const AstArena = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(AstNode),
    next_id: NodeId,

    pub fn init(allocator: std.mem.Allocator) AstArena {
        return AstArena{
            .allocator = allocator,
            .nodes = std.ArrayList(AstNode).init(allocator),
            .next_id = 0,
        };
    }

    pub fn deinit(self: *AstArena) void {
        // Clean up individual ArrayLists within nodes
        for (self.nodes.items) |*node| {
            self.deinitNode(node);
        }
        self.nodes.deinit();
    }
    
    fn deinitNode(self: *AstArena, node: *AstNode) void {
        _ = self; // suppress unused parameter warning
        switch (node.data) {
            .call_expr => |*call| call.args.deinit(),
            .function_decl => |*func| func.params.deinit(),
            .extern_fn_decl => |*extern_fn| extern_fn.params.deinit(),
            .struct_decl => |*struct_decl| struct_decl.fields.deinit(),
            .enum_decl => |*enum_decl| enum_decl.members.deinit(),  // Fix memory leak!
            .block => |*block| block.statements.deinit(),
            .match_expr => |*match| match.arms.deinit(),
            .match_compile_expr => |*match_compile| match_compile.arms.deinit(),
            .struct_init => |*struct_init| struct_init.fields.deinit(),
            .array_init => |*arr| arr.elements.deinit(),
            .struct_type_expr => |*struct_type| struct_type.fields.deinit(),
            .for_expr => |*for_expr| for_expr.captures.deinit(),
            .generic_type_expr => |*generic_type| generic_type.type_params.deinit(),
            else => {}, // No cleanup needed for other node types
        }
    }

    pub fn createNode(self: *AstArena, node: AstNode) !NodeId {
        const id = self.next_id;
        try self.nodes.append(node);
        self.next_id += 1;
        return id;
    }

    pub fn getNode(self: *const AstArena, id: NodeId) ?*AstNode {
        if (id >= self.nodes.items.len) return null;
        return &self.nodes.items[id];
    }

    pub fn getNodeConst(self: *const AstArena, id: NodeId) ?*const AstNode {
        if (id >= self.nodes.items.len) return null;
        return &self.nodes.items[id];
    }
};

// ============================================================================
// Binary Operators with Precedence and Associativity
// ============================================================================

pub const BinaryOp = enum {
    // Assignment (lowest precedence)
    assign,
    
    // Logical
    logical_or,
    logical_and,
    
    // Equality and comparison
    eq,
    ne,
    lt,
    le,
    gt,
    ge,
    
    // Arithmetic
    add,
    sub,
    mul,
    div,
    mod,
    power,
    concat, // String concatenation (++)
    
    // Bitwise
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr,
    
    // Special
    range,
    pipe,

    pub fn precedence(self: BinaryOp) u8 {
        return switch (self) {
            .assign => 1,
            .logical_or => 2,
            .logical_and => 3,
            .eq, .ne => 4,
            .lt, .le, .gt, .ge => 5,
            .bit_or => 6,
            .bit_xor => 7,
            .bit_and => 8,
            .shl, .shr => 9,
            .add, .sub, .concat => 10,
            .mul, .div, .mod => 11,
            .power => 12,
            .range, .pipe => 13,
        };
    }

    pub fn isRightAssociative(self: BinaryOp) bool {
        return switch (self) {
            .assign, .power => true,
            else => false,
        };
    }

    pub fn toString(self: BinaryOp) []const u8 {
        return switch (self) {
            .assign => "=",
            .logical_or => "or",
            .logical_and => "and",
            .eq => "==",
            .ne => "!=",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "mod",
            .power => "**",
            .concat => "++",
            .bit_and => "bAnd",
            .bit_or => "bOr",
            .bit_xor => "bXor",
            .shl => "bShiftLeft",
            .shr => "bShiftRight",
            .range => "..",
            .pipe => "|>",
        };
    }
};

pub const UnaryOp = enum {
    negate,
    not,
    bit_not,
    deref,
    address_of,

    pub fn toString(self: UnaryOp) []const u8 {
        return switch (self) {
            .negate => "-",
            .not => "!",
            .bit_not => "~",
            .deref => "*",
            .address_of => "&",
        };
    }
};

// ============================================================================
// Type System
// ============================================================================

pub const PrimitiveType = union(enum) {
    bool: void,
    i8: void,
    i16: void,
    i32: void,
    i64: void,
    u8: void,
    u16: void,
    u32: void,
    u64: void,
    usize: void,   // Platform-dependent unsigned integer
    isize: void,   // Platform-dependent signed integer
    f32: void,
    f64: void,
    char: void,
    str: void,     // Readonly string (const char*)
    strb: void,    // StringBuilder (mutable string builder)
    string: void,  // Legacy string type (kept for compatibility)
    void: void,
    type: void,    // The 'type' type for compile-time type manipulation
    noreturn: void, // Type for functions that never return
    module: void,  // Type for imported modules
};

pub const Type = struct {
    data: TypeData,
    source_loc: SourceLoc,

    const TypeData = union(enum) {
        primitive: PrimitiveType,
        pointer: *Type,
        array: struct {
            element_type: *Type,
            size: ?usize, // null for slices
        },
        @"struct": struct {
            name: []const u8,
            fields: []Field,
        },
        @"enum": struct {
            name: []const u8,
            members: []EnumMember,
        },
        function: struct {
            param_types: []Type,
            return_type: *Type,
        },
        optional: *Type,
        error_union: struct {
            error_set: []const u8, // simplified for now
            payload_type: *Type,
        },
        error_set: struct {
            name: []const u8,
            enumerants: [][]const u8,
        },
        // Compile-time types
        comptime_type: struct {
            resolved_type: ?*Type, // null until compile-time evaluation
            definition: NodeId, // the type definition expression
        },
        custom_struct: struct {
            name: []const u8,
            fields: []Field,
            is_comptime: bool, // whether this struct is created at compile time
        },
        unknown: void,
    };

    pub fn initPrimitive(primitive: PrimitiveType, source_loc: SourceLoc) Type {
        return Type{
            .data = .{ .primitive = primitive },
            .source_loc = source_loc,
        };
    }

    pub fn initComptimeType(definition: NodeId, source_loc: SourceLoc) Type {
        return Type{
            .data = .{ .comptime_type = .{ .resolved_type = null, .definition = definition } },
            .source_loc = source_loc,
        };
    }

    pub fn initCustomStruct(name: []const u8, fields: []Field, is_comptime: bool, source_loc: SourceLoc) Type {
        return Type{
            .data = .{ .custom_struct = .{ .name = name, .fields = fields, .is_comptime = is_comptime } },
            .source_loc = source_loc,
        };
    }

    pub fn initEnum(name: []const u8, members: []EnumMember, source_loc: SourceLoc) Type {
        return Type{
            .data = .{ .@"enum" = .{ .name = name, .members = members } },
            .source_loc = source_loc,
        };
    }

    pub fn initErrorSet(name: []const u8, enumerants: [][]const u8, source_loc: SourceLoc) Type {
        return Type{
            .data = .{ .error_set = .{ .name = name, .enumerants = enumerants } },
            .source_loc = source_loc,
        };
    }

    pub fn initError(error_set_name: []const u8, source_loc: SourceLoc) Type {
        return Type{
            .data = .{ .error_set = .{ .name = error_set_name, .enumerants = &[_][]const u8{} } },
            .source_loc = source_loc,
        };
    }

    pub fn isType(self: Type) bool {
        return switch (self.data) {
            .primitive => |p| p == .type,
            else => false,
        };
    }

    pub fn isComptimeType(self: Type) bool {
        return self.data == .comptime_type;
    }

    pub fn isCustomStruct(self: Type) bool {
        return self.data == .custom_struct;
    }

    pub fn isEnum(self: Type) bool {
        return self.data == .@"enum";
    }

    pub fn isPrimitive(self: Type) bool {
        return self.data == .primitive;
    }

    pub fn isInteger(self: Type) bool {
        return switch (self.data) {
            .primitive => |p| switch (p) {
                .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => true,
                else => false,
            },
            else => false,
        };
    }

    pub fn isFloat(self: Type) bool {
        return switch (self.data) {
            .primitive => |p| switch (p) {
                .f32, .f64 => true,
                else => false,
            },
            else => false,
        };
    }

    pub fn isNumeric(self: Type) bool {
        return self.isInteger() or self.isFloat();
    }

    pub fn isString(self: Type) bool {
        return switch (self.data) {
            .primitive => |p| switch (p) {
                .str => true,
                else => false,
            },
            else => false,
        };
    }

    pub fn toString(self: Type, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self.data) {
            .primitive => |p| switch (p) {
                .bool => "bool",
                .i8 => "i8",
                .i16 => "i16",
                .i32 => "i32",
                .i64 => "i64",
                .u8 => "u8",
                .u16 => "u16",
                .u32 => "u32",
                .u64 => "u64",
                .f32 => "f32",
                .f64 => "f64",
                .char => "char",
                .string => "string",
                .void => "void",
                .type => "type",
            },
            .pointer => |ptr_type| {
                const inner_str = try ptr_type.toString(allocator);
                defer allocator.free(inner_str);
                return try std.fmt.allocPrint(allocator, "*{s}", .{inner_str});
            },
            .optional => |opt_type| {
                const inner_str = try opt_type.toString(allocator);
                defer allocator.free(inner_str);
                return try std.fmt.allocPrint(allocator, "?{s}", .{inner_str});
            },
            .@"struct" => |s| try allocator.dupe(u8, s.name),
            .custom_struct => |s| try allocator.dupe(u8, s.name),
            .comptime_type => "comptime_type",
            .unknown => "unknown",
            else => "complex_type",
        };
    }
};

// ============================================================================
// Literal Values
// ============================================================================

pub const Literal = union(enum) {
    integer: struct {
        value: i64,
        type_hint: ?PrimitiveType = null,
    },
    float: struct {
        value: f64,
        type_hint: ?PrimitiveType = null,
    },
    string: struct {
        value: []const u8,
    },
    char: struct {
        value: u8,
    },
    bool_true: void,
    bool_false: void,
    enum_member: struct {
        name: []const u8, // Member name without the dot (e.g., "c", "javascript")
    },

    pub fn fromToken(token: Token) ?Literal {
        return switch (token) {
            .IntegerLiteral => |int_token| Literal{
                .integer = .{ .value = @intCast(int_token.value) },
            },
            .FloatLiteral => |float_token| Literal{
                .float = .{ .value = @floatCast(float_token.value) },
            },
            .StringLiteral => |str_token| Literal{
                .string = .{ .value = str_token.value },
            },
            .CharLiteral => |char_token| Literal{
                .char = .{ .value = char_token.char },
            },
            .True => Literal.bool_true,
            .False => Literal.bool_false,
            else => null,
        };
    }

    pub fn isNumeric(self: Literal) bool {
        return switch (self) {
            .integer, .float => true,
            else => false,
        };
    }

    pub fn isBool(self: Literal) bool {
        return switch (self) {
            .bool_true, .bool_false => true,
            else => false,
        };
    }

    pub fn getBoolValue(self: Literal) ?bool {
        return switch (self) {
            .bool_true => true,
            .bool_false => false,
            else => null,
        };
    }

    pub fn toString(self: Literal, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i.value}),
            .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f.value}),
            .string => |s| try std.fmt.allocPrint(allocator, "\"{s}\"", .{s.value}),
            .char => |c| try std.fmt.allocPrint(allocator, "'{c}'", .{c.value}),
            .bool_true => try allocator.dupe(u8, "true"),
            .bool_false => try allocator.dupe(u8, "false"),
            .enum_member => |member| try std.fmt.allocPrint(allocator, ".{s}", .{member.name}),
        };
    }
};

// ============================================================================
// Field Definition for Structs and Initializers
// ============================================================================

pub const Field = struct {
    name: []const u8,
    type_annotation: ?NodeId,
    default_value: ?NodeId,
    source_loc: SourceLoc,
};

pub const EnumMember = struct {
    name: []const u8,
    value: ?NodeId, // Optional explicit value (e.g., foo = 42)
    source_loc: SourceLoc,
};

// ============================================================================
// For Loop Capture Support  
// ============================================================================

pub const ForCapture = struct {
    name: []const u8,              // Variable name (could be "_" for ignored)
    capture_type: CaptureType,     // What kind of capture this is
    source_loc: SourceLoc,
};

pub const CaptureType = enum {
    value,   // |item| - captures the value
    index,   // |_, index| - captures the index (when second in a pair)
    ignored, // |_| - ignored capture
};

// ============================================================================
// Pattern Matching Support
// ============================================================================

pub const MatchPattern = union(enum) {
    literal: Literal,
    identifier: []const u8,
    wildcard: void, // _
    enum_member: []const u8, // .member (inferred enum member)
    comparison: struct {
        operator: std.meta.Tag(Token), // .LessThan, .GreaterThan, .EqualEqual, etc.
        value: NodeId,
    },
    range: struct {
        start: NodeId,
        end: NodeId,
        inclusive: bool,
    },
    tuple: []MatchPattern,
    array: []MatchPattern,
    guard: struct {
        pattern: *MatchPattern,
        condition: NodeId,
    },
};

pub const MatchArm = struct {
    pattern: MatchPattern,
    guard: ?NodeId, // Optional guard condition
    body: NodeId,
    source_loc: SourceLoc,
};

// Compile-time match arms for target-specific codegen
pub const CompileMatchArm = struct {
    target: CompileTarget,
    body: NodeId, // Block containing compile-time generation code
    source_loc: SourceLoc,
};

pub const CompileTarget = enum {
    c,
    javascript,
    
    pub fn fromString(str: []const u8) ?CompileTarget {
        if (std.mem.eql(u8, str, "c")) return .c;
        if (std.mem.eql(u8, str, "js") or std.mem.eql(u8, str, "javascript")) return .javascript;
        return null;
    }
    
    pub fn toString(self: CompileTarget) []const u8 {
        return switch (self) {
            .c => "c",
            .javascript => "javascript",
        };
    }
};

// ============================================================================
// Modern AST Node Definition
// ============================================================================

pub const AstNode = struct {
    data: NodeData,
    source_loc: SourceLoc,
    type_info: ?Type = null, // Filled in during semantic analysis

    const NodeData = union(enum) {
        // Literals and basic nodes
        literal: Literal,
        identifier: struct {
            name: []const u8,
        },
        
        // Expressions
        binary_expr: struct {
            op: BinaryOp,
            left: NodeId,
            right: NodeId,
        },
        unary_expr: struct {
            op: UnaryOp,
            operand: NodeId,
        },
        call_expr: struct {
            callee: NodeId,
            args: std.ArrayList(NodeId),
        },
        member_expr: struct {
            object: NodeId,
            field: []const u8,
        },
        index_expr: struct {
            object: NodeId,
            index: NodeId,
        },
        
        // Statements and declarations
        var_decl: struct {
            name: []const u8,
            type_annotation: ?NodeId,
            initializer: ?NodeId,
            is_mutable: bool,
        },
        function_decl: struct {
            name: []const u8,
            params: std.ArrayList(Parameter),
            return_type: ?NodeId,
            body: NodeId,
        },
        extern_fn_decl: struct {
            name: []const u8,
            params: std.ArrayList(Parameter),
            return_type: ?NodeId,
            compile_time_body: NodeId, // Block containing compile-time code generation
        },
        struct_decl: struct {
            name: []const u8,
            fields: std.ArrayList(Field),
            is_comptime: bool,
        },
        enum_decl: struct {
            name: []const u8,
            members: std.ArrayList(EnumMember),
        },
        type_decl: struct {
            name: []const u8,
            type_expr: NodeId, // Expression that evaluates to a type
        },
        import_decl: struct {
            module_path: []const u8, // Path to the module being imported
        },
        
        // Control flow
        if_expr: struct {
            condition: NodeId,
            then_branch: NodeId,
            else_branch: ?NodeId,
        },
        while_expr: struct {
            condition: NodeId,
            body: NodeId,
        },
        for_expr: struct {
            iterable: NodeId,              // The thing being iterated over
            captures: std.ArrayList(ForCapture), // The capture variables |value| or |value, index|
            body: NodeId,                  // The loop body
        },
        match_expr: struct {
            expression: NodeId,
            arms: std.ArrayList(MatchArm),
        },
        
        // Compound expressions
        block: struct {
            statements: std.ArrayList(NodeId),
        },
        struct_init: struct {
            type_name: ?[]const u8,
            fields: std.ArrayList(FieldInit),
        },
        array_init: struct {
            elements: std.ArrayList(NodeId),
        },
        range_expr: struct {
            start: ?NodeId,  // null for `..<end` syntax
            end: ?NodeId,    // null for `start..=` syntax (unbounded)
            inclusive: bool, // true for ..=, false for ..<
        },
        
        // Type expressions for compile-time type creation
        type_expr: struct {
            base_type: NodeId, // Can be a struct definition, primitive type, etc.
        },
        struct_type_expr: struct {
            fields: std.ArrayList(Field),
        },
        generic_type_expr: struct {
            base_type: NodeId,  // e.g., "List" from "List(T)"
            type_params: std.ArrayList(NodeId), // e.g., [T] from "List(T)"
        },
        
        // Compile-time constructs
        compile_target_expr: struct {
            // Evaluates to current compilation target (.c, .js, .wasm, etc.)
        },
        compile_insert_expr: struct {
            code: []const u8, // Raw code to insert during codegen
        },
        match_compile_expr: struct {
            target_expr: NodeId, // Usually @compile.target
            arms: std.ArrayList(CompileMatchArm),
        },
        
        // Control statements
        return_stmt: struct {
            value: ?NodeId,
        },
        break_stmt: void,
        continue_stmt: void,
        
        // Error handling
        error_node: struct {
            error_code: ErrorSystem.ErrorCode,
            message: []const u8,
        },
        try_expr: struct {
            expression: NodeId, // Expression that may return an error union
        },
        catch_expr: struct {
            expression: NodeId, // Expression that may return an error union (not tied to try)
            error_capture: ?[]const u8, // Optional error capture variable name |err|
            catch_body: ?NodeId, // Block to execute when error occurs, null for direct value fallback
            fallback_value: ?NodeId, // For `expr catch "default"` syntax
        },
        error_union_type: struct {
            error_set: ?NodeId, // Optional error set, null for inferred (anyerror)
            payload_type: NodeId, // The non-error type
        },
        error_literal: struct {
            name: []const u8, // Error name (e.g., "OutOfMemory")
        },
        error_set_decl: struct {
            name: []const u8, // Error set name (e.g., "FileError")
            errors: std.ArrayList([]const u8), // List of error names
        },
    };

    pub fn init(data: NodeData, source_loc: SourceLoc) AstNode {
        return AstNode{
            .data = data,
            .source_loc = source_loc,
        };
    }

    pub fn isExpression(self: *const AstNode) bool {
        return switch (self.data) {
            .literal,
            .identifier,
            .binary_expr,
            .unary_expr,
            .call_expr,
            .member_expr,
            .index_expr,
            .if_expr,
            .while_expr,
            .for_expr,
            .match_expr,
            .match_compile_expr,
            .compile_target_expr,
            .compile_insert_expr,
            .block,
            .struct_init,
            .array_init,
            .generic_type_expr,
            .try_expr,
            .catch_expr,
            .error_union_type,
            .error_literal => true,
            else => false,
        };
    }

    pub fn isStatement(self: *const AstNode) bool {
        return switch (self.data) {
            .var_decl,
            .function_decl,
            .extern_fn_decl,
            .struct_decl,
            .type_decl,
            .import_decl,
            .return_stmt,
            .break_stmt,
            .continue_stmt,
            .error_set_decl => true,
            else => false,
        };
    }
};

pub const Parameter = struct {
    name: []const u8,
    type_annotation: ?NodeId,
    default_value: ?NodeId,
    source_loc: SourceLoc,
};

pub const FieldInit = struct {
    name: []const u8,
    value: NodeId,
    source_loc: SourceLoc,
};

// ============================================================================
// AST Construction Helpers
// ============================================================================

pub fn createLiteralExpr(arena: *AstArena, source_loc: SourceLoc, literal: Literal) !NodeId {
    const node = AstNode.init(.{ .literal = literal }, source_loc);
    return arena.createNode(node);
}

pub fn createIdentifier(arena: *AstArena, source_loc: SourceLoc, name: []const u8) !NodeId {
    const node = AstNode.init(.{ .identifier = .{ .name = name } }, source_loc);
    return arena.createNode(node);
}

pub fn createBinaryExpr(arena: *AstArena, source_loc: SourceLoc, op: BinaryOp, left: NodeId, right: NodeId) !NodeId {
    const node = AstNode.init(.{ .binary_expr = .{ .op = op, .left = left, .right = right } }, source_loc);
    return arena.createNode(node);
}

pub fn createUnaryExpr(arena: *AstArena, source_loc: SourceLoc, op: UnaryOp, operand: NodeId) !NodeId {
    const node = AstNode.init(.{ .unary_expr = .{ .op = op, .operand = operand } }, source_loc);
    return arena.createNode(node);
}

pub fn createBlock(arena: *AstArena, source_loc: SourceLoc, statements: std.ArrayList(NodeId)) !NodeId {
    const node = AstNode.init(.{ .block = .{ .statements = statements } }, source_loc);
    return arena.createNode(node);
}

pub fn createErrorNode(arena: *AstArena, source_loc: SourceLoc, error_code: ErrorSystem.ErrorCode, message: []const u8) !NodeId {
    const node = AstNode.init(.{ .error_node = .{ .error_code = error_code, .message = message } }, source_loc);
    return arena.createNode(node);
}

pub fn createStructDecl(arena: *AstArena, source_loc: SourceLoc, name: []const u8, fields: std.ArrayList(Field), is_comptime: bool) !NodeId {
    const node = AstNode.init(.{ .struct_decl = .{ .name = name, .fields = fields, .is_comptime = is_comptime } }, source_loc);
    return arena.createNode(node);
}

pub fn createTypeDecl(arena: *AstArena, source_loc: SourceLoc, name: []const u8, type_expr: NodeId) !NodeId {
    const node = AstNode.init(.{ .type_decl = .{ .name = name, .type_expr = type_expr } }, source_loc);
    return arena.createNode(node);
}

pub fn createTypeExpr(arena: *AstArena, source_loc: SourceLoc, base_type: NodeId) !NodeId {
    const node = AstNode.init(.{ .type_expr = .{ .base_type = base_type } }, source_loc);
    return arena.createNode(node);
}

pub fn createStructTypeExpr(arena: *AstArena, source_loc: SourceLoc, fields: std.ArrayList(Field)) !NodeId {
    const node = AstNode.init(.{ .struct_type_expr = .{ .fields = fields } }, source_loc);
    return arena.createNode(node);
}

pub fn createExternFnDecl(arena: *AstArena, source_loc: SourceLoc, name: []const u8, params: std.ArrayList(Parameter), return_type: ?NodeId, compile_time_body: NodeId) !NodeId {
    const node = AstNode.init(.{ .extern_fn_decl = .{ .name = name, .params = params, .return_type = return_type, .compile_time_body = compile_time_body } }, source_loc);
    return arena.createNode(node);
}

pub fn createCompileTargetExpr(arena: *AstArena, source_loc: SourceLoc) !NodeId {
    const node = AstNode.init(.{ .compile_target_expr = .{} }, source_loc);
    return arena.createNode(node);
}

pub fn createCompileInsertExpr(arena: *AstArena, source_loc: SourceLoc, code: []const u8) !NodeId {
    const node = AstNode.init(.{ .compile_insert_expr = .{ .code = code } }, source_loc);
    return arena.createNode(node);
}

pub fn createMatchCompileExpr(arena: *AstArena, source_loc: SourceLoc, target_expr: NodeId, arms: std.ArrayList(CompileMatchArm)) !NodeId {
    const node = AstNode.init(.{ .match_compile_expr = .{ .target_expr = target_expr, .arms = arms } }, source_loc);
    return arena.createNode(node);
}

pub fn createTryExpr(arena: *AstArena, source_loc: SourceLoc, expression: NodeId) !NodeId {
    const node = AstNode.init(.{ .try_expr = .{ .expression = expression } }, source_loc);
    return arena.createNode(node);
}

pub fn createCatchExpr(arena: *AstArena, source_loc: SourceLoc, expression: NodeId, error_capture: ?[]const u8, catch_body: ?NodeId, fallback_value: ?NodeId) !NodeId {
    const node = AstNode.init(.{ .catch_expr = .{ .expression = expression, .error_capture = error_capture, .catch_body = catch_body, .fallback_value = fallback_value } }, source_loc);
    return arena.createNode(node);
}

pub fn createErrorSetDecl(arena: *AstArena, source_loc: SourceLoc, name: []const u8, errors: std.ArrayList([]const u8)) !NodeId {
    const node = AstNode.init(.{ .error_set_decl = .{ .name = name, .errors = errors } }, source_loc);
    return arena.createNode(node);
}

pub fn createErrorUnionType(arena: *AstArena, source_loc: SourceLoc, error_set: ?NodeId, payload_type: NodeId) !NodeId {
    const node = AstNode.init(.{ .error_union_type = .{ .error_set = error_set, .payload_type = payload_type } }, source_loc);
    return arena.createNode(node);
}

pub fn createErrorLiteral(arena: *AstArena, source_loc: SourceLoc, name: []const u8) !NodeId {
    const node = AstNode.init(.{ .error_literal = .{ .name = name } }, source_loc);
    return arena.createNode(node);
}

// ============================================================================
// AST Visitor Pattern for Traversal
// ============================================================================

pub const AstVisitor = struct {
    visit_fn: *const fn (visitor: *AstVisitor, arena: *const AstArena, node_id: NodeId) anyerror!void,
    context: ?*anyopaque = null,

    pub fn visit(self: *AstVisitor, arena: *const AstArena, node_id: NodeId) !void {
        try self.visit_fn(self, arena, node_id);
        
        // Visit children
        const node = arena.getNodeConst(node_id) orelse return;
        switch (node.data) {
            .binary_expr => |binary| {
                try self.visit(arena, binary.left);
                try self.visit(arena, binary.right);
            },
            .unary_expr => |unary| {
                try self.visit(arena, unary.operand);
            },
            .call_expr => |call| {
                try self.visit(arena, call.callee);
                for (call.args.items) |arg| {
                    try self.visit(arena, arg);
                }
            },
            .block => |block| {
                for (block.statements.items) |stmt| {
                    try self.visit(arena, stmt);
                }
            },
            .if_expr => |if_expr| {
                try self.visit(arena, if_expr.condition);
                try self.visit(arena, if_expr.then_branch);
                if (if_expr.else_branch) |else_branch| {
                    try self.visit(arena, else_branch);
                }
            },
            .var_decl => |var_decl| {
                if (var_decl.type_annotation) |type_node| {
                    try self.visit(arena, type_node);
                }
                if (var_decl.initializer) |init| {
                    try self.visit(arena, init);
                }
            },
            .function_decl => |func_decl| {
                if (func_decl.return_type) |ret_type| {
                    try self.visit(arena, ret_type);
                }
                try self.visit(arena, func_decl.body);
            },
            .extern_fn_decl => |extern_fn| {
                if (extern_fn.return_type) |ret_type| {
                    try self.visit(arena, ret_type);
                }
                try self.visit(arena, extern_fn.compile_time_body);
            },
            .match_compile_expr => |match_compile| {
                try self.visit(arena, match_compile.target_expr);
                for (match_compile.arms.items) |arm| {
                    try self.visit(arena, arm.body);
                }
            },
            .try_expr => |try_expr| {
                try self.visit(arena, try_expr.expression);
            },
            .catch_expr => |catch_expr| {
                try self.visit(arena, catch_expr.try_expression);
                try self.visit(arena, catch_expr.catch_body);
            },
            .error_union_type => |error_union| {
                if (error_union.error_set) |error_set| {
                    try self.visit(arena, error_set);
                }
                try self.visit(arena, error_union.payload_type);
            },
            else => {}, // Leaf nodes
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "AST Arena basic operations" {
    const testing = std.testing;
    var arena = AstArena.init(testing.allocator);
    defer arena.deinit();

    // Test creating a literal expression
    const literal = Literal{ .integer = .{ .value = 42 } };
    const lit_id = try createLiteralExpr(&arena, SourceLoc.invalid(), literal);

    const node = arena.getNode(lit_id).?;
    try testing.expect(node.data == .literal);
    try testing.expect(node.data.literal.integer.value == 42);
}

test "Type system basics" {
    const testing = std.testing;

    // Test primitive types
    const int_type = Type.initPrimitive(.{ .i32 = {} }, SourceLoc.invalid());
    try testing.expect(int_type.isPrimitive());
    try testing.expect(int_type.isInteger());
    try testing.expect(int_type.isNumeric());
    try testing.expect(!int_type.isFloat());

    const float_type = Type.initPrimitive(.{ .f64 = {} }, SourceLoc.invalid());
    try testing.expect(float_type.isPrimitive());
    try testing.expect(!float_type.isInteger());
    try testing.expect(float_type.isNumeric());
    try testing.expect(float_type.isFloat());

    const bool_type = Type.initPrimitive(.{ .bool = {} }, SourceLoc.invalid());
    try testing.expect(bool_type.isPrimitive());
    try testing.expect(!bool_type.isNumeric());
}

test "Binary operator precedence" {
    const testing = std.testing;

    // Test precedence ordering
    try testing.expect(BinaryOp.assign.precedence() < BinaryOp.logical_or.precedence());
    try testing.expect(BinaryOp.logical_or.precedence() < BinaryOp.logical_and.precedence());
    try testing.expect(BinaryOp.logical_and.precedence() < BinaryOp.eq.precedence());
    try testing.expect(BinaryOp.add.precedence() < BinaryOp.mul.precedence());
    try testing.expect(BinaryOp.mul.precedence() < BinaryOp.power.precedence());

    // Test associativity
    try testing.expect(BinaryOp.assign.isRightAssociative());
    try testing.expect(BinaryOp.power.isRightAssociative());
    try testing.expect(!BinaryOp.add.isRightAssociative());
    try testing.expect(!BinaryOp.mul.isRightAssociative());
}

test "Literal creation from tokens" {
    const testing = std.testing;

    // Test integer literal
    const int_token = Token{ .IntegerLiteral = .{ .pos = 0, .value = 123 } };
    const int_literal = Literal.fromToken(int_token).?;
    try testing.expect(int_literal == .integer);
    try testing.expect(int_literal.integer.value == 123);
    try testing.expect(int_literal.isNumeric());

    // Test boolean literal
    const true_token = Token{ .True = .{ .pos = 0 } };
    const bool_literal = Literal.fromToken(true_token).?;
    try testing.expect(bool_literal == .bool_true);
    try testing.expect(bool_literal.isBool());
    try testing.expect(bool_literal.getBoolValue().? == true);

    // Test string literal
    const str_token = Token{ .StringLiteral = .{ .pos = 0, .value = "hello" } };
    const str_literal = Literal.fromToken(str_token).?;
    try testing.expect(str_literal == .string);
    try testing.expect(std.mem.eql(u8, str_literal.string.value, "hello"));
}
