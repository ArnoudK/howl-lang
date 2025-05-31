const std = @import("std");
const Ast = @import("ast_simple.zig");

const SemanticError = error{
    InvalidIntegerSuffix,
    InvalidIntOctalDigit,
    InvalidIntBinaryDigit,
    InvalidIntHexDigit,
    InvalidIntBaseTenDigit,
    OutOfMemory,
    TypeMismatch,
    UndefinedVariable,
    UndefinedFunction,
    InvalidFunctionCall,
    InvalidMemberAccess,
    InvalidOperator,
    InvalidAssignment,
    ConstReassignment,
    InvalidImport,
    DuplicateSymbol,
    RecursiveImport,
    InvalidType,
    IncompatibleTypes,
    InvalidOperand,
};

/// IR types that will be used for code generation
pub const IrType = enum {
    Void,
    Bool,
    I32,
    I64,
    U8,
    U32,
    U64,
    String,
    Slice,
    Struct,
    Function,
    Unknown,
};

/// Symbol type for tracking variables, functions, etc.
const SymbolType = enum {
    Variable,
    Constant,
    Function,
    Parameter,
    Type,
    Import,
};

/// Symbol entry for the symbol table
const Symbol = struct {
    name: []const u8,
    symbol_type: SymbolType,
    ir_type: IrType,
    is_public: bool,
    value: ?*IrNode = null,
    initialized: bool = false,
    mutable: bool = true,
    // Add a field for function parameters
    parameters: ?std.ArrayList(IrVariable) = null,
};

/// Symbol table for tracking scope
const SymbolTable = struct {
    parent: ?*SymbolTable,
    symbols: std.StringHashMap(Symbol),
    allocator: std.mem.Allocator,
    // Add a field to track function context
    function_context: ?FunctionContext = null,

    fn init(allocator: std.mem.Allocator, parent: ?*SymbolTable) SymbolTable {
        return SymbolTable{
            .parent = parent,
            .symbols = std.StringHashMap(Symbol).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *SymbolTable) void {
        self.symbols.deinit();
    }

    fn define(self: *SymbolTable, symbol: Symbol) !void {
        // Check if symbol is already defined in current scope
        if (self.symbols.contains(symbol.name)) {
            return SemanticError.DuplicateSymbol;
        }
        try self.symbols.put(symbol.name, symbol);
    }

    fn resolve(self: *SymbolTable, name: []const u8) ?Symbol {
        if (self.symbols.get(name)) |symbol| {
            return symbol;
        }

        // Not in current scope, try parent scope
        if (self.parent) |parent| {
            return parent.resolve(name);
        }

        return null;
    }
};

/// Function context for return type checking
const FunctionContext = struct {
    name: []const u8,
    return_type: IrType,
};

/// IR Node types - similar to AST but with semantic information
pub const IrNodeType = enum {
    Program,
    IntLiteral,
    FloatLiteral,
    BoolLiteral,
    StringLiteral,
    Identifier,
    Variable,
    Constant,
    BinaryOp,
    UnaryOp,
    Call,
    Block,
    Function,
    Return,
    Import,
    Member,
    StructDef,
    TypeCast,
    Slice,
    Assignment,
};

// Move struct definitions outside of the union

/// Integer literal with type information
pub const IrIntLiteral = struct {
    value: i128,
    type: IrType,
};

/// Float literal with type information
pub const IrFloatLiteral = struct {
    value: f128,
    type: IrType,
};

/// Identifier with symbol reference
pub const IrIdentifier = struct {
    name: []const u8,
    symbol: Symbol,
};

/// Member expression (object.property)
pub const IrMember = struct {
    object: *IrNode,
    property: []const u8,
    result_type: IrType,
};

/// Type cast expression
pub const IrTypeCast = struct {
    expr: *IrNode,
    target_type: IrType,
};

/// Binary operation with type information
pub const IrBinaryOp = struct {
    op: Ast.BinaryOp,
    left: *IrNode,
    right: *IrNode,
    result_type: IrType,
};

/// Unary operation with type information
pub const IrUnaryOp = struct {
    op: Ast.UnaryOp,
    operand: *IrNode,
    result_type: IrType,
};

/// Function call with resolved arguments
pub const IrCall = struct {
    callee: *IrNode,
    arguments: std.ArrayList(*IrNode),
    result_type: IrType,
};

/// Variable declaration with type information
pub const IrVariable = struct {
    name: []const u8,
    value: *IrNode,
    var_type: IrType,
    is_public: bool,
    mutable: bool,
};

/// Function declaration with type information
pub const IrFunction = struct {
    name: []const u8,
    parameters: std.ArrayList(IrVariable),
    return_type: IrType,
    body: *IrNode,
    is_public: bool,
};

/// Block of statements with scope
pub const IrBlock = struct {
    statements: std.ArrayList(*IrNode),
    scope: *SymbolTable,
};

/// Structure definition
pub const IrStructDef = struct {
    name: []const u8,
    fields: std.ArrayList(IrVariable),
    methods: std.ArrayList(IrFunction),
    memory_type: Ast.MemoryManagementType,
};

/// Slice type
pub const IrSlice = struct {
    element_type: IrType,
    items: *IrNode,
};

/// Assignment expression
pub const IrAssignment = struct {
    target: *IrNode,
    value: *IrNode,
    type: IrType,
};

/// IR Node for intermediate representation
pub const IrNode = union(IrNodeType) {
    Program: std.ArrayList(*IrNode),
    IntLiteral: IrIntLiteral,
    FloatLiteral: IrFloatLiteral,
    BoolLiteral: bool,
    StringLiteral: []const u8,
    Identifier: IrIdentifier,
    Variable: IrVariable,
    Constant: IrVariable,
    BinaryOp: IrBinaryOp,
    UnaryOp: IrUnaryOp,
    Call: IrCall,
    Block: IrBlock,
    Function: IrFunction,
    Return: *IrNode,
    Import: []const u8,
    Member: IrMember,
    StructDef: IrStructDef,
    TypeCast: IrTypeCast,
    Slice: IrSlice,
    Assignment: IrAssignment,
};

pub const SemanticAnalyzerErrorMessage = struct {
    message: []const u8,
    node: *Ast.AstNode,
    err: SemanticError,
};

pub const SemanticAnalyzer = struct {
    parent_allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    ast: *Ast.AstNode, // Changed from Ast.Parser to Ast.AstNode

    // New fields for semantic analysis
    global_scope: *SymbolTable,
    current_scope: ?*SymbolTable,
    ir_root: *IrNode,

    // Error tracking with enhanced struct
    errors: *std.ArrayList(SemanticAnalyzerErrorMessage),

    pub fn init(
        parent_allocator: std.mem.Allocator,
        ast: *Ast.AstNode, // Changed from Ast.Parser to Ast.AstNode
    ) !SemanticAnalyzer {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        const allocator = arena.allocator();

        return SemanticAnalyzer{
            .parent_allocator = parent_allocator,
            .arena = arena,
            .allocator = allocator,
            .ast = ast,
            // other fields are defined in the analyze function
            .global_scope = undefined,
            .current_scope = undefined,
            .ir_root = undefined,
            .errors = undefined,
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        self.arena.deinit();
    }

    /// Create a new IR node
    fn createIrNode(self: *SemanticAnalyzer, comptime node_type: IrNodeType, init_value: anytype) !*IrNode {
        const node = try self.allocator.create(IrNode);
        node.* = switch (node_type) {
            .Program => IrNode{ .Program = init_value },
            .IntLiteral => IrNode{ .IntLiteral = init_value },
            .FloatLiteral => IrNode{ .FloatLiteral = init_value },
            .BoolLiteral => IrNode{ .BoolLiteral = init_value },
            .StringLiteral => IrNode{ .StringLiteral = init_value },
            .Identifier => IrNode{ .Identifier = init_value },
            .Variable => IrNode{ .Variable = init_value },
            .Constant => IrNode{ .Constant = init_value },
            .BinaryOp => IrNode{ .BinaryOp = init_value },
            .UnaryOp => IrNode{ .UnaryOp = init_value },
            .Call => IrNode{ .Call = init_value },
            .Block => IrNode{ .Block = init_value },
            .Function => IrNode{ .Function = init_value },
            .Return => IrNode{ .Return = init_value },
            .Import => IrNode{ .Import = init_value },
            .Member => IrNode{ .Member = init_value },
            .StructDef => IrNode{ .StructDef = init_value },
            .TypeCast => IrNode{ .TypeCast = init_value },
            .Slice => IrNode{ .Slice = init_value },
            .Assignment => IrNode{ .Assignment = init_value },
        };
        return node;
    }

    pub fn print(self: *SemanticAnalyzer) void {
        std.debug.print("Semantic Analysis Results:\n", .{});
        std.debug.print("  Errors: {d}\n", .{self.errors.items.len});
        for (self.errors.items) |error_info| {
            std.debug.print("    Error: {} at node type: {s}\n", .{
                error_info.err,
                @tagName(error_info.node.*),
            });
        }

        // Print the IR with proper indentation
        std.debug.print("\nIntermediate Representation:\n", .{});
        self.printIrNode(self.ir_root, 0);
    }

    fn printIrNode(self: *SemanticAnalyzer, ir_node: *IrNode, indent: usize) void {
        const spaces = " " ** 64; // Create a string of spaces for indentation
        const indent_str = spaces[0..@min(indent, spaces.len)];

        switch (ir_node.*) {
            .Program => |program| {
                std.debug.print("{s}Program with {d} statements\n", .{ indent_str, program.items.len });
                for (program.items) |statement| {
                    self.printIrNode(statement, indent + 2);
                }
            },

            .IntLiteral => |lit| {
                std.debug.print("{s}IntLiteral: {d} (type: {s})\n", .{ indent_str, lit.value, @tagName(lit.type) });
            },

            .FloatLiteral => |lit| {
                std.debug.print("{s}FloatLiteral: {d:.6} (type: {s})\n", .{ indent_str, lit.value, @tagName(lit.type) });
            },

            .BoolLiteral => |value| {
                std.debug.print("{s}BoolLiteral: {s}\n", .{ indent_str, if (value) "true" else "false" });
            },

            .StringLiteral => |value| {
                std.debug.print("{s}StringLiteral: \"{s}\"\n", .{ indent_str, value });
            },

            .Identifier => |ident| {
                std.debug.print("{s}Identifier: '{s}' of type {s}\n", .{ indent_str, ident.name, @tagName(ident.symbol.ir_type) });
            },

            .Variable => |var_decl| {
                std.debug.print("{s}Variable: '{s}' (type: {s}, mutable: {}, public: {})\n", .{ indent_str, var_decl.name, @tagName(var_decl.var_type), var_decl.mutable, var_decl.is_public });
                self.printIrNode(var_decl.value, indent + 2);
            },

            .Constant => |const_decl| {
                std.debug.print("{s}Constant: '{s}' (type: {s}, public: {})\n", .{ indent_str, const_decl.name, @tagName(const_decl.var_type), const_decl.is_public });
                self.printIrNode(const_decl.value, indent + 2);
            },

            .BinaryOp => |bin_op| {
                std.debug.print("{s}BinaryOp: {s} (result type: {s})\n", .{ indent_str, @tagName(bin_op.op), @tagName(bin_op.result_type) });
                std.debug.print("{s}  Left operand:\n", .{indent_str});
                self.printIrNode(bin_op.left, indent + 4);
                std.debug.print("{s}  Right operand:\n", .{indent_str});
                self.printIrNode(bin_op.right, indent + 4);
            },

            .UnaryOp => |unary_op| {
                std.debug.print("{s}UnaryOp: {s} (result type: {s})\n", .{ indent_str, @tagName(unary_op.op), @tagName(unary_op.result_type) });
                std.debug.print("{s}  Operand:\n", .{indent_str});
                self.printIrNode(unary_op.operand, indent + 4);
            },

            .Call => |call| {
                std.debug.print("{s}FunctionCall: (result type: {s})\n", .{ indent_str, @tagName(call.result_type) });
                std.debug.print("{s}  Callee:\n", .{indent_str});
                self.printIrNode(call.callee, indent + 4);

                std.debug.print("{s}  Arguments: {d}\n", .{ indent_str, call.arguments.items.len });
                for (call.arguments.items, 0..) |arg, i| {
                    std.debug.print("{s}    Arg {d}:\n", .{ indent_str, i });
                    self.printIrNode(arg, indent + 6);
                }
            },

            .Block => |block| {
                std.debug.print("{s}Block with {d} statements\n", .{ indent_str, block.statements.items.len });
                for (block.statements.items) |stmt| {
                    self.printIrNode(stmt, indent + 2);
                }
            },

            .Function => |func| {
                std.debug.print("{s}Function: '{s}' (public: {}, return type: {s})\n", .{ indent_str, func.name, func.is_public, @tagName(func.return_type) });

                std.debug.print("{s}  Parameters: {d}\n", .{ indent_str, func.parameters.items.len });
                for (func.parameters.items, 0..) |param, i| {
                    std.debug.print("{s}    Param {d}: '{s}' (type: {s})\n", .{ indent_str, i, param.name, @tagName(param.var_type) });
                }

                std.debug.print("{s}  Body:\n", .{indent_str});
                self.printIrNode(func.body, indent + 4);
            },

            .Return => |value| {
                std.debug.print("{s}Return:\n", .{indent_str});
                self.printIrNode(value, indent + 2);
            },

            .Import => |path| {
                std.debug.print("{s}Import: \"{s}\"\n", .{ indent_str, path });
            },

            .Member => |member| {
                std.debug.print("{s}MemberAccess: (property: '{s}', result type: {s})\n", .{ indent_str, member.property, @tagName(member.result_type) });
                std.debug.print("{s}  Object:\n", .{indent_str});
                self.printIrNode(member.object, indent + 4);
            },

            .StructDef => |struct_def| {
                const memory_type_str = switch (struct_def.memory_type) {
                    .Auto => "auto",
                    .GC => "gc",
                    .Scope => "scope",
                    .Stack => "stack",
                };

                std.debug.print("{s}StructDefinition: '{s}' (memory: {s})\n", .{ indent_str, struct_def.name, memory_type_str });

                std.debug.print("{s}  Fields: {d}\n", .{ indent_str, struct_def.fields.items.len });
                for (struct_def.fields.items, 0..) |field, i| {
                    std.debug.print("{s}    Field {d}: '{s}' (type: {s})\n", .{ indent_str, i, field.name, @tagName(field.var_type) });
                }

                std.debug.print("{s}  Methods: {d}\n", .{ indent_str, struct_def.methods.items.len });
                for (struct_def.methods.items, 0..) |method, i| {
                    std.debug.print("{s}    Method {d}: '{s}' (return type: {s})\n", .{ indent_str, i, method.name, @tagName(method.return_type) });
                }
            },

            .TypeCast => |cast| {
                std.debug.print("{s}TypeCast: (target type: {s})\n", .{ indent_str, @tagName(cast.target_type) });
                std.debug.print("{s}  Expression:\n", .{indent_str});
                self.printIrNode(cast.expr, indent + 4);
            },

            .Slice => |slice| {
                std.debug.print("{s}Slice: (element type: {s})\n", .{ indent_str, @tagName(slice.element_type) });
                std.debug.print("{s}  Items:\n", .{indent_str});
                self.printIrNode(slice.items, indent + 4);
            },

            .Assignment => |assign| {
                std.debug.print("{s}Assignment: (type: {s})\n", .{ indent_str, @tagName(assign.type) });
                std.debug.print("{s}  Target:\n", .{indent_str});
                self.printIrNode(assign.target, indent + 4);
                std.debug.print("{s}  Value:\n", .{indent_str});
                self.printIrNode(assign.value, indent + 4);
            },
        }
    }

    /// Main analysis entry point
    pub fn analyze(self: *SemanticAnalyzer) !?*IrNode {
        self.global_scope = try self.parent_allocator.create(SymbolTable);
        self.global_scope.* = SymbolTable.init(self.allocator, null);
        self.current_scope = self.global_scope;
        self.errors = try self.allocator.create(std.ArrayList(SemanticAnalyzerErrorMessage));
        self.errors.* = std.ArrayList(SemanticAnalyzerErrorMessage).init(self.allocator);

        if (self.ast.* != .Program) {
            try self.addError(self.ast, SemanticError.InvalidType, "Expected program node at root of AST");
            return null;
        }

        var program_nodes = std.ArrayList(*IrNode).init(self.allocator);

        // Process each top-level statement
        for (self.ast.Program.items) |statement| {
            if (try self.analyzeNode(statement)) |ir_node| {
                try program_nodes.append(ir_node);
            }
        }

        // Create the program node
        self.ir_root = try self.createIrNode(.Program, program_nodes);

        // If there were errors, return null
        if (self.errors.items.len > 0) {
            // Print all errors for debugging with improved formatting
            for (self.errors.items) |error_info| {
                std.debug.print("Semantic Error: {s}\n", .{
                    error_info.message,
                });
                std.debug.print("  At node type: {s}\n", .{@tagName(error_info.node.*)});

                // Print more specific information based on node type
                switch (error_info.node.*) {
                    .Identifier => |ident| {
                        std.debug.print("  Identifier: '{s}'\n", .{ident});
                    },
                    .BinaryExpr => |expr| {
                        std.debug.print("  Binary operation: '{s}'\n", .{@tagName(expr.op)});
                    },
                    .UnaryExpr => |expr| {
                        std.debug.print("  Unary operation: '{s}'\n", .{@tagName(expr.op)});
                    },
                    .VarDecl => |decl| {
                        std.debug.print("  Variable name: '{s}'\n", .{decl.name});
                    },
                    .ConstDecl => |decl| {
                        std.debug.print("  Constant name: '{s}'\n", .{decl.name});
                    },
                    .LetDecl => |decl| {
                        std.debug.print("  Let variable name: '{s}'\n", .{decl.name});
                    },
                    .StringLiteral => |str| {
                        if (str.len > 20) {
                            std.debug.print("  String: '{s}...'\n", .{str[0..20]});
                        } else {
                            std.debug.print("  String: '{s}'\n", .{str});
                        }
                    },
                    .NumberLiteral => |num| {
                        std.debug.print("  Number: {d}\n", .{num});
                    },
                    .CallExpr => |call| {
                        if (call.callee.* == .Identifier) {
                            std.debug.print("  Function call: '{s}'\n", .{call.callee.Identifier});
                        } else {
                            std.debug.print("  Function call to expression\n", .{});
                        }
                    },
                    else => {}, // Other node types don't need specific info
                }
            }
            return null;
        }

        return self.ir_root;
    }

    /// Add an error message with enhanced information
    fn addError(self: *SemanticAnalyzer, node: *Ast.AstNode, err: SemanticError, msg: []const u8) !void {
        try self.errors.append(.{
            .message = msg,
            .node = node,
            .err = err,
        });
    }

    /// Analyze a node from the AST and produce IR
    fn analyzeNode(self: *SemanticAnalyzer, node: *Ast.AstNode) !?*IrNode {
        switch (node.*) {
            .Program => {
                var program_nodes = std.ArrayList(*IrNode).init(self.allocator);

                for (node.Program.items) |statement| {
                    if (try self.analyzeNode(statement)) |ir_statement| {
                        try program_nodes.append(ir_statement);
                    }
                }

                return try self.createIrNode(.Program, program_nodes);
            },

            .VarDecl => {
                const name = node.VarDecl.name;
                const is_public = node.VarDecl.is_public;

                // Analyze the value
                const value_node = try self.analyzeNode(node.VarDecl.value) orelse
                    return null;

                // Get the type of the value
                const var_type = self.getNodeType(value_node);

                // Create variable symbol
                const symbol = Symbol{
                    .name = name,
                    .symbol_type = .Variable,
                    .ir_type = var_type,
                    .is_public = is_public,
                    .value = value_node,
                    .initialized = true,
                    .mutable = true,
                };

                // Add to symbol table
                self.current_scope.?.define(symbol) catch |err| {
                    if (err == SemanticError.DuplicateSymbol) {
                        try self.addError(node, SemanticError.DuplicateSymbol, try std.fmt.allocPrint(self.allocator, "Variable '{s}' already defined in the current scope", .{name}));
                    } else {
                        try self.addError(node, err, try std.fmt.allocPrint(self.allocator, "Error defining variable '{s}'", .{name}));
                    }
                    return null;
                };

                // Create IR node
                return try self.createIrNode(.Variable, IrVariable{
                    .name = name,
                    .value = value_node,
                    .var_type = var_type,
                    .is_public = is_public,
                    .mutable = true,
                });
            },

            .ConstDecl => {
                const name = node.ConstDecl.name;
                const is_public = node.ConstDecl.is_public;

                // Analyze the value
                const value_node = try self.analyzeNode(node.ConstDecl.value) orelse
                    return null;

                // Get the type of the value
                const var_type = self.getNodeType(value_node);

                // Create constant symbol
                const symbol = Symbol{
                    .name = name,
                    .symbol_type = .Constant,
                    .ir_type = var_type,
                    .is_public = is_public,
                    .value = value_node,
                    .initialized = true,
                    .mutable = false,
                };

                // Add to symbol table
                self.current_scope.?.define(symbol) catch |err| {
                    if (err == SemanticError.DuplicateSymbol) {
                        try self.addError(node, SemanticError.DuplicateSymbol, try std.fmt.allocPrint(self.allocator, "Constant '{s}' already defined in the current scope", .{name}));
                    } else {
                        try self.addError(node, err, try std.fmt.allocPrint(self.allocator, "Error defining constant '{s}'", .{name}));
                    }
                    return null;
                };

                // Create IR node
                return try self.createIrNode(.Constant, IrVariable{
                    .name = name,
                    .value = value_node,
                    .var_type = var_type,
                    .is_public = is_public,
                    .mutable = false,
                });
            },

            .LetDecl => {
                const name = node.LetDecl.name;

                // Analyze the value
                const value_node = try self.analyzeNode(node.LetDecl.value) orelse
                    return null;

                // Get the type of the value
                const var_type = self.getNodeType(value_node);

                // Create variable symbol
                const symbol = Symbol{
                    .name = name,
                    .symbol_type = .Variable,
                    .ir_type = var_type,
                    .is_public = false,
                    .value = value_node,
                    .initialized = true,
                    .mutable = false,
                };

                // Add to symbol table
                self.current_scope.?.define(symbol) catch |err| {
                    if (err == SemanticError.DuplicateSymbol) {
                        try self.addError(node, SemanticError.DuplicateSymbol, try std.fmt.allocPrint(self.allocator, "Let variable '{s}' already defined in the current scope", .{name}));
                    } else {
                        try self.addError(node, err, try std.fmt.allocPrint(self.allocator, "Error defining let variable '{s}'", .{name}));
                    }
                    return null;
                };

                // Create IR node
                return try self.createIrNode(.Variable, IrVariable{
                    .name = name,
                    .value = value_node,
                    .var_type = var_type,
                    .is_public = false,
                    .mutable = false,
                });
            },

            .FunctionDecl => return try self.analyzeFunctionDecl(node),

            .BlockStmt => {
                // Create a new scope for the block
                var block_scope = SymbolTable.init(self.allocator, self.current_scope);
                const old_scope = self.current_scope;
                self.current_scope = &block_scope;
                defer {
                    block_scope.deinit();
                    self.current_scope = old_scope;
                }

                var statements = std.ArrayList(*IrNode).init(self.allocator);

                for (node.BlockStmt.items) |statement| {
                    if (try self.analyzeNode(statement)) |ir_statement| {
                        try statements.append(ir_statement);
                    }
                }

                return try self.createIrNode(.Block, IrBlock{
                    .statements = statements,
                    .scope = &block_scope,
                });
            },

            .BinaryExpr => {
                const left = try self.analyzeNode(node.BinaryExpr.left) orelse
                    return null;
                const right = try self.analyzeNode(node.BinaryExpr.right) orelse
                    return null;

                const left_type = self.getNodeType(left);
                const right_type = self.getNodeType(right);

                // Validate operator and determine result type
                const result_type = self.validateBinaryOp(node, node.BinaryExpr.op, left_type, right_type) catch {
                    return null; // Error already added by validateBinaryOp
                };

                return try self.createIrNode(.BinaryOp, IrBinaryOp{
                    .op = node.BinaryExpr.op,
                    .left = left,
                    .right = right,
                    .result_type = result_type,
                });
            },

            .UnaryExpr => {
                const operand = try self.analyzeNode(node.UnaryExpr.operand) orelse
                    return null;

                const operand_type = self.getNodeType(operand);

                // Validate operator and determine result type
                const result_type = self.validateUnaryOp(node, node.UnaryExpr.op, operand_type) catch {
                    return null; // Error already added by validateUnaryOp
                };

                return try self.createIrNode(.UnaryOp, IrUnaryOp{
                    .op = node.UnaryExpr.op,
                    .operand = operand,
                    .result_type = result_type,
                });
            },

            .NumberLiteral => {
                // All number literals are I64 in our simplified type system
                return try self.createIrNode(.IntLiteral, IrIntLiteral{
                    .value = node.NumberLiteral,
                    .type = .I64,
                });
            },

            .FloatLiteral => {
                std.debug.panic("Not implemented: Float literals", .{});
                return null;
            },

            .BoolLiteral => {
                return try self.createIrNode(.BoolLiteral, node.BoolLiteral);
            },

            .StringLiteral => {
                return try self.createIrNode(.StringLiteral, node.StringLiteral);
            },

            .Identifier => {
                const name = node.Identifier;

                // Look up symbol in scope
                if (self.current_scope.?.resolve(name)) |symbol| {
                    return try self.createIrNode(.Identifier, IrIdentifier{
                        .name = name,
                        .symbol = symbol,
                    });
                } else {
                    try self.addError(node, SemanticError.UndefinedVariable, try std.fmt.allocPrint(self.allocator, "Undefined identifier: '{s}'", .{name}));
                    return null;
                }
            },

            .CallExpr => {
                // Analyze the callee
                const callee_node = try self.analyzeNode(node.CallExpr.callee) orelse
                    return null;

                // Process arguments
                var arguments = std.ArrayList(*IrNode).init(self.allocator);
                for (node.CallExpr.arguments.items) |arg_expr| {
                    const arg_node = try self.analyzeNode(arg_expr) orelse
                        return null;
                    try arguments.append(arg_node);
                }

                // Validate function call and determine return type
                const result_type = try self.validateFunctionCall(node, callee_node, arguments);

                return try self.createIrNode(.Call, IrCall{
                    .callee = callee_node,
                    .arguments = arguments,
                    .result_type = result_type,
                });
            },

            .ReturnStmt => {
                // Find the enclosing function context
                const function_context = self.findEnclosingFunctionContext();
                if (function_context == null) {
                    try self.addError(node, SemanticError.InvalidOperator, "Return statement outside of function");
                    return null;
                }

                // Get the expected return type from the function context
                const expected_return_type = function_context.?.return_type;

                // Process return value if present
                if (node.ReturnStmt.value) |value_expr| {
                    // Analyze the return value expression
                    const return_value = try self.analyzeNode(value_expr) orelse return null;
                    const actual_return_type = self.getNodeType(return_value);

                    // Check if the return type matches the function's declared return type
                    if (!self.areTypesCompatible(expected_return_type, actual_return_type)) {
                        try self.addError(node, SemanticError.TypeMismatch, try std.fmt.allocPrint(self.allocator, "Return value type {s} doesn't match function's return type {s} in function '{s}'", .{ @tagName(actual_return_type), @tagName(expected_return_type), function_context.?.name }));
                        return null;
                    }

                    // Create return IR node with the value
                    return try self.createIrNode(.Return, return_value);
                } else {
                    // No return value provided - check if function should return void
                    if (expected_return_type != .Void) {
                        try self.addError(node, SemanticError.TypeMismatch, try std.fmt.allocPrint(self.allocator, "Function '{s}' requires return value of type {s}, but return statement has no value", .{ function_context.?.name, @tagName(expected_return_type) }));
                        return null;
                    }

                    // For void returns, create a void return node
                    const void_value = try self.createIrNode(.IntLiteral, IrIntLiteral{
                        .value = 0,
                        .type = .Void,
                    });
                    return try self.createIrNode(.Return, void_value);
                }
            },

            .MemberExpr => {
                std.debug.panic("Not implemented: Member expressions", .{});
                return null;
            },

            .Accessor => {
                std.debug.panic("Not implemented: Accessor chains", .{});
                return null;
            },

            .ImportExpr => {
                std.debug.panic("Not implemented: Import expressions", .{});
                return null;
            },

            .StructInit => {
                std.debug.panic("Not implemented: Struct initialization", .{});
                return null;
            },

            .TypeNode => {
                // TypeNodes are not standalone expressions; they appear as part of other nodes
                // like function return types and parameter types.
                const ir_type = try self.resolveTypeNode(node);
                try self.addError(node, SemanticError.InvalidType, "Type expressions cannot be used as standalone values");

                // Return a placeholder node to avoid further errors
                return try self.createIrNode(.IntLiteral, IrIntLiteral{
                    .value = 0,
                    .type = ir_type,
                });
            },

            .AssignStmt => {
                // Analyze the target (left side of the assignment)
                const target = try self.analyzeNode(node.AssignStmt.target);
                if (target == null) {
                    try self.addError(node, SemanticError.InvalidAssignment, "Invalid assignment target");
                    return null;
                }
                // Make sure target is assignable (typically an identifier or member access)
                switch (target.?.*) {
                    .Identifier, .Member => {}, // These are valid assignment targets
                    else => {
                        try self.addError(node, SemanticError.InvalidAssignment, "Invalid assignment target");
                        return null;
                    },
                }

                // Analyze the value (right side of the assignment)
                const value = try self.analyzeNode(node.AssignStmt.value);

                // Check type compatibility between target and value
                if (!self.areTypesCompatible(self.getNodeType(target.?), self.getNodeType(value.?))) {
                    try self.addError(node, SemanticError.TypeMismatch, "Type mismatch in assignment");
                    return null;
                }

                // Create and return the IR assignment node
                return try self.createIrNode(.Assignment, IrAssignment{
                    .target = target.?,
                    .value = value.?,
                    .type = self.getNodeType(target.?),
                });
            },
        }
    }

    /// Find the enclosing function context by traversing the scope chain
    fn findEnclosingFunctionContext(self: *SemanticAnalyzer) ?FunctionContext {
        var current_scope = self.current_scope;

        // Walk up the scope chain looking for a function context
        while (current_scope != null) {
            if (current_scope.?.function_context != null) {
                return current_scope.?.function_context;
            }

            current_scope = current_scope.?.parent;
        }

        return null;
    }

    /// Get the type of an IR node
    fn getNodeType(self: *SemanticAnalyzer, node: *IrNode) IrType {
        _ = self;
        return switch (node.*) {
            IrNodeType.IntLiteral => |lit| lit.type,
            IrNodeType.FloatLiteral => |lit| lit.type,
            IrNodeType.BoolLiteral => .Bool,
            IrNodeType.StringLiteral => .String,
            IrNodeType.Identifier => |ident| ident.symbol.ir_type,
            IrNodeType.Variable => |var_decl| var_decl.var_type,
            IrNodeType.Constant => |const_decl| const_decl.var_type,
            IrNodeType.BinaryOp => |bin_op| bin_op.result_type,
            IrNodeType.UnaryOp => |unary_op| unary_op.result_type,
            IrNodeType.Call => |call| call.result_type,
            IrNodeType.Member => |member| member.result_type,
            IrNodeType.TypeCast => |cast| cast.target_type,
            IrNodeType.Slice => .Slice,
            IrNodeType.Block => .Void, // Blocks default to void
            IrNodeType.Function => .Function,
            IrNodeType.Assignment => |assign| assign.type,
            else => .Unknown,
        };
    }

    /// Resolve a type node to an IR type
    fn resolveTypeNode(self: *SemanticAnalyzer, type_node: *Ast.AstNode) !IrType {
        switch (type_node.*) {
            .TypeNode => |type_info| {
                switch (type_info) {
                    .Builtin => |builtin| {
                        // Direct mapping from BuiltinType to IrType
                        return switch (builtin) {
                            .Void => .Void,
                            .Bool => .Bool,
                            .I32 => .I32,
                            .I64 => .I64,
                            .U8 => .U8,
                            .U32 => .U32,
                            .U64 => .U64,
                            .String => .String,
                            // Map other built-in types as needed
                            .I8, .I16, .F32, .F64, .F80, .U16, .StringBuilder, .ISize, .USize => {
                                try self.addError(type_node, SemanticError.InvalidType, try std.fmt.allocPrint(self.allocator, "Built-in type {} not yet supported in IR", .{builtin}));
                                return .Unknown;
                            },
                        };
                    },
                    .Reference => |reference| {
                        // For user-defined types, we need to look up the definition in the symbol table
                        if (reference.definition.* == .Identifier) {
                            const type_name = reference.definition.Identifier;
                            if (self.current_scope.?.resolve(type_name)) |symbol| {
                                if (symbol.symbol_type == .Type) {
                                    return symbol.ir_type;
                                }
                            }

                            // Not found in symbol table as a type
                            try self.addError(type_node, SemanticError.InvalidType, try std.fmt.allocPrint(self.allocator, "Unknown user-defined type: '{s}'", .{type_name}));
                        } else {
                            // Reference points to something other than an identifier
                            try self.addError(type_node, SemanticError.InvalidType, "Invalid type reference");
                        }
                        return .Unknown;
                    },
                    .Array => |array_info| {
                        // For now, we'll handle arrays as slices in the IR
                        // In a more complete implementation, we'd handle fixed-size arrays differently
                        _ = try self.resolveTypeNode(array_info.element_type); // Validate element type
                        return .Slice;
                    },
                    .Slice => |slice_info| {
                        _ = try self.resolveTypeNode(slice_info.element_type); // Validate element type
                        return .Slice;
                    },
                    .Optional => |opt_info| {
                        // For now, we don't have dedicated optional types in IR
                        // In a real implementation, we'd handle optionals properly
                        try self.addError(type_node, SemanticError.InvalidType, "Optional types not yet supported");
                        _ = try self.resolveTypeNode(opt_info.base_type); // Validate base type anyway
                        return .Unknown;
                    },
                }
            },
            .Identifier => |ident| {
                // Legacy handling for plain identifier types
                std.debug.print("Warning: Using legacy identifier-based type: {s}\n", .{ident});

                // For identifiers, we only look up user-defined types in the symbol table
                if (self.current_scope.?.resolve(ident)) |symbol| {
                    if (symbol.symbol_type == .Type) {
                        return symbol.ir_type;
                    }
                }

                try self.addError(type_node, SemanticError.InvalidType, try std.fmt.allocPrint(self.allocator, "Unknown user-defined type: '{s}'", .{ident}));
                return .Unknown;
            },
            else => {
                try self.addError(type_node, SemanticError.InvalidType, try std.fmt.allocPrint(self.allocator, "Expected type node but got: {s}", .{@tagName(type_node.*)}));
                return .Unknown;
            },
        }
    }

    /// Validate binary operation and determine result type
    fn validateBinaryOp(self: *SemanticAnalyzer, node: *Ast.AstNode, op: Ast.BinaryOp, left_type: IrType, right_type: IrType) !IrType {
        // Simple implementation for our reduced type set
        switch (op) {
            // Arithmetic operations
            .Add, .Subtract, .Multiply, .Divide, .Modulo => {
                if (!self.isNumericType(left_type) or !self.isNumericType(right_type)) {
                    try self.addError(node, SemanticError.InvalidOperand, try std.fmt.allocPrint(self.allocator, "Arithmetic operation '{s}' requires numeric operands, got {s} and {s}", .{ @tagName(op), @tagName(left_type), @tagName(right_type) }));
                    return SemanticError.InvalidOperand;
                }
                return self.widenNumericTypes(left_type, right_type);
            },

            // Comparison operations
            .Equals, .NotEquals, .LessThan, .GreaterThan, .LessThanEquals, .GreaterThanEquals => {
                if (!self.areTypesCompatible(left_type, right_type)) {
                    try self.addError(node, SemanticError.IncompatibleTypes, try std.fmt.allocPrint(self.allocator, "Cannot compare incompatible types: {s} and {s} with operator '{s}'", .{ @tagName(left_type), @tagName(right_type), @tagName(op) }));
                    return SemanticError.IncompatibleTypes;
                }
                return .Bool;
            },

            // Logical operations
            .LogicalAnd, .LogicalOr => {
                if (left_type != .Bool or right_type != .Bool) {
                    try self.addError(node, SemanticError.InvalidOperand, try std.fmt.allocPrint(self.allocator, "Logical operation '{s}' requires boolean operands, got {s} and {s}", .{ @tagName(op), @tagName(left_type), @tagName(right_type) }));
                    return SemanticError.InvalidOperand;
                }
                return .Bool;
            },

            // Bitwise operations - not fully implemented
            .BitwiseAnd, .BitwiseOr, .BitwiseXor, .LeftShift, .RightShift => {
                std.debug.panic("Not implemented: Bitwise operations", .{});
                return .Unknown;
            },
        }

        try self.addError(
            node,
            SemanticError.InvalidOperator,
            try std.fmt.allocPrint(
                self.allocator,
                "Invalid binary operation: '{s}' applied to {s} and {s}",
                .{ @tagName(op), @tagName(left_type), @tagName(right_type) },
            ),
        );
        return SemanticError.InvalidOperator;
    }

    /// Validate unary operation and determine result type
    fn validateUnaryOp(self: *SemanticAnalyzer, node: *Ast.AstNode, op: Ast.UnaryOp, operand_type: IrType) !IrType {
        switch (op) {
            .Negate => {
                if (!self.isNumericType(operand_type)) {
                    try self.addError(node, SemanticError.InvalidOperand, try std.fmt.allocPrint(self.allocator, "Negation operation requires numeric operand, got {s}", .{@tagName(operand_type)}));
                    return SemanticError.InvalidOperand;
                }
                return operand_type;
            },
            .BitwiseNot => {
                std.debug.panic("Not implemented: Bitwise not operator", .{});
                return .Unknown;
            },
            .LogicalNot => {
                if (operand_type != .Bool) {
                    try self.addError(node, SemanticError.InvalidOperand, try std.fmt.allocPrint(self.allocator, "Logical not operation requires boolean operand, got {s}", .{@tagName(operand_type)}));
                    return SemanticError.InvalidOperand;
                }
                return .Bool;
            },
        }
    }

    /// Determine if a type is numeric (integer only in our simplified system)
    fn isNumericType(self: *SemanticAnalyzer, ir_type: IrType) bool {
        return self.isIntegerType(ir_type);
    }

    /// Determine if a type is an integer
    fn isIntegerType(self: *SemanticAnalyzer, ir_type: IrType) bool {
        _ = self;
        return switch (ir_type) {
            .I32, .I64, .U8, .U32, .U64 => true,
            else => false,
        };
    }

    /// Check if two types are compatible for operations
    fn areTypesCompatible(self: *SemanticAnalyzer, a: IrType, b: IrType) bool {
        // Same type is always compatible
        if (a == b) return true;

        // Numeric types are compatible with each other
        if (self.isNumericType(a) and self.isNumericType(b)) return true;

        // Special case: any non-void type can be returned from a Void function
        if (a == .Void) return true;

        return false;
    }

    /// Get the wider of two numeric types (simplified for our subset)
    fn widenNumericTypes(self: *SemanticAnalyzer, a: IrType, b: IrType) IrType {
        _ = self;

        // For integers, choose the widest or signed over unsigned
        if (a == .I64 or b == .I64) return .I64;
        if (a == .U64 or b == .U64) return .U64;
        if (a == .I32 or b == .I32) return .I32;
        if (a == .U32 or b == .U32) return .U32;
        if (a == .U8 or b == .U8) return .U8;

        // Default
        return a;
    }

    /// Determine the result type of a function call
    fn getCallResultType(self: *SemanticAnalyzer, callee: *IrNode, args: std.ArrayList(*IrNode)) !IrType {
        _ = self;
        switch (callee.*) {
            .Identifier => |ident| {
                const symbol = ident.symbol;
                if (symbol.symbol_type != .Function) {
                    return SemanticError.InvalidFunctionCall;
                }

                // Check if function node is available
                if (symbol.value) |func_node| {
                    if (func_node.* == .Function) {
                        return func_node.Function.return_type;
                    }
                }

                return symbol.ir_type;
            },
            .Member => |member| {
                // For method calls, we'd need to look up the method in the struct type
                // For now, return a simplified implementation
                _ = args;
                _ = member;
                return .Unknown;
            },
            else => return SemanticError.InvalidFunctionCall,
        }
    }

    /// Validate a function call and determine its return type
    fn validateFunctionCall(self: *SemanticAnalyzer, node: *Ast.AstNode, callee: *IrNode, args: std.ArrayList(*IrNode)) !IrType {
        // Get function symbol from callee
        var function_symbol: ?Symbol = null;

        switch (callee.*) {
            .Identifier => {
                // Direct function call
                function_symbol = callee.Identifier.symbol;
                if (function_symbol.?.symbol_type != .Function) {
                    try self.addError(node, SemanticError.InvalidFunctionCall, try std.fmt.allocPrint(self.allocator, "Called value is not a function: '{s}'", .{callee.Identifier.name}));
                    return SemanticError.InvalidFunctionCall;
                }
            },
            .Member => {
                // Method call
                const object_type = self.getNodeType(callee.Member.object);
                if (object_type != .Struct) {
                    try self.addError(node, SemanticError.InvalidMemberAccess, try std.fmt.allocPrint(self.allocator, "Cannot call method on non-struct type: {s}", .{@tagName(object_type)}));
                    return SemanticError.InvalidMemberAccess;
                }
                // Note: Full method call validation would require struct type info
                return .Unknown; // Simplified for now
            },
            else => {
                try self.addError(node, SemanticError.InvalidFunctionCall, "Called expression is not a function");
                return SemanticError.InvalidFunctionCall;
            },
        }

        // For direct function calls, validate parameters
        if (function_symbol) |func| {
            if (func.parameters) |params| {
                // Check parameter count
                if (args.items.len != params.items.len) {
                    try self.addError(node, SemanticError.InvalidFunctionCall, try std.fmt.allocPrint(self.allocator, "Function '{s}' expects {d} arguments but got {d}", .{ func.name, params.items.len, args.items.len }));
                    return SemanticError.InvalidFunctionCall;
                }

                // Check parameter types
                for (params.items, args.items, 0..) |param, arg, i| {
                    const arg_type = self.getNodeType(arg);
                    if (!self.areTypesCompatible(param.var_type, arg_type)) {
                        try self.addError(node, SemanticError.TypeMismatch, try std.fmt.allocPrint(self.allocator, "Type mismatch for argument {d}: expected {s}, got {s}", .{ i + 1, @tagName(param.var_type), @tagName(arg_type) }));
                        return SemanticError.TypeMismatch;
                    }
                }
            }

            // Return the function's return type
            return func.ir_type;
        }

        return .Unknown;
    }

    /// Determine the type of a member access
    fn getMemberType(self: *SemanticAnalyzer, object: *IrNode, property: []const u8) !IrType {
        _ = self;
        _ = object;
        _ = property;
        std.debug.panic("Not implemented: Member access type resolution", .{});
        return .Unknown;
    }

    /// Analyze a function declaration
    fn analyzeFunctionDecl(self: *SemanticAnalyzer, node: *Ast.AstNode) SemanticError!?*IrNode {
        const name = node.FunctionDecl.name;
        const is_public = node.FunctionDecl.is_public;

        // Create a new scope for the function body
        const function_scope = try self.allocator.create(SymbolTable);
        function_scope.* = SymbolTable.init(self.allocator, self.current_scope);
        const old_scope = self.current_scope;
        self.current_scope = function_scope;

        // Process parameters
        var parameters = std.ArrayList(IrVariable).init(self.allocator);
        for (node.FunctionDecl.parameters.items) |param| {
            // Get parameter type (default to I64 if not specified)
            var param_type: IrType = .I64;
            if (param.type_expr) |type_expr| {
                param_type = try self.resolveTypeNode(type_expr);
            }

            // Create parameter variable
            const param_var = IrVariable{
                .name = param.name,
                .value = undefined, // Will be set during function call
                .var_type = param_type,
                .is_public = false,
                .mutable = false, // Parameters are immutable by default
            };

            // Add parameter to list
            try parameters.append(param_var);

            // Add parameter to function scope
            try self.current_scope.?.define(Symbol{
                .name = param.name,
                .symbol_type = .Parameter,
                .ir_type = param_type,
                .is_public = false,
                .initialized = true,
                .mutable = false,
            });
        }

        // Determine return type
        var return_type: IrType = .Void;
        if (node.FunctionDecl.return_type) |type_node| {
            return_type = try self.resolveTypeNode(type_node);
        }

        // Set function context in the current scope for return statement validation
        self.current_scope.?.function_context = .{
            .name = name,
            .return_type = return_type,
        };

        // Process function body
        const body_node = try self.analyzeNode(node.FunctionDecl.body) orelse {
            self.current_scope = old_scope;
            return null;
        };

        // Return to previous scope
        self.current_scope = old_scope;

        // Create the function IR node
        const function_node = try self.createIrNode(.Function, IrFunction{
            .name = name,
            .parameters = parameters,
            .return_type = return_type,
            .body = body_node,
            .is_public = is_public,
        });

        // Add function to symbol table in the parent scope
        try self.current_scope.?.define(Symbol{
            .name = name,
            .symbol_type = .Function,
            .ir_type = return_type, // Store the actual return type in ir_type
            .is_public = is_public,
            .value = function_node,
            .initialized = true,
            .mutable = false,
            .parameters = parameters,
        });

        return function_node;
    }
};
