const std = @import("std");
const LexToken = @import("lextoken.zig").LexerToken;
const TokenError = @import("lextoken.zig").TokenError;
const TokenKinds = @import("tokenkinds.zig").TokenKinds;

/// Error types for AST parsing
pub const AstError = error{
    UnexpectedToken,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedEquals,
    ExpectedNumber,
    InvalidToken,
    OutOfMemory,
    ExpectedClosingParen,
    ExpectedClosingBrace,
    ExpectedClosingBracket,
    UnexpectedEof,
    Located,
    ExpectedString,
    ExpectedParamName,
    ExpectedFunctionBody,
    ExpectedArrowOrOpenBrace,
    ExpectedStatement,
    ExpectedLetIdentifier,
    ExpectedRightBrace,
    ExpectedInitializer,
    ExpectedReturnExpression,
};

/// AST node types
pub const NodeType = enum {
    Program,
    VarDecl,
    ConstDecl,
    LetDecl,
    FunctionDecl,
    BinaryExpr,
    UnaryExpr,
    NumberLiteral,
    FloatLiteral,
    BoolLiteral,
    StringLiteral,
    Identifier,
    ImportExpr,
    CallExpr,
    BlockStmt,
    MemberExpr,
    StructInit,
    Accessor, // New node type for multi-level accessors (e.g., std.debug.print)
    ReturnStmt, // New node type for return statements
    TypeNode, // New node type for representing types
    AssignStmt, // New node type for assignment statements
};

/// Binary operation types
pub const BinaryOp = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    LogicalAnd,
    LogicalOr,
};

/// Unary operation types
pub const UnaryOp = enum {
    Negate,
    BitwiseNot,
    LogicalNot,
};

/// Get operator precedence (higher means tighter binding)
fn getOperatorPrecedence(op: BinaryOp) u8 {
    return switch (op) {
        .LogicalOr => 1,
        .LogicalAnd => 2,
        .Equals, .NotEquals, .LessThan, .GreaterThan, .LessThanEquals, .GreaterThanEquals => 3,
        .BitwiseOr => 4,
        .BitwiseXor => 5,
        .BitwiseAnd => 6,
        .LeftShift, .RightShift => 7,
        .Add, .Subtract => 8,
        .Multiply, .Divide, .Modulo => 9,
    };
}

/// Maps token kinds to binary operators
fn tokenToBinaryOp(kind: TokenKinds) ?BinaryOp {
    return switch (kind) {
        .Plus => .Add,
        .Minus => .Subtract,
        .Asterisk => .Multiply,
        .Slash => .Divide,
        .Percent => .Modulo,
        .Ampersand => .BitwiseAnd,
        .Pipe => .BitwiseOr,
        .Caret => .BitwiseXor,
        .DoubleLessThan => .LeftShift,
        .DoubleGreaterThan => .RightShift,
        .DoubleEquals => .Equals,
        .ExclamationEquals => .NotEquals,
        .LessThan => .LessThan,
        .GreaterThan => .GreaterThan,
        .LessThanEquals => .LessThanEquals,
        .GreaterThanEquals => .GreaterThanEquals,
        .And => .LogicalAnd,
        .Or => .LogicalOr,
        .bXor => .BitwiseXor,
        .bAnd => .BitwiseAnd,
        .bOr => .BitwiseOr,
        // .AddSec // TODO create Sec operators
        // these need to create errors with over/underflow or divide by zero
        // .SubSec
        // .MulSec
        // .DivSec
        // .ModSec
        // .RemSec
        else => null,
    };
}

/// AST node structures
pub const VarDecl = struct {
    is_public: bool,
    name: []const u8,
    value: *AstNode,
};

pub const ConstDecl = struct {
    is_public: bool,
    name: []const u8,
    value: *AstNode,
};

pub const Parameter = struct {
    name: []const u8,
    type_expr: ?*AstNode,
};

pub const FunctionDecl = struct {
    is_public: bool,
    name: []const u8,
    parameters: std.ArrayList(Parameter),
    return_type: ?*AstNode,
    body: *AstNode,
};

pub const BinaryExpr = struct {
    op: BinaryOp,
    left: *AstNode,
    right: *AstNode,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    operand: *AstNode,
};

pub const CallExpr = struct {
    callee: *AstNode,
    arguments: std.ArrayList(*AstNode),
};

pub const LetDecl = struct {
    name: []const u8,
    value: *AstNode,
};

pub const MemberExpr = struct {
    object: *AstNode,
    property: *AstNode,
};

/// Memory management type for structs
pub const MemoryManagementType = enum {
    Auto, // .{ } - automatically managed
    GC, // ${ } - garbage collected
    Scope, // #{ } - scope managed
    Stack, // @{ } - stack allocated
};

/// Create an Accessor struct for handling chains of member access like std.debug.print
pub const Accessor = struct {
    base: *AstNode, // Base identifier (e.g., "std")
    parts: std.ArrayList(*AstNode), // Array of member names (e.g., ["debug", "print"])
};

pub const StructInit = struct {
    fields: std.ArrayList(*AstNode),
    funcs: std.ArrayList(*AstNode),
    memory_type: MemoryManagementType,
};

/// Create a Return Statement structure
pub const ReturnStmt = struct {
    value: ?*AstNode, // Optional because return; with no value is valid in some languages
};

/// Create an assignment statement structure
pub const AssignStmt = struct {
    target: *AstNode, // Left side of assignment (identifier or access)
    value: *AstNode, // Right side (the expression being assigned)
};

/// Built-in type enum for type safety
pub const BuiltinType = enum {
    Void,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    F80,
    String,
    StringBuilder,
    ISize,
    USize,

    // Helper to convert from TokenKind to BuiltinType
    pub fn fromTokenKind(kind: TokenKinds) ?BuiltinType {
        return switch (kind) {
            .Void => .Void,
            .Bool => .Bool,
            .I8 => .I8,
            .I16 => .I16,
            .I32 => .I32,
            .I64 => .I64,
            .U8 => .U8,
            .U16 => .U16,
            .U32 => .U32,
            .U64 => .U64,
            .F32 => .F32,
            .F64 => .F64,
            .F80 => .F80,
            .String => .String,
            .StringBuilder => .StringBuilder,
            .ISize => .ISize,
            .USize => .USize,
            else => null,
        };
    }
};

/// Type representation with better structure
pub const TypeKind = enum {
    Builtin,
    Reference, // Reference to a type definition node
    Array,
    Slice,
    Optional,
    // Add more as needed (Pointer, Function, etc.)
};

pub const TypeInfo = union(TypeKind) {
    Builtin: BuiltinType,
    Reference: struct {
        definition: *AstNode, // Direct reference to the type definition node
    },
    Array: struct {
        element_type: *AstNode,
        size: ?i64, // null means dynamic size
    },
    Slice: struct {
        element_type: *AstNode,
    },
    Optional: struct {
        base_type: *AstNode,
    },
    // Add more variants as needed
};

/// AST node structure
pub const AstNode = union(NodeType) {
    Program: std.ArrayList(*AstNode),
    VarDecl: VarDecl,
    ConstDecl: ConstDecl,
    LetDecl: LetDecl,
    FunctionDecl: FunctionDecl,
    BinaryExpr: BinaryExpr,
    UnaryExpr: UnaryExpr,
    NumberLiteral: i128,
    FloatLiteral: f128,
    BoolLiteral: bool,
    StringLiteral: []const u8,
    Identifier: []const u8,
    ImportExpr: []const u8,
    CallExpr: CallExpr,
    BlockStmt: std.ArrayList(*AstNode),
    MemberExpr: MemberExpr,
    StructInit: StructInit,
    Accessor: Accessor, // Add the Accessor node type to the union
    ReturnStmt: ReturnStmt, // Add the ReturnStmt node type to the union
    TypeNode: TypeInfo, // Add the TypeNode variant
    AssignStmt: AssignStmt, // Add the AssignStmt variant
};

/// Token iterator for efficient parsing
const TokenIterator = struct {
    tokens: []const LexToken,
    position: *usize,
    parser: *Parser,
    current: ?LexToken = null,

    fn init(tokens: []const LexToken, position: *usize, parser: *Parser) TokenIterator {
        var iter = TokenIterator{
            .tokens = tokens,
            .position = position,
            .parser = parser,
        };
        iter.advance(); // Initialize current token
        return iter;
    }

    fn peek(self: *const TokenIterator) ?LexToken {
        return self.current;
    }

    /// Peek at the next token, skipping only whitespace
    fn peekNextNonWhiteSpace(self: *const TokenIterator) ?LexToken {
        var pos = self.position.*;

        while (pos < self.tokens.len) {
            const token = self.tokens[pos];

            // Skip only whitespace
            if (token.kind != .Whitespace) {
                return token;
            }

            pos += 1;
        }

        return null;
    }

    /// Peek at the next token, skipping whitespace, newlines, and comments
    fn peekNextNonWhiteOrNewLine(self: *const TokenIterator) ?LexToken {
        var pos = self.position.*;

        while (pos < self.tokens.len) {
            const token = self.tokens[pos];

            // Skip whitespace, newlines, and comments
            if (token.kind != .Whitespace and
                token.kind != .Newline and
                token.kind != .LineComment and
                token.kind != .BlockComment)
            {
                return token;
            }

            pos += 1;
        }

        return null;
    }

    fn advance(self: *TokenIterator) void {
        var pos = self.position.*;

        // Skip all whitespace tokens until we find a substantive token
        while (pos < self.tokens.len) {
            const token = self.tokens[pos];
            pos += 1;

            // Skip over non-syntactical tokens
            if (token.kind != .Whitespace and
                token.kind != .Newline and
                token.kind != .LineComment and
                token.kind != .BlockComment)
            {
                self.position.* = pos;
                self.current = token;
                return;
            }
        }

        // If we reach here, we've hit the end of tokens
        self.position.* = pos;
        self.current = null;
    }

    fn expectToken(self: *TokenIterator, kind: TokenKinds) !void {
        // First skip any whitespace
        self.advanceToSubstantiveToken();

        const token = self.peek() orelse return AstError.UnexpectedEof;

        if (token.kind != kind) {
            std.debug.print("Expected token {} but found {}\n", .{ kind, token.kind });
            return AstError.UnexpectedToken;
        }

        self.advance();
    }

    fn consume(self: *TokenIterator, kind: TokenKinds) bool {
        if (self.peek()) |token| {
            if (token.kind == kind) {
                self.advance();
                return true;
            }

            return false;
        }
        return false;
    }

    /// Helper method to advance through whitespace and newlines to the next substantive token
    fn advanceToSubstantiveToken(self: *TokenIterator) void {
        // If current token is already substantive, return
        if (self.current != null and
            self.current.?.kind != .Whitespace and
            self.current.?.kind != .Newline and
            self.current.?.kind != .LineComment and
            self.current.?.kind != .BlockComment)
        {
            return; // Current token is already substantive
        }

        // Otherwise repeatedly call advance until we reach a substantive token
        while (self.current != null) {
            if (self.current.?.kind != .Whitespace and
                self.current.?.kind != .Newline and
                self.current.?.kind != .LineComment and
                self.current.?.kind != .BlockComment)
            {
                return; // Found a substantive token
            }
            self.advance();
        }
    }

    /// Expect a token, but first skip any whitespace or newlines
    fn expectTokenSkipWhitespace(self: *TokenIterator, kind: TokenKinds) !void {
        self.advanceToSubstantiveToken();
        const token = self.peek() orelse return AstError.UnexpectedEof;

        if (token.kind != kind) {
            std.debug.print("Expected token {} but found {}\n", .{ kind, token.kind });
            return AstError.UnexpectedToken;
        }

        self.advance();
    }

    /// Consume a token if it matches, but first skip any whitespace or newlines
    fn consumeSkipWhitespace(self: *TokenIterator, kind: TokenKinds) bool {
        self.advanceToSubstantiveToken();
        return self.consume(kind);
    }
};

/// Parser for building AST from tokens
pub const Parser = struct {
    tokens: []const LexToken,
    position: usize,
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    parent_allocator: std.mem.Allocator,
    error_location: usize,
    has_errors: bool,
    root: ?*AstNode,

    /// Initialize a new parser
    pub fn init(tokens: []const LexToken, parent_allocator: std.mem.Allocator) Parser {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        const allocator = arena.allocator();

        return Parser{
            .tokens = tokens,
            .position = 0,
            .arena = arena,
            .allocator = allocator,
            .error_location = 0,
            .has_errors = false,
            .parent_allocator = parent_allocator,
            .root = null,
        };
    }

    /// Deallocate all parser resources
    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    /// Parse the tokens into an AST
    pub fn parse(self: *Parser) !*AstNode {
        self.root = try self.parseProgram();
        return self.root.?;
    }

    /// Create a new AST node with a given type and value
    fn createNode(self: *Parser, comptime node_type: NodeType, init_value: anytype) !*AstNode {
        const node = try self.allocator.create(AstNode);
        node.* = switch (node_type) {
            .StringLiteral => AstNode{ .StringLiteral = init_value },
            .NumberLiteral => AstNode{ .NumberLiteral = init_value },
            .FloatLiteral => AstNode{ .FloatLiteral = init_value },
            .BoolLiteral => AstNode{ .BoolLiteral = init_value },
            .Identifier => AstNode{ .Identifier = init_value },
            .BinaryExpr => AstNode{ .BinaryExpr = init_value },
            .UnaryExpr => AstNode{ .UnaryExpr = init_value },
            .ImportExpr => AstNode{ .ImportExpr = init_value },
            .CallExpr => AstNode{ .CallExpr = init_value },
            .BlockStmt => AstNode{ .BlockStmt = init_value },
            .MemberExpr => AstNode{ .MemberExpr = init_value },
            .StructInit => AstNode{ .StructInit = init_value },
            .Program => AstNode{ .Program = init_value },
            .VarDecl => AstNode{ .VarDecl = init_value },
            .ConstDecl => AstNode{ .ConstDecl = init_value },
            .LetDecl => AstNode{ .LetDecl = init_value },
            .FunctionDecl => AstNode{ .FunctionDecl = init_value },
            .Accessor => AstNode{ .Accessor = init_value },
            .ReturnStmt => AstNode{ .ReturnStmt = init_value }, // Add ReturnStmt case
            .TypeNode => AstNode{ .TypeNode = init_value }, // Add TypeNode case
            .AssignStmt => AstNode{ .AssignStmt = init_value }, // Add AssignStmt case
        };
        return node;
    }

    fn printToken(lexToken: LexToken, current: bool) void {
        if (current) {
            std.debug.print("Current Token: ", .{});
        } else {
            std.debug.print("", .{});
        }
        std.debug.print(" {s}\t\t", .{@tagName(lexToken.kind)});
        if (lexToken.value) |val| {
            std.debug.print("{s}", .{val});
        }
        std.debug.print("\n", .{});
    }

    /// Parse a complete program
    fn parseProgram(self: *Parser) AstError!*AstNode {
        var statements = std.ArrayList(*AstNode).init(self.allocator);
        var iter = TokenIterator.init(self.tokens, &self.position, self);

        // Parse statements until EOF
        while (iter.peek()) |token| {
            if (token.kind == .EOF) break;

            const statement = self.parseStatement(&iter) catch |err| {
                // Skip to next statement on error
                self.has_errors = true;

                // Print more detailed context for debugging
                std.debug.print("Parse error at position {d}: {!}\n", .{ self.position, err });

                // Enhanced error context: Show previous 5 tokens
                std.debug.print("Previous 5 tokens:\n", .{});
                const start_pos = if (self.position > 5) self.position - 5 else 0;
                for (start_pos..self.position) |pos| {
                    if (pos < self.tokens.len) {
                        const prev_token = self.tokens[pos];
                        printToken(prev_token, false);
                    }
                }

                // Show current token
                if (self.position < self.tokens.len) {
                    const current_token = self.tokens[self.position];
                    printToken(current_token, true);
                }

                // Show next 5 tokens
                std.debug.print("Next 5 tokens:\n", .{});
                for (self.position + 1..@min(self.position + 6, self.tokens.len)) |pos| {
                    const next_token = self.tokens[pos];
                    printToken(next_token, false);
                }

                // Ensure we advance past the problematic token
                if (self.position < self.tokens.len) {
                    self.position += 1;
                }

                // Skip to next valid statement
                self.skipToNextStatement();

                // Check if we've reached the end of tokens after skipping
                if (self.position >= self.tokens.len) {
                    std.debug.print("Reached end of tokens after error\n", .{});
                }

                // Reinitialize iterator with updated position
                iter = TokenIterator.init(self.tokens, &self.position, self);

                // Check if the new iterator is valid before continuing
                if (iter.peek() == null or iter.peek().?.kind == .EOF) {
                    std.debug.print("No more valid tokens to parse\n", .{});
                    break;
                }

                continue;
            };

            try statements.append(statement);
        }

        // Create program node with the statements we collected
        // This might be empty if we encountered errors and couldn't parse anything
        return self.createNode(.Program, statements);
    }

    /// Skip to the next valid statement after an error
    fn skipToNextStatement(self: *Parser) void {
        // Track nesting depth of brackets/braces/parentheses
        var paren_depth: usize = 0;
        var brace_depth: usize = 0;
        var bracket_depth: usize = 0;

        while (self.position < self.tokens.len) {
            const token = self.tokens[self.position];

            // Skip all whitespace tokens immediately
            if (token.kind == .Whitespace or
                token.kind == .Newline or
                token.kind == .LineComment or
                token.kind == .BlockComment)
            {
                self.position += 1;
                continue;
            }

            // Update nesting depth
            switch (token.kind) {
                .ParenOpen => paren_depth += 1,
                .ParenClose => {
                    if (paren_depth > 0) paren_depth -= 1;
                },
                .CurlyOpen => brace_depth += 1,
                .CurlyClose => {
                    if (brace_depth > 0) brace_depth -= 1;
                },
                .BracketOpen => bracket_depth += 1,
                .BracketClose => {
                    if (bracket_depth > 0) bracket_depth -= 1;
                },
                else => {},
            }

            // At top level, look for statement boundaries
            if (paren_depth == 0 and brace_depth == 0 and bracket_depth == 0) {
                // Check for statement keyword tokens
                switch (token.kind) {
                    .Var, .Const, .Pub, .EOF, .Fn, .Let => return,
                    .Newline => {
                        // Use a more robust way to check next token
                        var look_ahead = self.position + 1;
                        var found_substantive = false;

                        // Skip non-substantive tokens
                        while (look_ahead < self.tokens.len) {
                            const next = self.tokens[look_ahead];
                            if (next.kind == .Whitespace or
                                next.kind == .LineComment or
                                next.kind == .BlockComment)
                            {
                                look_ahead += 1;
                                continue;
                            }

                            found_substantive = true;

                            switch (next.kind) {
                                .Var, .Const, .Pub, .EOF, .Fn, .Let => {
                                    self.position = look_ahead;
                                    return;
                                },
                                else => break,
                            }
                        }

                        if (!found_substantive) break; // End of file
                    },
                    else => {},
                }
            }

            self.position += 1;
        }
    }

    /// Parse a statement
    fn parseStatement(self: *Parser, iter: *TokenIterator) AstError!*AstNode {
        iter.advanceToSubstantiveToken(); // Skip whitespace before parsing
        const token = iter.peek() orelse return AstError.UnexpectedEof;

        std.debug.print("Statement starts with token: {s}\n", .{@tagName(token.kind)});

        return switch (token.kind) {
            .Pub => {
                iter.advance();
                const next = iter.peek() orelse return AstError.UnexpectedEof;
                return switch (next.kind) {
                    .Fn => try self.parseFunction(iter, true),
                    .Var, .Const => try self.parseDeclaration(iter, true),
                    else => |t| {
                        std.debug.print("Unexpected token after pub: {s}\n", .{@tagName(t)});
                        return AstError.UnexpectedToken;
                    },
                };
            },
            .Var, .Const => try self.parseDeclaration(iter, false),
            .Let => try self.parseLetDeclaration(iter),
            .Fn => try self.parseFunction(iter, false),
            .Return => try self.parseReturnStatement(iter), // Add case for return statements
            .Identifier => {
                // This could be an assignment or just an expression
                const ident_expr = try self.parseExpression(iter);

                // After parsing the expression, check if it's followed by an equals sign
                iter.advanceToSubstantiveToken();
                if (iter.peek()) |next_token| {
                    if (next_token.kind == .Equals) {
                        iter.advance(); // Consume the equals sign
                        iter.advanceToSubstantiveToken(); // Skip whitespace after equals

                        // Parse the right side of the assignment
                        const value_expr = try self.parseExpression(iter);

                        // Create assignment node
                        return self.createNode(.AssignStmt, AssignStmt{
                            .target = ident_expr,
                            .value = value_expr,
                        });
                    }
                }

                // Not an assignment, just return the expression
                return ident_expr;
            },
            else => {
                std.debug.print("Parsing expression statement\n", .{});
                const expr = try self.parseExpression(iter);
                std.debug.print("Expression statement type: {s}\n", .{@tagName(expr.*)});
                return expr;
            },
        };
    }

    /// Parse variable or constant declaration
    fn parseDeclaration(self: *Parser, iter: *TokenIterator, is_public: bool) AstError!*AstNode {
        iter.advanceToSubstantiveToken(); // Skip whitespace before parsing

        // Check for var/const
        const token = iter.peek() orelse return AstError.UnexpectedEof;
        const is_const = switch (token.kind) {
            .Var => false,
            .Const => true,
            else => |t| {
                std.debug.print("Expected var or const keyword. Got: {}\n", .{t});
                return AstError.UnexpectedToken;
            },
        };
        iter.advance();

        // Get identifier name
        iter.advanceToSubstantiveToken(); // Explicitly skip whitespace before identifier
        const name_token = iter.peek() orelse return AstError.ExpectedIdentifier;
        if (name_token.kind != .Identifier) return AstError.ExpectedIdentifier;
        const name = name_token.value.?;
        iter.advance();

        // Parse equals sign and value
        try iter.expectToken(.Equals);
        const value = try self.parseExpression(iter);

        // Create the appropriate node based on whether it's const or var
        if (is_const) {
            return self.createNode(.ConstDecl, ConstDecl{
                .is_public = is_public,
                .name = name,
                .value = value,
            });
        } else {
            return self.createNode(.VarDecl, VarDecl{
                .is_public = is_public,
                .name = name,
                .value = value,
            });
        }
    }

    /// Parse a let declaration
    fn parseLetDeclaration(self: *Parser, iter: *TokenIterator) AstError!*AstNode {
        try iter.expectTokenSkipWhitespace(.Let);

        // Get identifier name
        iter.advanceToSubstantiveToken(); // Skip whitespace before identifier
        const name_token = iter.peek() orelse return AstError.ExpectedLetIdentifier;
        if (name_token.kind != .Identifier) return AstError.ExpectedLetIdentifier;
        const name = name_token.value.?;
        iter.advance();

        // Parse equals sign and value
        try iter.expectToken(.Equals);
        const value = try self.parseExpression(iter);

        return self.createNode(.LetDecl, LetDecl{
            .name = name,
            .value = value,
        });
    }

    // Add a helper function to check if a token kind represents a type

    // Convert a type token to an identifier node
    fn typeTokenToIdentifier(self: *Parser, token: LexToken) !*AstNode {
        // For built-in type tokens, create a TypeNode with Builtin variant
        if (tokenKindToBuiltinType(token.kind)) |builtin_type| {
            return try self.createNode(.TypeNode, TypeInfo{ .Builtin = builtin_type });
        }

        // For identifiers, create a TypeNode with Reference variant pointing to an Identifier
        if (token.kind == .Identifier) {
            const ident_node = try self.createNode(.Identifier, token.value.?);
            return try self.createNode(.TypeNode, TypeInfo{ .Reference = .{
                .definition = ident_node,
            } });
        }

        std.debug.print("Not a valid type token: {s}\n", .{@tagName(token.kind)});
        return AstError.ExpectedIdentifier;
    }

    /// Parse a function declaration
    fn parseFunction(self: *Parser, iter: *TokenIterator, is_public: bool) AstError!*AstNode {
        try iter.expectTokenSkipWhitespace(.Fn);

        // Get function name
        iter.advanceToSubstantiveToken(); // Skip whitespace before name
        const name_token = iter.peek() orelse return AstError.ExpectedIdentifier;
        if (name_token.kind != .Identifier) return AstError.ExpectedIdentifier;
        const name = name_token.value.?;
        iter.advance();

        // Parse parameters
        try iter.expectToken(.ParenOpen);
        var params = std.ArrayList(Parameter).init(self.allocator);

        // Parse each parameter if there are any
        if (!iter.consume(.ParenClose)) {
            while (true) {
                // First check if we're immediately closing the parameter list (for empty params)
                if (iter.peek()) |token| {
                    if (token.kind == .ParenClose) {
                        iter.advance();
                        break;
                    }
                }

                // Get parameter name - at this point we must have an identifier
                const param_name_token = iter.peek() orelse return AstError.ExpectedParamName;
                if (param_name_token.kind != .Identifier) {
                    std.debug.print("Expected parameter name, found: {s}\n", .{@tagName(param_name_token.kind)});
                    return AstError.ExpectedParamName;
                }
                const param_name = param_name_token.value.?;
                iter.advance();

                // Check for type annotation
                var param_type: ?*AstNode = null;
                if (iter.consume(.Colon)) {
                    // Parse type (should be an identifier like i32)
                    const type_token = iter.peek() orelse return AstError.ExpectedIdentifier;

                    // Debug what we're getting
                    std.debug.print("Parameter type token: {s}\n", .{@tagName(type_token.kind)});

                    param_type = try self.typeTokenToIdentifier(type_token);
                    iter.advance();
                }

                // Add parameter to list
                try params.append(Parameter{
                    .name = param_name,
                    .type_expr = param_type,
                });

                // Check for end of parameter list or comma
                // Use our new peek function to handle whitespace and newlines properly
                const next_token = iter.peek() orelse return AstError.UnexpectedEof;

                if (next_token.kind == .ParenClose) {
                    iter.advance(); // Consume the closing parenthesis
                    break;
                } else if (next_token.kind == .Comma) {
                    iter.advance(); // Consume the comma

                    // After a comma, check for trailing whitespace and newlines
                    const after_comma = iter.peekNextNonWhiteOrNewLine();
                    if (after_comma) |token| {
                        if (token.kind == .ParenClose) {
                            // We have a trailing comma, consume tokens until and including the closing paren
                            while (iter.peek()) |t| {
                                if (t.kind == .ParenClose) {
                                    iter.advance();
                                    break;
                                }
                                iter.advance();
                            }
                            break;
                        }
                        // Otherwise we'll loop back and expect another parameter
                    } else {
                        return AstError.UnexpectedEof;
                    }
                } else {
                    std.debug.print("Expected comma or closing parenthesis after parameter, found: {s}\n", .{@tagName(next_token.kind)});
                    return AstError.UnexpectedToken;
                }
            }
        }

        // Parse return type if present
        var return_type: ?*AstNode = null;

        // After the parameters, the next token should be either a return type or an opening brace
        const next_token = iter.peek() orelse return AstError.ExpectedFunctionBody;

        // Check if this token is a type token (could be an identifier or a built-in type like i32)
        if (next_token.kind != .CurlyOpen) {
            // If it's not the opening brace of the function body, it must be a return type
            std.debug.print("Found return type token: {s}\n", .{@tagName(next_token.kind)});

            // Parse return type using our dedicated type expression parser
            return_type = try self.parseTypeExpression(iter);
        }

        // Parse function body
        try iter.expectToken(.CurlyOpen);
        const body = try self.parseBlockStatements(iter);

        return self.createNode(.FunctionDecl, FunctionDecl{
            .is_public = is_public,
            .name = name,
            .parameters = params,
            .return_type = return_type,
            .body = body,
        });
    }

    /// Parse a block of statements
    fn parseBlockStatements(self: *Parser, iter: *TokenIterator) AstError!*AstNode {
        var statements = std.ArrayList(*AstNode).init(self.allocator);

        while (true) {
            // Explicitly skip whitespace before checking for closing brace
            iter.advanceToSubstantiveToken();

            const token = iter.peek();
            if (token == null) {
                return AstError.ExpectedRightBrace; // Better error message than UnexpectedEof
            }

            if (token.?.kind == .CurlyClose) {
                iter.advance(); // Consume closing brace
                break;
            }

            // Parse statement or expression as the last statement
            const is_last = token.?.kind == .CurlyClose or token.?.kind == .EOF;

            if (is_last) {
                // Last statement can be just an expression (for implicit return)
                const expr = try self.parseExpression(iter);
                try statements.append(expr);
            } else {
                std.debug.print("Parsing statement in block\n", .{});
                const stmt = try self.parseStatement(iter);

                // For easier debugging, print what we parsed
                std.debug.print("Parsed statement of type: {s}\n", .{@tagName(stmt.*)});

                try statements.append(stmt);
            }
        }

        return self.createNode(.BlockStmt, statements);
    }

    /// Parse an expression
    fn parseExpression(self: *Parser, iter: *TokenIterator) AstError!*AstNode {
        // Special case for standalone struct initializer
        if (iter.peek()) |token| {
            if (token.kind == .Dot) {
                std.debug.print("Found potential standalone struct initializer\n", .{});

                // Peek ahead to see if it's followed by an opening brace
                iter.advance(); // Consume the dot
                iter.advanceToSubstantiveToken();

                if (iter.peek()) |next| {
                    if (next.kind == .CurlyOpen) {
                        std.debug.print("Confirmed standalone .{{}} initializer\n", .{});
                        iter.advance(); // Consume the {
                        return self.parseStructInitialization(iter, .Auto);
                    }
                }

                // If not followed by {, rewind position to before the dot
                // and let the binary expression parser handle it normally
                std.debug.print("Not a standalone initializer, backtracking\n", .{});
                iter.position.* -= 1; // Rewind one token
                //iter = TokenIterator.init(self.tokens, &self.position, self);
            }
        }

        // After parsing the initial expression, check if it's part of an access chain
        var result = try self.parseBinaryExpression(iter, 0);

        // Debug the result type
        std.debug.print("Expression result type: {s}\n", .{@tagName(result.*)});

        // Convert nested MemberExpr structures to a single Accessor node when appropriate
        if (self.isNestedMemberExpr(result)) {
            std.debug.print("Converting nested member expressions to accessor\n", .{});
            result = try self.convertToAccessor(result);
        }

        return result;
    }

    /// Check if a node is part of a nested member expression chain
    fn isNestedMemberExpr(self: *Parser, node: *AstNode) bool {
        _ = self;
        if (node.* != .MemberExpr) return false;

        std.debug.print("Checking for nested member expr\n", .{});

        // Check if the object is itself a MemberExpr
        const is_nested = node.MemberExpr.object.* == .MemberExpr;
        if (is_nested) {
            std.debug.print("Found nested member expr\n", .{});
        }
        return is_nested;
    }

    /// Convert a nested chain of MemberExpr nodes to a single Accessor node
    fn convertToAccessor(self: *Parser, node: *AstNode) !*AstNode {
        std.debug.print("Converting to accessor\n", .{});

        var parts = std.ArrayList(*AstNode).init(self.allocator);
        var current = node;
        var base: *AstNode = undefined;

        // Traverse the chain from the end to the beginning
        while (current.* == .MemberExpr) {
            try parts.append(current.MemberExpr.property);

            // If the object is a regular identifier, we've reached the base
            if (current.MemberExpr.object.* != .MemberExpr) {
                base = current.MemberExpr.object;
                break;
            }

            current = current.MemberExpr.object;
        }

        // Reverse the parts array since we collected them in reverse order
        var i: usize = 0;
        var j: usize = parts.items.len - 1;
        while (i < j) {
            const temp = parts.items[i];
            parts.items[i] = parts.items[j];
            parts.items[j] = temp;
            i += 1;
            j -= 1;
        }

        // Create the accessor node
        return self.createNode(.Accessor, Accessor{
            .base = base,
            .parts = parts,
        });
    }

    /// Parse a binary expression with operator precedence
    fn parseBinaryExpression(self: *Parser, iter: *TokenIterator, min_precedence: u8) !*AstNode {
        // Skip whitespace before parsing the left operand
        iter.advanceToSubstantiveToken();
        var left = try self.parsePrimaryExpression(iter);

        // Look for binary operators and apply precedence rules
        while (true) {
            // Explicitly skip all whitespace before checking for binary operator
            iter.advanceToSubstantiveToken();

            // Check if next token is an operator
            const next_token = iter.peek() orelse break; // End of input, not an error

            // Skip "equals" as it's not a binary operator but an assignment operator
            if (next_token.kind == .Equals) break;

            const op = tokenToBinaryOp(next_token.kind) orelse break;

            const precedence = getOperatorPrecedence(op);
            if (precedence < min_precedence) break;

            // Consume the operator token
            iter.advance();

            // Skip whitespace after operator before parsing right-hand expression
            iter.advanceToSubstantiveToken();

            // Parse the right operand with higher precedence
            const right = try self.parseBinaryExpression(iter, precedence + 1);

            // Create binary expression node
            left = try self.createNode(.BinaryExpr, BinaryExpr{
                .op = op,
                .left = left,
                .right = right,
            });
        }

        return left;
    }

    /// Parse a primary expression (literals, identifiers, etc.)
    fn parsePrimaryExpression(self: *Parser, iter: *TokenIterator) AstError!*AstNode {
        // Explicitly advance through any whitespace or newlines
        iter.advanceToSubstantiveToken();

        const token = iter.peek() orelse return AstError.ExpectedExpression;
        self.error_location = self.position;

        std.debug.print("Parsing primary expression, token: {s}\n", .{@tagName(token.kind)});

        // Now handle the expression
        var expr = switch (token.kind) {
            // Unary operators
            .Minus, .Exclamation, .Tilde => {
                iter.advance();
                const operand = try self.parsePrimaryExpression(iter);

                const op: UnaryOp = switch (token.kind) {
                    .Minus => .Negate,
                    .Exclamation => .LogicalNot,
                    .Tilde => .BitwiseNot,
                    else => unreachable,
                };

                return self.createNode(.UnaryExpr, UnaryExpr{
                    .op = op,
                    .operand = operand,
                });
            },

            // Number literal
            .NumberLiteral => {
                iter.advance();
                const value = token.getAsIntValue() catch return AstError.InvalidToken;
                return self.createNode(.NumberLiteral, value);
            },

            // Float literal
            .FloatLiteral => {
                iter.advance();
                const value = token.getAsFloatValue() catch return AstError.InvalidToken;
                return self.createNode(.FloatLiteral, value);
            },

            // String literal
            .StringLiteral => {
                iter.advance();
                return self.createNode(.StringLiteral, token.value.?);
            },

            // Boolean literals
            .True, .False => {
                iter.advance();
                return self.createNode(.BoolLiteral, token.kind == .True);
            },

            // Identifier (could be function call, member access, or struct initialization)
            .Identifier => {
                const ident_value = token.value.?;
                std.debug.print("Found identifier: {s}\n", .{ident_value});
                iter.advance();

                // IMPORTANT: Check immediately if this is a function call
                iter.advanceToSubstantiveToken();
                if (iter.peek()) |next| {
                    std.debug.print("After identifier, next token: {s}\n", .{@tagName(next.kind)});
                    if (next.kind == .ParenOpen) {
                        std.debug.print("Found function call to {s}\n", .{ident_value});
                        const ident = try self.createNode(.Identifier, ident_value);
                        return self.parseCallExpression(iter, ident);
                    } else if (next.kind == .Dot) {
                        // If we see a dot after an identifier, this is the start of a member access chain
                        const ident_node = try self.createNode(.Identifier, ident_value);
                        return self.parseMemberAccessChain(iter, ident_node);
                    }
                }

                // Not a function call or member access, return the identifier
                return self.createNode(.Identifier, ident_value);
            },

            // Parenthesized expression
            .ParenOpen => {
                iter.advance();
                const expr = try self.parseExpression(iter);
                try iter.expectToken(.ParenClose);
                return expr;
            },

            // Import expression
            .Import => {
                iter.advance();

                // Expect opening parenthesis
                if (!iter.consume(.ParenOpen)) {
                    return AstError.ExpectedClosingParen;
                }

                // Expect string literal path
                const path_token = iter.peek() orelse return AstError.ExpectedString;
                if (path_token.kind != .StringLiteral) {
                    return AstError.ExpectedString;
                }

                const path = path_token.value.?;
                iter.advance();

                // Expect closing parenthesis
                if (!iter.consume(.ParenClose)) {
                    return AstError.ExpectedClosingParen;
                }

                return self.createNode(.ImportExpr, path);
            },

            // Standalone struct initializer (.{})
            .Dot => {
                iter.advance();
                iter.advanceToSubstantiveToken(); // Skip any whitespace after the dot

                std.debug.print("After dot in primary expression, next token: {s}\n", .{if (iter.peek()) |t| @tagName(t.kind) else "null"});

                // Check if it's a struct initializer (.{})
                if (iter.peek()) |t| {
                    if (t.kind == .CurlyOpen) {
                        iter.advance(); // Consume the {
                        return self.parseStructInitialization(iter, .Auto);
                    }
                }

                // Error - standalone dot needs to be followed by {
                std.debug.print("Expected a '{{' after standalone '.'\n", .{});
                return AstError.ExpectedInitializer;
            },

            // GC managed object
            .Dollar => {
                iter.advance();
                if (iter.consume(.CurlyOpen)) {
                    return self.parseStructInitialization(iter, .GC);
                } else {
                    return AstError.ExpectedInitializer;
                }
            },

            // Scope managed object
            .Hash => {
                iter.advance();
                if (iter.consume(.CurlyOpen)) {
                    return self.parseStructInitialization(iter, .Scope);
                } else {
                    return AstError.ExpectedInitializer;
                }
            },

            // Stack based object
            .At => { // @ is the same token as Import in many lexers
                iter.advance();
                if (iter.consume(.CurlyOpen)) {
                    return self.parseStructInitialization(iter, .Stack);
                } else {
                    // Continue with normal import parsing
                    // Expect opening parenthesis
                    if (!iter.consume(.ParenOpen)) {
                        return AstError.ExpectedClosingParen;
                    }

                    // Expect string literal path
                    const path_token = iter.peek() orelse return AstError.ExpectedString;
                    if (path_token.kind != .StringLiteral) {
                        return AstError.ExpectedString;
                    }

                    const path = path_token.value.?;
                    iter.advance();

                    // Expect closing parenthesis
                    if (!iter.consume(.ParenClose)) {
                        return AstError.ExpectedClosingParen;
                    }

                    return self.createNode(.ImportExpr, path);
                }
            },

            else => return AstError.ExpectedExpression,
        };

        // Access chain handling - we should only reach here for non-identifiers
        // or identifiers that didn't immediately have a dot or paren following them
        expr = try self.parseAccessChain(iter, expr);
        return expr;
    }

    /// Parse a member access chain specifically (for chains starting with an identifier)
    fn parseMemberAccessChain(self: *Parser, iter: *TokenIterator, base: *AstNode) !*AstNode {
        std.debug.print("Starting member access chain with base: ", .{});
        if (base.* == .Identifier) {
            std.debug.print("{s}\n", .{base.Identifier});
        } else {
            std.debug.print("<expr>\n", .{});
        }

        // Create accessor with the base identifier
        var parts = std.ArrayList(*AstNode).init(self.allocator);

        // Parse all parts of the chain
        var current = base;
        var has_parts = false;

        while (true) {
            iter.advanceToSubstantiveToken();
            const next_token = iter.peek() orelse break;

            if (next_token.kind == .Dot) {
                iter.advance(); // Consume dot
                iter.advanceToSubstantiveToken();

                // Check for struct initializer (.{})
                if (iter.peek()) |t| {
                    if (t.kind == .CurlyOpen) {
                        iter.advance(); // Consume {
                        return self.parseStructInitialization(iter, .Auto);
                    }
                }

                // Must be property access - expect identifier
                const prop_token = iter.peek() orelse return AstError.ExpectedIdentifier;
                if (prop_token.kind != .Identifier) {
                    std.debug.print("Expected identifier after dot, got: {s}\n", .{@tagName(prop_token.kind)});
                    return AstError.ExpectedIdentifier;
                }

                const prop_name = try self.createNode(.Identifier, prop_token.value.?);
                iter.advance(); // Consume property name

                // Add property to chain parts
                try parts.append(prop_name);
                has_parts = true;

                // Also update current to support regular member expression fallback
                current = try self.createNode(.MemberExpr, MemberExpr{
                    .object = current,
                    .property = prop_name,
                });

                std.debug.print("Added chain part: {s}\n", .{prop_token.value.?});

                // Continue parsing chain
                continue;
            } else if (next_token.kind == .ParenOpen) {
                // Function call at end of chain
                std.debug.print("Found function call at end of member chain\n", .{});

                // If chain has parts, create an accessor
                if (has_parts) {
                    // Create proper accessor
                    const accessor = try self.createNode(.Accessor, Accessor{
                        .base = base,
                        .parts = parts,
                    });

                    // Now parse function call with accessor as callee
                    return self.parseCallExpression(iter, accessor);
                } else {
                    // Just a simple function call
                    return self.parseCallExpression(iter, current);
                }
            } else {
                // End of chain without function call
                break;
            }
        }

        // If we collected chain parts, create accessor
        if (has_parts) {
            std.debug.print("Creating accessor with {d} parts (no function call)\n", .{parts.items.len});
            return self.createNode(.Accessor, Accessor{
                .base = base,
                .parts = parts,
            });
        }

        // Otherwise return the current expression
        return current;
    }

    /// Parse a chain of accessors (a.b.c) or member access with function calls
    fn parseAccessChain(self: *Parser, iter: *TokenIterator, base: *AstNode) !*AstNode {
        // For identifiers, we should have already handled chains in parsePrimaryExpression
        // This is only for handling chains that start with non-identifiers
        if (base.* == .Identifier) {
            // Check if next token is a dot
            iter.advanceToSubstantiveToken();
            if (iter.peek()) |next_token| {
                if (next_token.kind == .Dot) {
                    // This is a member chain starting with an identifier
                    return self.parseMemberAccessChain(iter, base);
                }
            }
            // Not a chain, return the base
            return base;
        }

        // Handle non-identifier base expressions
        var expr = base;

        // Check for member access or function call after expression
        while (true) {
            iter.advanceToSubstantiveToken();
            const next_token = iter.peek() orelse break;

            if (next_token.kind == .Dot) {
                // Handle member access
                iter.advance(); // Consume dot
                iter.advanceToSubstantiveToken();

                // Check for struct initializer (.{})
                if (iter.peek()) |t| {
                    if (t.kind == .CurlyOpen) {
                        iter.advance(); // Consume {
                        return self.parseStructInitialization(iter, .Auto);
                    }
                }

                // Regular member access
                const prop_token = iter.peek() orelse return AstError.ExpectedIdentifier;
                if (prop_token.kind != .Identifier) {
                    std.debug.print("Expected identifier after dot, got: {s}\n", .{@tagName(prop_token.kind)});
                    return AstError.ExpectedIdentifier;
                }

                const prop_name = try self.createNode(.Identifier, prop_token.value.?);
                iter.advance(); // Consume property name

                // Create member expression
                expr = try self.createNode(.MemberExpr, MemberExpr{
                    .object = expr,
                    .property = prop_name,
                });

                // Continue parsing chain
                continue;
            } else if (next_token.kind == .ParenOpen) {
                // Handle function call
                return self.parseCallExpression(iter, expr);
            } else {
                // No more chaining
                break;
            }
        }

        return expr;
    }

    /// Parse a function call expression
    fn parseCallExpression(self: *Parser, iter: *TokenIterator, callee: *AstNode) !*AstNode {
        // Skip any whitespace between the function name/member and open parenthesis
        try iter.expectTokenSkipWhitespace(.ParenOpen);

        // Print detailed information about the callee for debugging
        std.debug.print("FUNCTION CALL DETAILS: ", .{});
        switch (callee.*) {
            .Identifier => |ident| {
                std.debug.print("Direct call to function '{s}'\n", .{ident});
            },
            .MemberExpr => {
                // Print member expression details
                var callee_str = std.ArrayList(u8).init(self.allocator);
                defer callee_str.deinit();

                // Recursively print the member expression for debugging
                try self.printMemberExprToBuffer(callee, &callee_str);
                std.debug.print("Method call to '{s}'\n", .{callee_str.items});
            },
            .Accessor => {
                // Print the full accessor chain
                var output = std.ArrayList(u8).init(self.allocator);
                defer output.deinit();

                try self.printAccessorToBuffer(callee, &output);
                std.debug.print("Accessor chain call to: '{s}'\n", .{output.items});
            },
            else => {
                std.debug.print("Call to complex expression\n", .{});
            },
        }

        std.debug.print("FOUND FUNCTION CALL ", .{});

        var args = std.ArrayList(*AstNode).init(self.allocator);

        // Explicitly skip all whitespace and newlines before checking for arguments
        iter.advanceToSubstantiveToken();

        // Empty argument list handling
        if (iter.peek()) |token| {
            if (token.kind == .ParenClose) {
                iter.advance(); // Consume the closing parenthesis
                // Empty argument list, return early
                return self.createNode(.CallExpr, CallExpr{
                    .callee = callee,
                    .arguments = args,
                });
            }
        }

        // Parse arguments with rigorous whitespace handling
        while (true) {
            // Skip any whitespace/newlines before parsing the argument
            iter.advanceToSubstantiveToken();

            // Parse the argument expression
            const arg = try self.parseExpression(iter);
            try args.append(arg);

            // Skip any whitespace/newlines after the argument
            iter.advanceToSubstantiveToken();

            // Debug the current token after parsing an argument
            std.debug.print("After argument: token is ", .{});
            if (iter.peek()) |t| {
                std.debug.print("{s}\n", .{@tagName(t.kind)});
            } else {
                std.debug.print("null (EOF)\n", .{});
            }

            // Check for comma or closing parenthesis
            if (iter.peek()) |token| {
                if (token.kind == .ParenClose) {
                    // End of argument list
                    iter.advance(); // Consume closing parenthesis
                    break;
                } else if (token.kind == .Comma) {
                    // Found a comma - we have more arguments
                    std.debug.print("Found comma, expecting another argument\n", .{});
                    iter.advance(); // Consume comma

                    // Thoroughly skip ALL whitespace and newlines after the comma
                    iter.advanceToSubstantiveToken();

                    // Peek ahead (skipping whitespace) to check for trailing comma
                    const after_comma = iter.peek();
                    if (after_comma) |token_after_comma| {
                        std.debug.print("After comma, token is {s}\n", .{@tagName(token_after_comma.kind)});
                        if (token_after_comma.kind == .ParenClose) {
                            // We have a trailing comma followed by closing paren - end of args
                            iter.advance(); // Consume the closing parenthesis
                            break;
                        }
                        // Otherwise we'll continue to parse the next argument
                    } else {
                        // Hit EOF after comma
                        return AstError.ExpectedClosingParen;
                    }

                    // Continue loop to parse next argument
                    continue;
                } else {
                    // Unexpected token after argument
                    std.debug.print("Expected comma or closing parenthesis after function argument, found: {s}\n", .{@tagName(token.kind)});
                    if (callee.* == .Identifier) {
                        std.debug.print("In function call to '{s}', argument #{d}\n", .{ callee.Identifier, args.items.len });
                    }
                    return AstError.UnexpectedToken;
                }
            } else {
                // Hit EOF while parsing arguments
                return AstError.ExpectedClosingParen;
            }
        }

        // For debugging: print information about the function call
        if (callee.* == .Identifier) {
            std.debug.print("Successfully parsed function call to '{s}' with {d} arguments\n", .{ callee.Identifier, args.items.len });
        } else if (callee.* == .MemberExpr) {
            const obj = callee.MemberExpr.object;
            const prop = callee.MemberExpr.property;
            if (obj.* == .Identifier and prop.* == .Identifier) {
                std.debug.print("Successfully parsed method call {s}.{s} with {d} arguments\n", .{ obj.Identifier, prop.Identifier, args.items.len });
            }
        }

        return self.createNode(.CallExpr, CallExpr{
            .callee = callee,
            .arguments = args,
        });
    }

    /// Helper to recursively print a member expression to a buffer
    fn printMemberExprToBuffer(self: *Parser, node: *AstNode, buffer: *std.ArrayList(u8)) !void {
        if (node.* == .MemberExpr) {
            // Print the object part
            try self.printMemberExprToBuffer(node.MemberExpr.object, buffer);

            // Print the dot
            try buffer.appendSlice(".");

            // Print the property
            if (node.MemberExpr.property.* == .Identifier) {
                try buffer.appendSlice(node.MemberExpr.property.Identifier);
            } else {
                try buffer.appendSlice("<expr>");
            }
        } else if (node.* == .Identifier) {
            try buffer.appendSlice(node.Identifier);
        } else {
            try buffer.appendSlice("<expr>");
        }
    }

    /// Helper to print an accessor to a buffer
    fn printAccessorToBuffer(self: *Parser, node: *AstNode, buffer: *std.ArrayList(u8)) !void {
        _ = self;
        if (node.* != .Accessor) return;

        // Print the base
        if (node.Accessor.base.* == .Identifier) {
            try buffer.appendSlice(node.Accessor.base.Identifier);
        } else {
            try buffer.appendSlice("<expr>");
        }

        // Print each part
        for (node.Accessor.parts.items) |part| {
            try buffer.appendSlice(".");
            if (part.* == .Identifier) {
                try buffer.appendSlice(part.Identifier);
            } else {
                try buffer.appendSlice("<expr>");
            }
        }
    }

    /// Parse a struct initialization
    fn parseStructInitialization(self: *Parser, iter: *TokenIterator, memory_type: MemoryManagementType) !*AstNode {
        var fields = std.ArrayList(*AstNode).init(self.allocator);

        // Parse fields if there are any
        if (!iter.consumeSkipWhitespace(.CurlyClose)) {
            while (true) {
                // Skip whitespace before parsing fields
                iter.advanceToSubstantiveToken();

                const field = try self.parseExpression(iter);
                try fields.append(field);

                iter.advanceToSubstantiveToken(); // Skip whitespace after field
                if (iter.consume(.CurlyClose)) break;

                try iter.expectTokenSkipWhitespace(.Comma);

                // After comma, check for trailing whitespace and newlines followed by closing brace
                iter.advanceToSubstantiveToken();
                const after_comma = iter.peek();

                // We'll only return EOF error, not 'expected expression' for whitespace
                if (after_comma) |token| {
                    if (token.kind == .CurlyClose) {
                        iter.advance(); // Consume the closing brace
                        break;
                    }
                    // Otherwise we'll continue to the next field
                } else {
                    return AstError.ExpectedClosingBrace; // Change from UnexpectedEof to make error more specific
                }
            }
        }

        return self.createNode(.StructInit, StructInit{
            .fields = fields,
            .funcs = std.ArrayList(*AstNode).init(self.allocator),
            .memory_type = memory_type,
        });
    }

    /// Parse a return statement
    fn parseReturnStatement(self: *Parser, iter: *TokenIterator) AstError!*AstNode {
        try iter.expectTokenSkipWhitespace(.Return);

        // Skip whitespace after 'return' keyword
        iter.advanceToSubstantiveToken();

        // Check if there's an expression after return
        // If the next token is a closing brace, it's a return with no value
        var value: ?*AstNode = null;

        if (iter.peek()) |token| {
            if (token.kind != .CurlyClose) {
                // Parse the return expression
                value = try self.parseExpression(iter);

                // Debug the type of return expression
                std.debug.print("Return expression type: {s}\n", .{@tagName(value.?.*)});
            }
        }

        // No semicolon consumption - this language doesn't use semicolons

        return self.createNode(.ReturnStmt, ReturnStmt{
            .value = value,
        });
    }

    /// Parse a type expression
    fn parseTypeExpression(self: *Parser, iter: *TokenIterator) !*AstNode {
        iter.advanceToSubstantiveToken();
        const token = iter.peek() orelse return AstError.ExpectedIdentifier;

        // Check for builtin type tokens first
        if (tokenKindToBuiltinType(token.kind)) |builtin_type| {
            iter.advance(); // Consume the type token
            return try self.createNode(.TypeNode, TypeInfo{ .Builtin = builtin_type });
        }

        // Handle user-defined types (identifiers)
        if (token.kind == .Identifier) {
            const type_name = token.value.?;
            iter.advance(); // Consume the identifier

            // Check for array syntax (e.g., []Type or [N]Type)
            iter.advanceToSubstantiveToken();
            if (iter.peek()) |next| {
                if (next.kind == .BracketOpen) {
                    iter.advance(); // Consume '['

                    // Parse optional array size
                    var array_size: ?i64 = null;
                    iter.advanceToSubstantiveToken();
                    if (iter.peek()) |size_token| {
                        if (size_token.kind == .NumberLiteral) {
                            const size_value = size_token.getAsIntValue() catch return AstError.InvalidToken;
                            array_size = @intCast(size_value);
                            iter.advance(); // Consume the number
                        }
                    }

                    try iter.expectToken(.BracketClose); // Expect ']'

                    // Parse element type
                    iter.advanceToSubstantiveToken();
                    const element_type = try self.parseTypeExpression(iter);

                    // Create array type node
                    return try self.createNode(.TypeNode, TypeInfo{ .Array = .{
                        .element_type = element_type,
                        .size = array_size,
                    } });
                }
            }

            // Always treat identifiers as references to user-defined types
            // We don't check for built-in types here - those should be handled by the lexer as specific tokens
            const ident_node = try self.createNode(.Identifier, type_name);
            return try self.createNode(.TypeNode, TypeInfo{ .Reference = .{
                .definition = ident_node,
            } });
        }

        std.debug.print("Expected type identifier @pos: {d}", .{token.filePos});
        return AstError.ExpectedIdentifier;
    }

    /// Helper to match a string to a built-in type
    /// Print the AST for debugging
    pub fn print_ast(self: *Parser) void {
        if (self.root) |node| {
            printAstNode(node, 0);
        } else {
            std.debug.print("No AST available - call parse() first\n", .{});
        }
    }
};

/// Print AST node with indentation for debugging
pub fn printAstNode(node: *AstNode, indent: usize) void {
    const spaces = " " ** 64;
    const indent_str = spaces[0..@min(indent, spaces.len)];

    switch (node.*) {
        .Program => |prog| {
            std.debug.print("{s}Program\n", .{indent_str});
            for (prog.items) |statement| {
                printAstNode(statement, indent + 2);
            }
        },
        .VarDecl => |decl| {
            std.debug.print("{s}VarDecl: pub:{}   name:\"{s}\"\n", .{ indent_str, decl.is_public, decl.name });
            printAstNode(decl.value, indent + 2);
        },
        .ConstDecl => |decl| {
            std.debug.print("{s}ConstDecl: pub:{}   name:\"{s}\"\n", .{ indent_str, decl.is_public, decl.name });
            printAstNode(decl.value, indent + 2);
        },
        .FunctionDecl => |func| {
            std.debug.print("{s}FunctionDecl: pub:{} name:\"{s}\" params:{d}\n", .{ indent_str, func.is_public, func.name, func.parameters.items.len });

            for (func.parameters.items, 0..) |param, i| {
                std.debug.print("{s}  Param[{d}]: {s}\n", .{ indent_str, i, param.name });
                if (param.type_expr) |type_expr| {
                    std.debug.print("{s}    Type: ", .{indent_str});
                    printAstNode(type_expr, indent + 6);
                }
            }

            if (func.return_type) |ret_type| {
                std.debug.print("{s}  Return Type: ", .{indent_str});
                printAstNode(ret_type, indent + 4);
            }

            std.debug.print("{s}  Body:\n", .{indent_str});
            printAstNode(func.body, indent + 4);
        },
        .BinaryExpr => |expr| {
            std.debug.print("{s}BinaryExpr: {}\n", .{ indent_str, expr.op });
            printAstNode(expr.left, indent + 2);
            printAstNode(expr.right, indent + 2);
        },
        .UnaryExpr => |expr| {
            const op_str = switch (expr.op) {
                .Negate => "-",
                .BitwiseNot => "~",
                .LogicalNot => "!",
            };
            std.debug.print("{s}UnaryExpr: {s}\n", .{ indent_str, op_str });
            printAstNode(expr.operand, indent + 2);
        },
        .FloatLiteral => |lit| {
            std.debug.print("{s}FloatLiteral: {d}\n", .{ indent_str, lit });
        },
        .NumberLiteral => |lit| {
            std.debug.print("{s}NumberLiteral: {d}\n", .{ indent_str, lit });
        },
        .StringLiteral => |lit| {
            std.debug.print("{s}StringLiteral: \"{s}\"\n", .{ indent_str, lit });
        },
        .BoolLiteral => |lit| {
            std.debug.print("{s}BoolLiteral: {}\n", .{ indent_str, lit });
        },
        .Identifier => |ident| {
            std.debug.print("{s}Identifier: \"{s}\"\n", .{ indent_str, ident });
        },
        .ImportExpr => |import| {
            std.debug.print("{s}Import: \"{s}\"\n", .{ indent_str, import });
        },
        .CallExpr => |call| {
            std.debug.print("{s}FunctionCall: args:{d}\n", .{ indent_str, call.arguments.items.len });
            std.debug.print("{s}  Callee:\n", .{indent_str});
            printAstNode(call.callee, indent + 4);

            for (call.arguments.items, 0..) |arg, i| {
                std.debug.print("{s}  Arg[{d}]:\n", .{ indent_str, i });
                printAstNode(arg, indent + 4);
            }
        },
        .LetDecl => |decl| {
            std.debug.print("{s}LetDecl: name:\"{s}\"\n", .{ indent_str, decl.name });
            printAstNode(decl.value, indent + 2);
        },
        .BlockStmt => |block| {
            std.debug.print("{s}Block: statements:{d}\n", .{ indent_str, block.items.len });
            for (block.items) |stmt| {
                printAstNode(stmt, indent + 2);
            }
        },
        .MemberExpr => |member| {
            std.debug.print("{s}MemberExpr:\n", .{indent_str});
            std.debug.print("{s}  Object:\n", .{indent_str});
            printAstNode(member.object, indent + 4);
            std.debug.print("{s}  Property:\n", .{indent_str});
            printAstNode(member.property, indent + 4);
        },
        .StructInit => |struct_init| {
            const memory_type_str = switch (struct_init.memory_type) {
                .Auto => "auto",
                .GC => "gc",
                .Scope => "scope",
                .Stack => "stack",
            };
            std.debug.print("{s}StructInit: type:{s} fields:{d}\n", .{ indent_str, memory_type_str, struct_init.fields.items.len });
            for (struct_init.fields.items, 0..) |field, i| {
                std.debug.print("{s}  Field[{d}]:\n", .{ indent_str, i });
                printAstNode(field, indent + 4);
            }
        },
        .Accessor => |accessor| {
            var buf = std.ArrayList(u8).init(std.heap.page_allocator);
            defer buf.deinit();

            // Print base
            if (accessor.base.* == .Identifier) {
                buf.writer().print("{s}", .{accessor.base.Identifier}) catch {};
            } else {
                buf.writer().print("<expr>", .{}) catch {};
            }

            // Print parts
            for (accessor.parts.items) |part| {
                if (part.* == .Identifier) {
                    buf.writer().print(".{s}", .{part.Identifier}) catch {};
                } else {
                    buf.writer().print(".<expr>", .{}) catch {};
                }
            }

            std.debug.print("{s}Accessor: \"{s}\"\n", .{ indent_str, buf.items });

            std.debug.print("{s}  Base:\n", .{indent_str});
            printAstNode(accessor.base, indent + 4);

            std.debug.print("{s}  Parts:\n", .{indent_str});
            for (accessor.parts.items, 0..) |part, i| {
                std.debug.print("{s}    Part[{d}]:\n", .{ indent_str, i });
                printAstNode(part, indent + 6);
            }
        },
        .AssignStmt => |assign| {
            std.debug.print("{s}AssignStmt:\n", .{indent_str});
            std.debug.print("{s}  Target:\n", .{indent_str});
            printAstNode(assign.target, indent + 4);
            std.debug.print("{s}  Value:\n", .{indent_str});
            printAstNode(assign.value, indent + 4);
        },
        .ReturnStmt => |ret| {
            std.debug.print("{s}ReturnStmt:\n", .{indent_str});
            if (ret.value) |val| {
                printAstNode(val, indent + 2);
            } else {
                std.debug.print("{s}  <no value>\n", .{indent_str});
            }
        },
        .TypeNode => |type_info| {
            switch (type_info) {
                .Builtin => |builtin| {
                    std.debug.print("{s}TypeNode: Built-in {s}\n", .{ indent_str, builtin.toString() });
                },
                .Reference => |reference| {
                    std.debug.print("{s}TypeNode: Reference to:\n", .{indent_str});
                    printAstNode(reference.definition, indent + 4);
                },
                .Array => |array_info| {
                    if (array_info.size) |size| {
                        std.debug.print("{s}TypeNode: Array[{d}] of:\n", .{ indent_str, size });
                    } else {
                        std.debug.print("{s}TypeNode: Array[] of:\n", .{indent_str});
                    }
                    printAstNode(array_info.element_type, indent + 4);
                },
                .Slice => |slice_info| {
                    std.debug.print("{s}TypeNode: Slice of:\n", .{indent_str});
                    printAstNode(slice_info.element_type, indent + 4);
                },
                .Optional => |opt_info| {
                    std.debug.print("{s}TypeNode: Optional of:\n", .{indent_str});
                    printAstNode(opt_info.base_type, indent + 4);
                },
            }
        },
    }
}

/// Helper to convert token kinds to built-in types
fn tokenKindToBuiltinType(kind: TokenKinds) ?BuiltinType {
    return BuiltinType.fromTokenKind(kind);
}

/// Visitor for AST traversal
pub const AstVisitor = struct {
    const Callback = fn (*AstNode) anyerror!void;
    const PreCallback = fn (*AstNode) anyerror!void;
    const PostCallback = fn (*AstNode) anyerror!void;

    /// Visit all nodes in the AST
    pub fn visit(ast: *const AstNode, pre_callback: ?PreCallback, callback: Callback, post_callback: ?PostCallback) !void {
        if (pre_callback) |pre| {
            try pre(@constCast(ast));
        }

        try callback(@constCast(ast));

        switch (ast.*) {
            .Program => |prog| {
                for (prog.items) |stmt| {
                    try visit(stmt, pre_callback, callback, post_callback);
                }
            },
            .VarDecl => |decl| try visit(decl.value, pre_callback, callback, post_callback),
            .ConstDecl => |decl| try visit(decl.value, pre_callback, callback, post_callback),
            .FunctionDecl => |func| {
                // Visit parameters' type expressions
                for (func.parameters.items) |param| {
                    if (param.type_expr) |type_expr| {
                        try visit(type_expr, pre_callback, callback, post_callback);
                    }
                }

                // Visit return type if it exists
                if (func.return_type) |ret_type| {
                    try visit(ret_type, pre_callback, callback, post_callback);
                }

                // Visit function body
                try visit(func.body, pre_callback, callback, post_callback);
            },
            .BinaryExpr => |expr| {
                try visit(expr.left, pre_callback, callback, post_callback);
                try visit(expr.right, pre_callback, callback, post_callback);
            },
            .UnaryExpr => |expr| try visit(expr.operand, pre_callback, callback, post_callback),
            .CallExpr => |call| {
                try visit(call.callee, pre_callback, callback, post_callback);
                for (call.arguments.items) |arg| {
                    try visit(arg, pre_callback, callback, post_callback);
                }
            },
            .LetDecl => |decl| try visit(decl.value, pre_callback, callback, post_callback),
            .BlockStmt => |block| {
                for (block.items) |stmt| {
                    try visit(stmt, pre_callback, callback, post_callback);
                }
            },
            .MemberExpr => |member| {
                try visit(member.object, pre_callback, callback, post_callback);
                try visit(member.property, pre_callback, callback, post_callback);
            },
            .StructInit => |struct_init| {
                for (struct_init.fields.items) |field| {
                    try visit(field, pre_callback, callback, post_callback);
                }
            },
            .Accessor => |accessor| {
                try visit(accessor.base, pre_callback, callback, post_callback);
                for (accessor.parts.items) |part| {
                    try visit(part, pre_callback, callback, post_callback);
                }
            },
            .AssignStmt => |assign| {
                try visit(assign.target, pre_callback, callback, post_callback);
                try visit(assign.value, pre_callback, callback, post_callback);
            },
            .ReturnStmt => |ret| {
                if (ret.value) |val| {
                    try visit(val, pre_callback, callback, post_callback);
                }
            },
            .TypeNode => |type_info| {
                switch (type_info) {
                    .Builtin => {}, // Nothing to traverse
                    .Reference => |ref| try visit(ref.definition, pre_callback, callback, post_callback),
                    .Array => |array_info| try visit(array_info.element_type, pre_callback, callback, post_callback),
                    .Slice => |slice_info| try visit(slice_info.element_type, pre_callback, callback, post_callback),
                    .Optional => |opt_info| try visit(opt_info.base_type, pre_callback, callback, post_callback),
                }
            },
            .NumberLiteral, .FloatLiteral, .BoolLiteral, .StringLiteral, .Identifier, .ImportExpr => {},
        }

        if (post_callback) |post| {
            try post(@constCast(ast));
        }
    }
};
pub fn isTypeToken(kind: TokenKinds) bool {
    return switch (kind) {
        .Identifier,
        .I8,
        .I16,
        .I32,
        .I64,
        .U8,
        .U16,
        .U32,
        .U64,
        .F32,
        .F64,
        .F80,
        .Bool,
        .String,
        .StringBuilder,
        .ISize,
        .USize,
        .AnyType,

        .None,
        => true,
        else => false,
    };
}
