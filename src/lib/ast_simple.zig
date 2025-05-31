const std = @import("std");
const LexToken = @import("lextoken.zig").LexerToken;
const TokenKinds = @import("tokenkinds.zig").TokenKinds;

/// Simple, clean AST for the Howl programming language
/// Focused on simplicity, maintainability, and performance

// ============================================================================
// Core Types and Enums
// ============================================================================

/// Parse error with token context for better error reporting
pub const ParseError = struct {
    kind: AstError,
    token: ?LexToken, // The token where the error occurred (null if EOF)
    message: []const u8, // Owned by this struct, must be freed
    position: usize, // Token position in the input
    allocator: std.mem.Allocator, // For freeing the message

    pub fn deinit(self: *ParseError) void {
        self.allocator.free(self.message);
    }
};

pub const AstError = error{
    UnexpectedToken,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedOperator,
    UnexpectedEof,
    OutOfMemory,
    InvalidSyntax,
};

/// Unified declaration types
pub const DeclKind = enum {
    immutable, // ::
    mutable, // :=
    @"comptime", // comptime
};

/// Memory management hint for allocations
pub const MemoryHint = enum {
    auto, // .{} - automatic
    gc, // ${} - garbage collected
    scope, // #{} - scope managed
    stack, // @{} - stack allocated
};

/// Binary operators with consistent precedence
pub const BinaryOp = enum {
    // Arithmetic
    add,
    sub,
    mul,
    div,
    mod,
    // Comparison
    eq,
    ne,
    lt,
    gt,
    le,
    ge,
    // Logical
    logical_and,
    logical_or,
    // Bitwise
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr,
    // Special
    range,
    range_inc,
    range_exc,
    pipe,
    // Assignment
    assign,

    pub fn precedence(self: BinaryOp) u8 {
        return switch (self) {
            .logical_or => 1,
            .logical_and => 2,
            .eq, .ne, .lt, .gt, .le, .ge => 3,
            .range, .range_inc, .range_exc => 4,
            .bit_or => 5,
            .bit_xor => 6,
            .bit_and => 7,
            .shl, .shr => 8,
            .add, .sub => 9,
            .mul, .div, .mod => 10,
            .pipe => 1,
            .assign => 0, // Lowest precedence
        };
    }

    pub fn fromToken(kind: TokenKinds) ?BinaryOp {
        return switch (kind) {
            .Plus => .add,
            .Minus => .sub,
            .Asterisk => .mul,
            .Slash => .div,
            .Percent => .mod,
            .DoubleEquals => .eq,
            .ExclamationEquals => .ne,
            .LessThan => .lt,
            .GreaterThan => .gt,
            .LessThanEquals => .le,
            .GreaterThanEquals => .ge,
            .And => .logical_and,
            .Or => .logical_or,
            .Ampersand => .bit_and,
            .Pipe => .bit_or,
            .Caret => .bit_xor,
            .DoubleLessThan => .shl,
            .DoubleGreaterThan => .shr,
            .DotDot => .range,
            .DotDotEquals => .range_inc,
            .DotDotLessThan => .range_exc,
            .PipeGreaterThan => .pipe,
            .Equals => .assign,
            else => null,
        };
    }
};

pub const UnaryOp = enum {
    negate,
    not,
    bit_not,

    pub fn fromToken(kind: TokenKinds) ?UnaryOp {
        return switch (kind) {
            .Minus => .negate,
            .Exclamation => .not,
            .Tilde => .bit_not,
            else => null,
        };
    }
};

// ============================================================================
// AST Node Types
// ============================================================================

/// Simplified AST node with fewer, more consistent types
pub const Node = union(enum) {
    // Literals
    int: i64,
    float: f64,
    string: []const u8,
    char: u8,
    bool: bool,
    identifier: []const u8,

    // Expressions
    binary: BinaryExpr,
    unary: UnaryExpr,
    call: CallExpr,
    member: MemberExpr,
    index: IndexExpr,
    range: RangeExpr,

    // Statements/Declarations
    declaration: Declaration,
    function: Function,
    struct_def: StructDef,
    enum_def: EnumDef,
    return_stmt: ?*Node, // null for bare return
    assignment: Assignment,

    // Control flow
    if_expr: IfExpr,
    match_expr: MatchExpr,
    for_loop: ForLoop,
    while_loop: WhileLoop,

    // Compound
    block: Block,
    struct_init: StructInit,
    array: Array,

    // Special
    import: []const u8,
    type_ref: TypeRef,
    error_node: ErrorNode,
};

pub const ErrorNode = struct {
    kind: AstError,
    message: []const u8,
};

// ============================================================================
// Expression Types
// ============================================================================

pub const BinaryExpr = struct {
    op: BinaryOp,
    left: *Node,
    right: *Node,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    operand: *Node,
};

pub const CallExpr = struct {
    callee: *Node,
    args: []const *Node,
};

pub const MemberExpr = struct {
    object: *Node,
    property: []const u8,
};

pub const IndexExpr = struct {
    object: *Node,
    index: *Node,
};

pub const RangeExpr = struct {
    start: *Node,
    end: *Node,
    inclusive: bool,
};

// ============================================================================
// Statement/Declaration Types
// ============================================================================

pub const Declaration = struct {
    kind: DeclKind,
    name: []const u8,
    type_hint: ?*Node,
    value: *Node,
    is_public: bool = false,
};

pub const Function = struct {
    name: []const u8,
    params: []const Parameter,
    return_type: ?*Node,
    body: *Node,
    is_public: bool = false,
};

pub const Parameter = struct {
    name: []const u8,
    type_hint: ?*Node,
    default_value: ?*Node,
};

pub const StructDef = struct {
    name: []const u8,
    fields: []const Field,
    methods: []const *Node, // Function nodes
    is_public: bool = false,
};

pub const Field = struct {
    name: []const u8,
    type_hint: *Node,
    default_value: ?*Node,
};

pub const EnumDef = struct {
    name: []const u8,
    variants: []const Variant,
    is_public: bool = false,
};

pub const Variant = struct {
    name: []const u8,
    value: ?*Node,
    associated_type: ?*Node,
};

pub const Assignment = struct {
    target: *Node,
    value: *Node,
};

// ============================================================================
// Control Flow Types
// ============================================================================

pub const IfExpr = struct {
    condition: *Node,
    then_branch: *Node,
    else_branch: ?*Node,
};

pub const MatchExpr = struct {
    expr: *Node,
    arms: []const MatchArm,
};

pub const MatchArm = struct {
    pattern: *Node,
    guard: ?*Node,
    body: *Node,
};

pub const ForLoop = struct {
    variable: ?[]const u8,
    iterable: *Node,
    body: *Node,
};

pub const WhileLoop = struct {
    condition: *Node,
    body: *Node,
};

// ============================================================================
// Compound Types
// ============================================================================

pub const Block = struct {
    statements: []const *Node,
};

pub const StructInit = struct {
    type_name: ?[]const u8, // null for anonymous structs
    fields: []const FieldInit,
    memory_hint: MemoryHint = .auto,
};

pub const FieldInit = struct {
    name: []const u8,
    value: *Node,
};

pub const Array = struct {
    elements: []const *Node,
};

pub const TypeRef = struct {
    name: []const u8,
    args: []const *Node, // for generic types
    is_optional: bool = false,
    is_pointer: bool = false,
};

// ============================================================================
// Parser
// ============================================================================

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const LexToken,
    pos: usize = 0,
    allocated_nodes: std.ArrayList(*Node),
    errors: std.ArrayList(ParseError),
    error_sentinel: Node = .{ .error_node = ErrorNode{ .kind = AstError.InvalidSyntax, .message = "Error sentinel node" } },

    pub fn init(allocator: std.mem.Allocator, tokens: []const LexToken) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .allocated_nodes = std.ArrayList(*Node).init(allocator),
            .errors = std.ArrayList(ParseError).init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        // Free all allocated AST nodes
        for (self.allocated_nodes.items) |node| {
            self.freeNode(node);
        }
        self.allocated_nodes.deinit();

        // Free all error messages
        for (self.errors.items) |*error_info| {
            error_info.deinit();
        }
        self.errors.deinit();
    }

    fn freeNode(self: *Parser, node: *Node) void {
        switch (node.*) {
            .binary => |expr| {
                // Child nodes are already tracked and will be freed
                _ = expr;
            },
            .unary => |expr| {
                // Child nodes are already tracked and will be freed
                _ = expr;
            },
            .call => |expr| {
                // Free the args array
                self.allocator.free(expr.args);
            },
            .declaration => |decl| {
                // Child nodes are already tracked and will be freed
                _ = decl;
            },
            .function => |func| {
                // Free the params array
                self.allocator.free(func.params);
            },
            .struct_def => |struct_def| {
                // Free the fields and methods arrays
                self.allocator.free(struct_def.fields);
                self.allocator.free(struct_def.methods);
            },
            .enum_def => |enum_def| {
                // Free the variants array
                self.allocator.free(enum_def.variants);
            },
            .match_expr => |match_expr| {
                // Free the arms array
                self.allocator.free(match_expr.arms);
            },
            .block => |block| {
                // Free the statements array
                self.allocator.free(block.statements);
            },
            .struct_init => |struct_init| {
                // Free the fields array
                self.allocator.free(struct_init.fields);
            },
            .array => |array| {
                // Free the elements array
                self.allocator.free(array.elements);
            },
            .type_ref => |type_ref| {
                // Free the args array
                self.allocator.free(type_ref.args);
            },
            else => {
                // Other node types don't have owned memory beyond the node itself
            },
        }
        // Free the node itself
        self.allocator.destroy(node);
    }

    // ========================================================================
    // Utility Methods
    // ========================================================================

    fn current(self: *Parser) ?LexToken {
        if (self.pos >= self.tokens.len) return null;
        return self.tokens[self.pos];
    }

    fn peek(self: *Parser, offset: usize) ?LexToken {
        const pos = self.pos + offset;
        if (pos >= self.tokens.len) return null;
        return self.tokens[pos];
    }

    fn advance(self: *Parser) void {
        if (self.pos < self.tokens.len) self.pos += 1;
    }

    fn expect(self: *Parser, kind: TokenKinds) AstError!LexToken {
        const token = self.current() orelse {
            self.addError(AstError.UnexpectedEof, "Unexpected end of input");
            return AstError.UnexpectedEof;
        };
        if (token.kind != kind) {
            // Properly allocate the error message
            const message = std.fmt.allocPrint(self.allocator, "Expected '{s}' but found '{s}'", .{ @tagName(kind), @tagName(token.kind) }) catch "Expected token not found";
            self.addError(AstError.UnexpectedToken, message);
            // Free the message since addError makes its own copy
            if (!std.mem.eql(u8, message, "Expected token not found")) {
                self.allocator.free(message);
            }
            return AstError.UnexpectedToken;
        }
        self.advance();
        return token;
    }

    fn match(self: *Parser, kind: TokenKinds) bool {
        if (self.current()) |token| {
            if (token.kind == kind) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn skipWhitespace(self: *Parser) void {
        while (self.current()) |token| {
            switch (token.kind) {
                .Whitespace, .Newline, .LineComment, .BlockComment => self.advance(),
                else => break,
            }
        }
    }

    fn create(self: *Parser, comptime T: type, value: T) AstError!*Node {
        const node = self.allocator.create(Node) catch return AstError.OutOfMemory;
        node.* = value;

        // Track the allocated node for cleanup
        self.allocated_nodes.append(node) catch {
            // If we can't track the node, free it immediately and return error
            self.allocator.destroy(node);
            return AstError.OutOfMemory;
        };

        return node;
    }

    // ========================================================================
    // Error Handling Methods
    // ========================================================================

    /// Add an error to the error list with current token context
    fn addError(self: *Parser, kind: AstError, message: []const u8) void {
        const current_token = self.current();

        // Allocate owned copy of the message
        const owned_message = self.allocator.dupe(u8, message) catch {
            // If we can't allocate the message, print it immediately as fallback
            std.debug.print("PARSE ERROR (memory allocation failed): {s} at position {d}\n", .{ message, self.pos });
            return;
        };

        const error_info = ParseError{
            .kind = kind,
            .token = current_token,
            .message = owned_message,
            .position = self.pos,
            .allocator = self.allocator,
        };
        self.errors.append(error_info) catch {
            // If we can't append the error, free the message and print it immediately as fallback
            self.allocator.free(owned_message);
            std.debug.print("PARSE ERROR (failed to store): {s} at position {d}\n", .{ message, self.pos });
        };
    }

    /// Add an error with specific token context
    fn addErrorAtToken(self: *Parser, kind: AstError, token: ?LexToken, message: []const u8, position: usize) void {
        // Allocate owned copy of the message
        const owned_message = self.allocator.dupe(u8, message) catch {
            // If we can't allocate the message, print it immediately as fallback
            std.debug.print("PARSE ERROR (memory allocation failed): {s} at position {d}\n", .{ message, position });
            return;
        };

        const error_info = ParseError{
            .kind = kind,
            .token = token,
            .message = owned_message,
            .position = position,
            .allocator = self.allocator,
        };
        self.errors.append(error_info) catch {
            // If we can't append the error, free the message and print it immediately as fallback
            self.allocator.free(owned_message);
            std.debug.print("PARSE ERROR (failed to store): {s} at position {d}\n", .{ message, position });
        };
    }

    /// Check if any errors occurred during parsing
    pub fn hasErrors(self: *Parser) bool {
        return self.errors.items.len > 0;
    }

    /// Get the list of errors collected during parsing
    pub fn getErrors(self: *Parser) []const ParseError {
        return self.errors.items;
    }

    /// Print all collected errors with detailed context
    pub fn printErrors(self: *Parser) void {
        if (self.errors.items.len == 0) {
            std.debug.print("No parsing errors found.\n", .{});
            return;
        }

        std.debug.print("\n=== PARSE ERRORS ({d}) ===\n", .{self.errors.items.len});
        for (self.errors.items, 0..) |error_info, i| {
            std.debug.print("\nError {d}: {s}\n", .{ i + 1, @errorName(error_info.kind) });
            std.debug.print("  Message: {s}\n", .{error_info.message});
            std.debug.print("  Position: {d}\n", .{error_info.position});

            if (error_info.token) |token| {
                std.debug.print("  Token: {s}", .{@tagName(token.kind)});
                if (token.value) |value| {
                    std.debug.print(" = '{s}'", .{value});
                }
                std.debug.print("\n", .{});
            } else {
                std.debug.print("  Token: <EOF>\n", .{});
            }

            // Show context around the error
            self.printErrorContext(error_info.position);
        }
        std.debug.print("\n========================\n", .{});
    }

    /// Print context around an error position (previous 2 tokens, current, next 2 tokens)
    fn printErrorContext(self: *Parser, error_pos: usize) void {
        std.debug.print("  Context: ", .{});

        const start_pos = if (error_pos >= 2) error_pos - 2 else 0;
        const end_pos = @min(error_pos + 3, self.tokens.len);

        for (start_pos..end_pos) |i| {
            if (i < self.tokens.len) {
                const token = self.tokens[i];
                if (i == error_pos) {
                    std.debug.print(" >>>{s}<<<", .{@tagName(token.kind)});
                } else {
                    std.debug.print(" {s}", .{@tagName(token.kind)});
                }
            }
        }
        std.debug.print("\n", .{});
    }

    /// Clear all collected errors (useful for error recovery)
    pub fn clearErrors(self: *Parser) void {
        self.errors.clearRetainingCapacity();
    }

    // ========================================================================
    // Error Recovery Methods
    // ========================================================================

    /// Skip tokens until we find a synchronization point for error recovery
    fn synchronize(self: *Parser) void {
        while (self.current()) |token| {
            switch (token.kind) {
                // Statement boundaries - safe places to resume parsing
                .Fn, .Struct, .Enum, .Pub, .Return, .If, .Match, .For, .While, .Comptime => return,
                .CurlyClose => {
                    self.advance(); // consume the closing brace
                    return;
                },
                .EOF => return,
                else => self.advance(),
            }
        }
    }

    /// Try to recover from a parsing error by skipping to a safe sync point
    fn recoverFromError(self: *Parser, error_kind: AstError, message: []const u8) *Node {
        self.addError(error_kind, message);
        self.synchronize();

        // Return an error node to maintain AST structure
        return self.create(Node, .{ .error_node = ErrorNode{
            .kind = error_kind,
            .message = message,
        } }) catch {
            // If we can't even create an error node, return a simple one
            return &self.error_sentinel;
        };
    }

    /// Parse with error recovery - continues parsing even after errors
    pub fn parseWithRecovery(self: *Parser) *Node {
        var statements = std.ArrayList(*Node).init(self.allocator);
        defer statements.deinit();

        while (self.current() != null) {
            self.skipWhitespace();

            // Check if we've reached EOF after skipping whitespace
            const token = self.current() orelse break;
            if (token.kind == .EOF) break;

            if (self.parseStatement()) |stmt| {
                statements.append(stmt) catch break;
            } else |err| {
                // On error, try to recover and continue parsing
                const error_msg = switch (err) {
                    AstError.UnexpectedToken => "Unexpected token in statement",
                    AstError.ExpectedIdentifier => "Expected identifier",
                    AstError.ExpectedExpression => "Expected expression",
                    AstError.UnexpectedEof => "Unexpected end of file",
                    else => "Parse error",
                };
                const error_node = self.recoverFromError(err, error_msg);
                statements.append(error_node) catch break;
            }
        }

        return self.create(Node, .{ .block = Block{
            .statements = statements.toOwnedSlice() catch &[_]*Node{},
        } }) catch &self.error_sentinel;
    }

    // ========================================================================
    // Main Parsing Methods
    // ========================================================================

    pub fn parse(self: *Parser) AstError!*Node {
        var statements = std.ArrayList(*Node).init(self.allocator);
        defer statements.deinit();

        while (self.current() != null) {
            self.skipWhitespace();

            // Check if we've reached EOF after skipping whitespace
            const token = self.current() orelse break;
            if (token.kind == .EOF) break;

            const stmt = self.parseStatement() catch |err| {
                return err;
            };
            try statements.append(stmt);
        }

        return self.create(Node, .{ .block = Block{
            .statements = statements.toOwnedSlice() catch return AstError.OutOfMemory,
        } });
    }

    fn parseStatement(self: *Parser) AstError!*Node {
        self.skipWhitespace();
        const token = self.current() orelse {
            self.addError(AstError.UnexpectedEof, "Expected statement but reached end of input");
            return AstError.UnexpectedEof;
        };

        return switch (token.kind) {
            .Pub => self.parsePublicDeclaration(),
            .Fn => self.parseFunction(false),
            .Enum => self.parseEnum(false),
            .Return => self.parseReturn(),
            .If => self.parseIf(),
            .Match => self.parseMatch(),
            .For => self.parseFor(),
            .While => self.parseWhile(),
            .Identifier => self.parseIdentifierStatement(),
            .Comptime => self.parseComptimeDeclaration(false),
            else => self.parseExpression(),
        };
    }

    fn parsePublicDeclaration(self: *Parser) AstError!*Node {
        _ = self.expect(.Pub) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'pub' keyword");
            return err;
        };
        self.skipWhitespace();

        const token = self.current() orelse {
            self.addError(AstError.UnexpectedEof, "Expected declaration after 'pub'");
            return AstError.UnexpectedEof;
        };
        return switch (token.kind) {
            .Fn => self.parseFunction(true),
            .Enum => self.parseEnum(true),
            .Comptime => self.parseComptimeDeclaration(true),
            .Identifier => self.parseDeclaration(true),
            else => {
                const message = std.fmt.allocPrint(self.allocator, "Expected function, enum, or declaration after 'pub' but found '{s}'", .{@tagName(token.kind)}) catch "Expected declaration after 'pub'";
                self.addError(AstError.UnexpectedToken, message);
                // Free the message since addError makes its own copy
                if (!std.mem.eql(u8, message, "Expected declaration after 'pub'")) {
                    self.allocator.free(message);
                }
                return AstError.UnexpectedToken;
            },
        };
    }

    fn parseIdentifierStatement(self: *Parser) AstError!*Node {
        // Look ahead to determine if this is a declaration or assignment
        if (self.peek(1)) |next| {
            switch (next.kind) {
                .ColonColon, .ColonEquals => return self.parseDeclaration(false),
                .Colon => {
                    // For typed declarations, we need to scan ahead to find the declaration operator
                    var look_ahead: usize = 2;
                    while (self.peek(look_ahead)) |ahead_token| {
                        switch (ahead_token.kind) {
                            .ColonColon, .ColonEquals => return self.parseDeclaration(false),
                            .Colon, .Equals => return self.parseDeclaration(false), // Handle : TYPE : VALUE and : TYPE = VALUE
                            .Identifier, .U8, .U16, .U32, .U64, .I8, .I16, .I32, .I64, .F32, .F64, .ISize, .USize, .String, .Bool, .Void => {
                                // Continue scanning - this could be part of type name
                                look_ahead += 1;
                            },
                            else => break, // Not a declaration
                        }
                        if (look_ahead > 10) break; // Reasonable limit to avoid infinite loops
                    }
                    return self.parseExpression();
                },
                .Equals => return self.parseAssignment(),
                .PlusEquals, .MinusEquals, .AsteriskEquals, .SlashEquals, .PercentEquals => return self.parseAssignment(),
                .Dot => {
                    // Scan ahead through member accesses to see if this leads to an assignment
                    var look_ahead: usize = 1;
                    while (self.peek(look_ahead)) |ahead_token| {
                        switch (ahead_token.kind) {
                            .Dot => {
                                look_ahead += 1;
                                // Expect identifier after dot
                                if (self.peek(look_ahead)) |id_token| {
                                    if (id_token.kind == .Identifier) {
                                        look_ahead += 1;
                                    } else {
                                        break; // Not a valid member access
                                    }
                                } else {
                                    break; // End of tokens
                                }
                            },
                            .Identifier => {
                                look_ahead += 1;
                            },
                            .Equals, .PlusEquals, .MinusEquals, .AsteriskEquals, .SlashEquals, .PercentEquals => {
                                // Found assignment after member access
                                return self.parseAssignment();
                            },
                            else => break, // Not member access or assignment
                        }
                        if (look_ahead > 10) break; // Reasonable limit
                    }
                    return self.parseExpression();
                },
                else => return self.parseExpression(),
            }
        }
        return self.parseExpression();
    }

    // ========================================================================
    // Declaration Parsing
    // ========================================================================

    fn parseDeclaration(self: *Parser, is_public: bool) AstError!*Node {
        self.skipWhitespace();

        const name_token = self.expect(.Identifier) catch |err| {
            self.addError(AstError.ExpectedIdentifier, "Expected identifier for declaration name");
            return err;
        };
        const name = name_token.value.?;

        self.skipWhitespace();

        // Optional type annotation
        var type_hint: ?*Node = null;
        if (self.match(.Colon)) {
            self.skipWhitespace();
            type_hint = self.parseTypeExpression() catch |err| {
                self.addError(AstError.ExpectedExpression, "Expected type expression after ':'");
                return err;
            };
            self.skipWhitespace();
        }

        // Declaration operator
        const decl_token = self.current() orelse {
            self.addError(AstError.UnexpectedEof, "Expected declaration operator (::, :=, :, or =)");
            return AstError.UnexpectedEof;
        };
        const kind = switch (decl_token.kind) {
            .ColonColon => DeclKind.immutable, // NAME :: VALUE
            .ColonEquals => DeclKind.mutable, // NAME := VALUE
            .Colon => DeclKind.immutable, // NAME : TYPE : VALUE (const with type)
            .Equals => DeclKind.mutable, // NAME : TYPE = VALUE (reassignable with type)
            else => {
                const message = std.fmt.allocPrint(self.allocator, "Expected declaration operator but found '{s}'", .{@tagName(decl_token.kind)}) catch "Expected declaration operator";
                self.addError(AstError.UnexpectedToken, message);
                // Free the message since addError makes its own copy
                if (!std.mem.eql(u8, message, "Expected declaration operator")) {
                    self.allocator.free(message);
                }
                return AstError.UnexpectedToken;
            },
        };
        self.advance();

        self.skipWhitespace();

        // Check if this is a function declaration (NAME :: fn)
        if (self.current()) |token| {
            if (token.kind == .Fn) {
                const func = self.parseFunctionDeclaration(name, is_public) catch |err| {
                    self.addError(AstError.ExpectedExpression, "Failed to parse function declaration");
                    return err;
                };
                return func;
            }
            // Check if this is an anonymous struct declaration (NAME :: { ... })
            if (token.kind == .CurlyOpen) {
                const struct_def = self.parseAnonymousStructDefinition(name, is_public) catch |err| {
                    self.addError(AstError.ExpectedExpression, "Failed to parse anonymous struct declaration");
                    return err;
                };
                return struct_def;
            }
        }

        const value = self.parseExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected expression after declaration operator");
            return err;
        };

        // Check if the value is a struct definition
        if (value.* == .struct_def) {
            // Update the existing struct definition with proper name and visibility
            value.struct_def.name = name;
            value.struct_def.is_public = is_public;

            return value; // Return the existing node instead of creating a new one
        }

        return self.create(Node, .{ .declaration = Declaration{
            .kind = kind,
            .name = name,
            .type_hint = type_hint,
            .value = value,
            .is_public = is_public,
        } });
    }

    fn parseComptimeDeclaration(self: *Parser, is_public: bool) AstError!*Node {
        _ = self.expect(.Comptime) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'comptime' keyword");
            return err;
        };
        self.skipWhitespace();

        const name_token = self.expect(.Identifier) catch |err| {
            self.addError(AstError.ExpectedIdentifier, "Expected identifier for comptime declaration");
            return err;
        };
        const name = name_token.value.?;

        self.skipWhitespace();

        // Optional type annotation
        var type_hint: ?*Node = null;
        if (self.match(.Colon)) {
            self.skipWhitespace();
            type_hint = self.parseTypeExpression() catch |err| {
                self.addError(AstError.ExpectedExpression, "Expected type expression after ':'");
                return err;
            };
            self.skipWhitespace();
        }

        // Must be :: for comptime
        _ = self.expect(.ColonColon) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected '::' for comptime declaration");
            return err;
        };
        self.skipWhitespace();

        const value = self.parseExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected expression after '::'");
            return err;
        };

        return self.create(Node, .{ .declaration = Declaration{
            .kind = .@"comptime",
            .name = name,
            .type_hint = type_hint,
            .value = value,
            .is_public = is_public,
        } });
    }

    fn parseFunction(self: *Parser, is_public: bool) AstError!*Node {
        _ = self.expect(.Fn) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'fn' keyword");
            return err;
        };
        self.skipWhitespace();

        const name_token = self.expect(.Identifier) catch |err| {
            self.addError(AstError.ExpectedIdentifier, "Expected function name");
            return err;
        };
        const name = name_token.value.?;

        self.skipWhitespace();
        _ = self.expect(.ParenOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening parenthesis for function parameters");
            return err;
        };

        // Parse parameters
        var params = std.ArrayList(Parameter).init(self.allocator);
        defer params.deinit();

        self.skipWhitespace();
        if (!self.match(.ParenClose)) {
            while (true) {
                self.skipWhitespace();
                const param_name = self.expect(.Identifier) catch |err| {
                    self.addError(AstError.ExpectedIdentifier, "Expected parameter name");
                    return err;
                };

                self.skipWhitespace();
                var param_type: ?*Node = null;
                if (self.match(.Colon)) {
                    self.skipWhitespace();
                    param_type = self.parseTypeExpression() catch |err| {
                        self.addError(AstError.ExpectedExpression, "Expected type annotation for parameter");
                        return err;
                    };
                }

                try params.append(Parameter{
                    .name = param_name.value.?,
                    .type_hint = param_type,
                    .default_value = null,
                });

                self.skipWhitespace();
                if (self.match(.ParenClose)) break;
                _ = self.expect(.Comma) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected comma between parameters");
                    return err;
                };
            }
        }

        self.skipWhitespace();

        // Optional return type
        var return_type: ?*Node = null;
        if (self.current()) |token| {
            if (token.kind != .CurlyOpen) {
                return_type = self.parseTypeExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected return type or function body");
                    return err;
                };
                self.skipWhitespace();
            }
        }

        // Function body
        const body = self.parseBlock() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected function body");
            return err;
        };

        return self.create(Node, .{ .function = Function{
            .name = name,
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .body = body,
            .is_public = is_public,
        } });
    }

    fn parseFunctionDeclaration(self: *Parser, func_name: []const u8, is_public: bool) AstError!*Node {
        _ = self.expect(.Fn) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'fn' keyword");
            return err;
        };
        self.skipWhitespace();

        _ = self.expect(.ParenOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening parenthesis for function parameters");
            return err;
        };

        // Parse parameters
        var params = std.ArrayList(Parameter).init(self.allocator);
        defer params.deinit();

        self.skipWhitespace();
        if (!self.match(.ParenClose)) {
            while (true) {
                self.skipWhitespace();
                const param_name = self.expect(.Identifier) catch |err| {
                    self.addError(AstError.ExpectedIdentifier, "Expected parameter name");
                    return err;
                };

                self.skipWhitespace();
                var param_type: ?*Node = null;
                if (self.match(.Colon)) {
                    self.skipWhitespace();
                    param_type = self.parseTypeExpression() catch |err| {
                        self.addError(AstError.ExpectedExpression, "Expected type annotation for parameter");
                        return err;
                    };
                }

                try params.append(Parameter{
                    .name = param_name.value.?,
                    .type_hint = param_type,
                    .default_value = null,
                });

                self.skipWhitespace();
                if (self.match(.ParenClose)) break;
                _ = self.expect(.Comma) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected comma between parameters");
                    return err;
                };
            }
        }

        self.skipWhitespace();

        // Optional return type
        var return_type: ?*Node = null;
        if (self.current()) |token| {
            if (token.kind != .CurlyOpen) {
                return_type = self.parseTypeExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected return type or function body");
                    return err;
                };
                self.skipWhitespace();
            }
        }

        // Parse function body
        const body = self.parseBlock() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected function body");
            return err;
        };

        return self.create(Node, .{ .function = Function{
            .name = func_name,
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .body = body,
            .is_public = is_public,
        } });
    }

    fn parseFunctionBody(self: *Parser, func_name: []const u8, is_public: bool) AstError!*Node {
        // Parse function parameters, return type, and body (fn keyword already consumed)
        self.skipWhitespace();

        _ = self.expect(.ParenOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening parenthesis for function parameters");
            return err;
        };

        // Parse parameters
        var params = std.ArrayList(Parameter).init(self.allocator);
        defer params.deinit();

        self.skipWhitespace();
        if (!self.match(.ParenClose)) {
            while (true) {
                self.skipWhitespace();
                const param_name = self.expect(.Identifier) catch |err| {
                    self.addError(AstError.ExpectedIdentifier, "Expected parameter name");
                    return err;
                };

                self.skipWhitespace();
                var param_type: ?*Node = null;
                if (self.match(.Colon)) {
                    self.skipWhitespace();
                    param_type = self.parseTypeExpression() catch |err| {
                        self.addError(AstError.ExpectedExpression, "Expected type annotation for parameter");
                        return err;
                    };
                }

                try params.append(Parameter{
                    .name = param_name.value.?,
                    .type_hint = param_type,
                    .default_value = null,
                });

                self.skipWhitespace();
                if (self.match(.ParenClose)) break;
                _ = self.expect(.Comma) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected comma between parameters");
                    return err;
                };
            }
        }

        self.skipWhitespace();

        // Optional return type
        var return_type: ?*Node = null;
        if (self.current()) |token| {
            if (token.kind != .CurlyOpen) {
                return_type = self.parseTypeExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected return type or function body");
                    return err;
                };
                self.skipWhitespace();
            }
        }

        // Parse function body
        const body = self.parseBlock() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected function body");
            return err;
        };

        return self.create(Node, .{ .function = Function{
            .name = func_name,
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .body = body,
            .is_public = is_public,
        } });
    }

    fn parseStructDefinition(self: *Parser) AstError!*Node {
        _ = self.expect(.Struct) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'struct' keyword");
            return err;
        };
        self.skipWhitespace();

        _ = self.expect(.CurlyOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening brace for struct body");
            return err;
        };

        var fields = std.ArrayList(Field).init(self.allocator);
        errdefer fields.deinit();

        var methods = std.ArrayList(*Node).init(self.allocator);
        errdefer methods.deinit();

        self.skipWhitespace();
        while (!self.match(.CurlyClose)) {
            self.skipWhitespace();

            // First, get the identifier (either field name or method name)
            const name_token = self.expect(.Identifier) catch |err| {
                self.addError(AstError.ExpectedIdentifier, "Expected field or method name");
                return err;
            };
            self.skipWhitespace();

            // Look ahead to see if this is a method declaration (:: fn) or field (:)
            if (self.current()) |token| {
                if (token.kind == .ColonColon) {
                    // This is a method declaration: name :: fn
                    self.advance(); // consume ::
                    self.skipWhitespace();

                    // Expect fn keyword
                    _ = self.expect(.Fn) catch |err| {
                        self.addError(AstError.UnexpectedToken, "Expected 'fn' after '::' in method declaration");
                        return err;
                    };

                    // Parse the method - don't call parseFunctionDeclaration since we already consumed 'fn'
                    const method = self.parseFunctionBody(name_token.value.?, false) catch |err| {
                        self.addError(AstError.ExpectedExpression, "Failed to parse struct method");
                        return err;
                    };
                    try methods.append(method);
                    self.skipWhitespace();
                    // Check for optional comma after method
                    _ = self.match(.Comma);
                    continue;
                } else if (token.kind == .Colon) {
                    // This is a field declaration: name : type
                    self.advance(); // consume :
                    self.skipWhitespace();

                    const field_type = self.parseTypeExpression() catch |err| {
                        self.addError(AstError.ExpectedExpression, "Expected field type");
                        return err;
                    };

                    try fields.append(Field{
                        .name = name_token.value.?,
                        .type_hint = field_type,
                        .default_value = null,
                    });
                } else {
                    self.addError(AstError.UnexpectedToken, "Expected ':' for field or '::' for method after identifier");
                    return AstError.UnexpectedToken;
                }
            } else {
                self.addError(AstError.UnexpectedEof, "Unexpected end of input in struct definition");
                return AstError.UnexpectedEof;
            }

            self.skipWhitespace();

            // Handle optional comma - if there's no comma, we expect closing brace
            if (self.match(.Comma)) {
                self.skipWhitespace();
                // After comma, check if we have closing brace (trailing comma case)
                if (self.current()) |next_token| {
                    if (next_token.kind == .CurlyClose) {
                        self.advance(); // Consume closing brace
                        break;
                    }
                }
                // Continue to next field/method
            } else {
                // No comma, expect closing brace
                _ = self.expect(.CurlyClose) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected closing brace or comma after field/method");
                    return err;
                };
                break;
            }
        }

        // Return an anonymous struct definition that can be used in declarations
        return self.create(Node, .{
            .struct_def = StructDef{
                .name = "", // No name - this will be set by the declaration context
                .fields = try fields.toOwnedSlice(),
                .methods = try methods.toOwnedSlice(),
                .is_public = false, // This will be set by the declaration context
            },
        });
    }

    fn parseAnonymousStructDefinition(self: *Parser, struct_name: []const u8, is_public: bool) AstError!*Node {
        _ = self.expect(.CurlyOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening brace for struct body");
            return err;
        };

        var fields = std.ArrayList(Field).init(self.allocator);
        errdefer fields.deinit(); // Clean up on error
        var methods = std.ArrayList(*Node).init(self.allocator);
        errdefer methods.deinit(); // Clean up on error

        self.skipWhitespace();
        while (!self.match(.CurlyClose)) {
            self.skipWhitespace();

            // Parse field or method declaration
            const name_token = self.expect(.Identifier) catch |err| {
                self.addError(AstError.ExpectedIdentifier, "Expected field or method name");
                return err;
            };
            const item_name = name_token.value.?;

            self.skipWhitespace();

            if (self.match(.Colon)) {
                // Field: name: type
                self.skipWhitespace();
                const field_type = self.parseTypeExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected field type");
                    return err;
                };

                try fields.append(Field{
                    .name = item_name,
                    .type_hint = field_type,
                    .default_value = null,
                });

                self.skipWhitespace();
                // Handle optional comma
                _ = self.match(.Comma);
            } else if (self.match(.ColonColon)) {
                // Method: name :: fn
                self.skipWhitespace();
                if (self.current()) |token| {
                    if (token.kind == .Fn) {
                        // Consume the 'fn' token first
                        _ = self.advance();
                        // Parse the method using parseFunctionBody since we already consumed 'fn'
                        const method = self.parseFunctionBody(item_name, false) catch |err| {
                            self.addError(AstError.ExpectedExpression, "Failed to parse struct method");
                            return err;
                        };
                        try methods.append(method);
                        self.skipWhitespace();
                        // Check for optional comma after method
                        _ = self.match(.Comma);
                    } else {
                        self.addError(AstError.UnexpectedToken, "Expected 'fn' after '::' in struct method declaration");
                        return AstError.UnexpectedToken;
                    }
                } else {
                    self.addError(AstError.UnexpectedEof, "Expected 'fn' after '::' in struct method declaration");
                    return AstError.UnexpectedEof;
                }
            } else {
                self.addError(AstError.UnexpectedToken, "Expected ':' for field or '::' for method");
                return AstError.UnexpectedToken;
            }

            self.skipWhitespace();
        }

        return self.create(Node, .{
            .struct_def = StructDef{
                .name = struct_name,
                .fields = try fields.toOwnedSlice(),
                .methods = try methods.toOwnedSlice(),
                .is_public = is_public,
            },
        });
    }

    fn parseEnum(self: *Parser, is_public: bool) AstError!*Node {
        _ = self.expect(.Enum) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'enum' keyword");
            return err;
        };
        self.skipWhitespace();

        const name_token = self.expect(.Identifier) catch |err| {
            self.addError(AstError.ExpectedIdentifier, "Expected enum name");
            return err;
        };
        const name = name_token.value.?;

        self.skipWhitespace();
        _ = self.expect(.CurlyOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening brace for enum body");
            return err;
        };

        var variants = std.ArrayList(Variant).init(self.allocator);
        defer variants.deinit();

        self.skipWhitespace();
        while (!self.match(.CurlyClose)) {
            self.skipWhitespace();

            const variant_name = self.expect(.Identifier) catch |err| {
                self.addError(AstError.ExpectedIdentifier, "Expected variant name");
                return err;
            };

            var value: ?*Node = null;
            var associated_type: ?*Node = null;

            self.skipWhitespace();
            if (self.match(.Equals)) {
                self.skipWhitespace();
                value = self.parseExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected value expression after '='");
                    return err;
                };
            } else if (self.match(.ParenOpen)) {
                self.skipWhitespace();
                associated_type = self.parseTypeExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected associated type");
                    return err;
                };
                self.skipWhitespace();
                _ = self.expect(.ParenClose) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected closing parenthesis after associated type");
                    return err;
                };
            }

            try variants.append(Variant{
                .name = variant_name.value.?,
                .value = value,
                .associated_type = associated_type,
            });

            self.skipWhitespace();
            if (!self.match(.Comma)) {
                self.skipWhitespace();
                _ = self.expect(.CurlyClose) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected closing brace or comma after variant");
                    return err;
                };
                break;
            }
        }

        return self.create(Node, .{ .enum_def = EnumDef{
            .name = name,
            .variants = try variants.toOwnedSlice(),
            .is_public = is_public,
        } });
    }

    // ========================================================================
    // Expression Parsing
    // ========================================================================

    fn parseExpression(self: *Parser) AstError!*Node {
        return self.parseBinaryExpression(0) catch |err| {
            self.addError(AstError.ExpectedExpression, "Failed to parse expression");
            return err;
        };
    }

    fn parseBinaryExpression(self: *Parser, min_precedence: u8) AstError!*Node {
        var left = self.parseUnaryExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected left operand in binary expression");
            return err;
        };

        while (true) {
            self.skipWhitespace();
            const token = self.current() orelse break;

            const op = BinaryOp.fromToken(token.kind) orelse break;
            const precedence = op.precedence();
            if (precedence < min_precedence) break;

            self.advance();
            self.skipWhitespace();

            const right = self.parseBinaryExpression(precedence + 1) catch |err| {
                self.addError(AstError.ExpectedExpression, "Expected right operand in binary expression");
                return err;
            };

            const binary_node = self.create(Node, .{ .binary = BinaryExpr{
                .op = op,
                .left = left,
                .right = right,
            } }) catch |err| {
                return err;
            };
            left = binary_node;
        }

        return left;
    }

    fn parseUnaryExpression(self: *Parser) AstError!*Node {
        self.skipWhitespace();
        const token = self.current() orelse {
            self.addError(AstError.UnexpectedEof, "Expected expression after unary operator");
            return AstError.UnexpectedEof;
        };

        if (UnaryOp.fromToken(token.kind)) |op| {
            self.advance();
            const operand = self.parseUnaryExpression() catch |err| {
                self.addError(AstError.ExpectedExpression, "Expected operand for unary expression");
                return err;
            };
            return self.create(Node, .{ .unary = UnaryExpr{
                .op = op,
                .operand = operand,
            } });
        }

        return self.parsePostfixExpression();
    }

    fn parsePostfixExpression(self: *Parser) AstError!*Node {
        var expr = self.parsePrimaryExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected primary expression in postfix");
            return err;
        };

        while (true) {
            self.skipWhitespace();
            const token = self.current() orelse break;

            switch (token.kind) {
                .Dot => {
                    self.advance();
                    self.skipWhitespace();
                    const prop_token = self.expect(.Identifier) catch |err| {
                        self.addError(AstError.ExpectedIdentifier, "Expected property name after '.'");
                        return err;
                    };
                    expr = self.create(Node, .{ .member = MemberExpr{
                        .object = expr,
                        .property = prop_token.value.?,
                    } }) catch |err| {
                        return err;
                    };
                },
                .BracketOpen => {
                    self.advance();
                    self.skipWhitespace();
                    const index = self.parseExpression() catch |err| {
                        self.addError(AstError.ExpectedExpression, "Expected index expression");
                        return err;
                    };
                    self.skipWhitespace();
                    _ = self.expect(.BracketClose) catch |err| {
                        self.addError(AstError.UnexpectedToken, "Expected closing bracket after index");
                        return err;
                    };
                    expr = self.create(Node, .{ .index = IndexExpr{
                        .object = expr,
                        .index = index,
                    } }) catch |err| {
                        return err;
                    };
                },
                .ParenOpen => {
                    self.advance();
                    self.skipWhitespace();

                    var args = std.ArrayList(*Node).init(self.allocator);
                    defer args.deinit();

                    if (!self.match(.ParenClose)) {
                        while (true) {
                            self.skipWhitespace();
                            const arg = self.parseExpression() catch |err| {
                                self.addError(AstError.ExpectedExpression, "Expected argument in function call");
                                return err;
                            };
                            try args.append(arg);

                            self.skipWhitespace();
                            if (self.match(.ParenClose)) break;
                            _ = self.expect(.Comma) catch |err| {
                                self.addError(AstError.UnexpectedToken, "Expected comma between arguments");
                                return err;
                            };
                        }
                    }

                    expr = self.create(Node, .{ .call = CallExpr{
                        .callee = expr,
                        .args = try args.toOwnedSlice(),
                    } }) catch |err| {
                        return err;
                    };
                },
                else => break,
            }
        }

        return expr;
    }

    fn parsePrimaryExpression(self: *Parser) AstError!*Node {
        self.skipWhitespace();
        const token = self.current() orelse {
            self.addError(AstError.UnexpectedEof, "Unexpected end of input while parsing expression");
            return AstError.UnexpectedEof;
        };

        switch (token.kind) {
            .NumberLiteral => {
                self.advance();
                const value_i128 = token.getAsIntValue() catch {
                    self.addError(AstError.InvalidSyntax, "Invalid number literal");
                    return AstError.InvalidSyntax;
                };
                // Convert to i64, clamping if necessary
                const value = @as(i64, @intCast(@min(@max(value_i128, std.math.minInt(i64)), std.math.maxInt(i64))));
                return self.create(Node, .{ .int = value });
            },
            .FloatLiteral => {
                self.advance();
                const value_f128 = token.getAsFloatValue() catch {
                    self.addError(AstError.InvalidSyntax, "Invalid float literal");
                    return AstError.InvalidSyntax;
                };
                // Convert to f64
                const value = @as(f64, @floatCast(value_f128));
                return self.create(Node, .{ .float = value });
            },
            .StringLiteral => {
                self.advance();
                return self.create(Node, .{ .string = token.value.? });
            },
            .CharLiteral => {
                self.advance();
                const char_str = token.value.?;
                const char_val = if (char_str.len > 0) char_str[0] else 0;
                return self.create(Node, .{ .char = char_val });
            },
            .True => {
                self.advance();
                return self.create(Node, .{ .bool = true });
            },
            .False => {
                self.advance();
                return self.create(Node, .{ .bool = false });
            },
            .Identifier => {
                self.advance();
                return self.create(Node, .{ .identifier = token.value.? });
            },
            .ParenOpen => {
                self.advance();
                self.skipWhitespace();
                const expr = self.parseExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected expression inside parentheses");
                    return err;
                };
                self.skipWhitespace();
                _ = self.expect(.ParenClose) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected closing parenthesis");
                    return err;
                };
                return expr;
            },
            .CurlyOpen => return self.parseStructInit(),
            .BracketOpen => return self.parseArray(),
            .Import => return self.parseImport(),
            .If => return self.parseIf(),
            .Match => return self.parseMatch(),
            .Struct => return self.parseStructDefinition(),
            else => {
                const message = std.fmt.allocPrint(self.allocator, "Unexpected token in expression: '{s}'", .{@tagName(token.kind)}) catch "Unexpected token in expression";
                self.addError(AstError.ExpectedExpression, message);
                // Free the message since addError makes its own copy
                if (!std.mem.eql(u8, message, "Unexpected token in expression")) {
                    self.allocator.free(message);
                }
                return AstError.ExpectedExpression;
            },
        }
    }

    // ========================================================================
    // Control Flow Parsing
    // ========================================================================

    fn parseIf(self: *Parser) AstError!*Node {
        _ = self.expect(.If) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'if' keyword");
            return err;
        };
        self.skipWhitespace();

        const condition = self.parseExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected condition after 'if'");
            return err;
        };
        self.skipWhitespace();

        const then_branch = self.parseBlock() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected then block after if condition");
            return err;
        };
        self.skipWhitespace();

        var else_branch: ?*Node = null;
        if (self.match(.Else)) {
            self.skipWhitespace();
            else_branch = if (self.current() != null and self.current().?.kind == .If)
                self.parseIf() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Failed to parse else-if statement");
                    return err;
                }
            else
                self.parseBlock() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected else block");
                    return err;
                };
        }

        return self.create(Node, .{ .if_expr = IfExpr{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        } });
    }

    fn parseMatch(self: *Parser) AstError!*Node {
        _ = self.expect(.Match) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'match' keyword");
            return err;
        };
        self.skipWhitespace();

        const expr = self.parseExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected expression after 'match'");
            return err;
        };
        self.skipWhitespace();

        _ = self.expect(.CurlyOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening brace for match body");
            return err;
        };

        var arms = std.ArrayList(MatchArm).init(self.allocator);
        defer arms.deinit();

        self.skipWhitespace();
        while (!self.match(.CurlyClose)) {
            self.skipWhitespace();
            _ = self.expect(.Pipe) catch |err| {
                self.addError(AstError.UnexpectedToken, "Expected '|' before match pattern");
                return err;
            };
            self.skipWhitespace();

            const pattern = self.parseExpression() catch |err| {
                self.addError(AstError.ExpectedExpression, "Expected pattern in match arm");
                return err;
            };
            self.skipWhitespace();

            var guard: ?*Node = null;
            if (self.match(.If)) {
                self.skipWhitespace();
                guard = self.parseExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected guard expression after 'if'");
                    return err;
                };
                self.skipWhitespace();
            }

            _ = self.expect(.EqualsGreaterThan) catch |err| {
                self.addError(AstError.UnexpectedToken, "Expected '=>' after match pattern");
                return err;
            };
            self.skipWhitespace();

            const body = self.parseExpression() catch |err| {
                self.addError(AstError.ExpectedExpression, "Expected body expression in match arm");
                return err;
            };

            try arms.append(MatchArm{
                .pattern = pattern,
                .guard = guard,
                .body = body,
            });

            self.skipWhitespace();
            if (!self.match(.Comma)) {
                self.skipWhitespace();
                _ = self.expect(.CurlyClose) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected closing brace or comma after match arm");
                    return err;
                };
                break;
            }
        }

        return self.create(Node, .{ .match_expr = MatchExpr{
            .expr = expr,
            .arms = try arms.toOwnedSlice(),
        } });
    }

    fn parseFor(self: *Parser) AstError!*Node {
        _ = self.expect(.For) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'for' keyword");
            return err;
        };
        self.skipWhitespace();

        var variable: ?[]const u8 = null;
        if (self.current()) |token| {
            if (token.kind == .Identifier) {
                variable = token.value;
                self.advance();
                self.skipWhitespace();
                // Expect 'in' keyword (assuming it's an identifier for now)
                _ = self.expect(.Identifier) catch |err| {
                    self.addError(AstError.ExpectedIdentifier, "Expected 'in' keyword after loop variable");
                    return err;
                }; // 'in'
                self.skipWhitespace();
            }
        }

        const iterable = self.parseExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected iterable expression in for loop");
            return err;
        };
        self.skipWhitespace();

        const body = self.parseBlock() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected loop body");
            return err;
        };

        return self.create(Node, .{ .for_loop = ForLoop{
            .variable = variable,
            .iterable = iterable,
            .body = body,
        } });
    }

    fn parseWhile(self: *Parser) AstError!*Node {
        _ = self.expect(.While) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'while' keyword");
            return err;
        };
        self.skipWhitespace();

        const condition = self.parseExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected condition after 'while'");
            return err;
        };
        self.skipWhitespace();

        const body = self.parseBlock() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected loop body");
            return err;
        };

        return self.create(Node, .{ .while_loop = WhileLoop{
            .condition = condition,
            .body = body,
        } });
    }

    fn parseReturn(self: *Parser) AstError!*Node {
        _ = self.expect(.Return) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'return' keyword");
            return err;
        };
        self.skipWhitespace();

        // Check if there's an expression to return
        if (self.current()) |token| {
            switch (token.kind) {
                .CurlyClose, .EOF, .Newline => {
                    return self.create(Node, .{ .return_stmt = null });
                },
                else => {
                    const value = self.parseExpression() catch |err| {
                        self.addError(AstError.ExpectedExpression, "Expected expression after 'return'");
                        return err;
                    };
                    return self.create(Node, .{ .return_stmt = value });
                },
            }
        }

        return self.create(Node, .{ .return_stmt = null });
    }

    fn parseAssignment(self: *Parser) AstError!*Node {
        const target = self.parsePostfixExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected assignment target");
            return err;
        };
        self.skipWhitespace();

        const op_token = self.current() orelse {
            self.addError(AstError.UnexpectedToken, "Expected assignment operator");
            return AstError.UnexpectedToken;
        };
        const is_compound = switch (op_token.kind) {
            .PlusEquals, .MinusEquals, .AsteriskEquals, .SlashEquals, .PercentEquals => true,
            .Equals => false,
            else => {
                self.addError(AstError.UnexpectedToken, "Expected assignment operator");
                return AstError.UnexpectedToken;
            },
        };

        self.advance(); // consume assignment operator
        self.skipWhitespace();

        const rhs = self.parseExpression() catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected value expression in assignment");
            return err;
        };

        if (is_compound) {
            // Convert compound assignment to regular assignment with binary operation
            // e.g., x += 5 becomes x = x + 5
            const binary_op: BinaryOp = switch (op_token.kind) {
                .PlusEquals => .add,
                .MinusEquals => .sub,
                .AsteriskEquals => .mul,
                .SlashEquals => .div,
                .PercentEquals => .mod,
                else => unreachable,
            };

            const binary_expr = self.create(Node, .{ .binary = BinaryExpr{
                .op = binary_op,
                .left = target,
                .right = rhs,
            } }) catch |err| {
                self.addError(AstError.OutOfMemory, "Failed to create binary expression");
                return err;
            };

            return self.create(Node, .{ .assignment = Assignment{
                .target = target,
                .value = binary_expr,
            } });
        } else {
            return self.create(Node, .{ .assignment = Assignment{
                .target = target,
                .value = rhs,
            } });
        }
    }

    // ========================================================================
    // Compound Expression Parsing
    // ========================================================================

    fn parseBlock(self: *Parser) AstError!*Node {
        _ = self.expect(.CurlyOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening brace for block");
            return err;
        };

        var statements = std.ArrayList(*Node).init(self.allocator);
        defer statements.deinit();

        self.skipWhitespace();
        while (!self.match(.CurlyClose)) {
            self.skipWhitespace();
            const stmt = self.parseStatement() catch |err| {
                self.addError(AstError.ExpectedExpression, "Failed to parse statement in block");
                return err;
            };
            try statements.append(stmt);
            self.skipWhitespace();
        }

        return self.create(Node, .{ .block = Block{
            .statements = try statements.toOwnedSlice(),
        } });
    }

    fn parseStructInit(self: *Parser) AstError!*Node {
        _ = self.expect(.CurlyOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening brace for struct initialization");
            return err;
        };

        var fields = std.ArrayList(FieldInit).init(self.allocator);
        defer fields.deinit();

        self.skipWhitespace();
        if (!self.match(.CurlyClose)) {
            while (true) {
                self.skipWhitespace();
                const field_name = self.expect(.Identifier) catch |err| {
                    self.addError(AstError.ExpectedIdentifier, "Expected field name in struct initialization");
                    return err;
                };
                self.skipWhitespace();
                _ = self.expect(.Colon) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected colon after field name");
                    return err;
                };
                self.skipWhitespace();
                const value = self.parseExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected field value in struct initialization");
                    return err;
                };

                try fields.append(FieldInit{
                    .name = field_name.value.?,
                    .value = value,
                });

                self.skipWhitespace();
                if (self.match(.CurlyClose)) break;
                _ = self.expect(.Comma) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected comma between struct fields");
                    return err;
                };
            }
        }

        return self.create(Node, .{ .struct_init = StructInit{
            .type_name = null,
            .fields = try fields.toOwnedSlice(),
        } });
    }

    fn parseArray(self: *Parser) AstError!*Node {
        _ = self.expect(.BracketOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening bracket for array");
            return err;
        };

        var elements = std.ArrayList(*Node).init(self.allocator);
        defer elements.deinit();

        self.skipWhitespace();
        if (!self.match(.BracketClose)) {
            while (true) {
                self.skipWhitespace();
                const element = self.parseExpression() catch |err| {
                    self.addError(AstError.ExpectedExpression, "Expected array element");
                    return err;
                };
                try elements.append(element);

                self.skipWhitespace();
                if (self.match(.BracketClose)) break;
                _ = self.expect(.Comma) catch |err| {
                    self.addError(AstError.UnexpectedToken, "Expected comma between array elements");
                    return err;
                };
            }
        }

        return self.create(Node, .{ .array = Array{
            .elements = try elements.toOwnedSlice(),
        } });
    }

    fn parseImport(self: *Parser) AstError!*Node {
        _ = self.expect(.Import) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected 'import' keyword");
            return err;
        };
        self.skipWhitespace();
        _ = self.expect(.ParenOpen) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected opening parenthesis after 'import'");
            return err;
        };
        self.skipWhitespace();
        const path_token = self.expect(.StringLiteral) catch |err| {
            self.addError(AstError.ExpectedExpression, "Expected string literal for import path");
            return err;
        };
        self.skipWhitespace();
        _ = self.expect(.ParenClose) catch |err| {
            self.addError(AstError.UnexpectedToken, "Expected closing parenthesis after import path");
            return err;
        };

        return self.create(Node, .{ .import = path_token.value.? });
    }

    fn parseTypeExpression(self: *Parser) AstError!*Node {
        self.skipWhitespace();
        const token = self.current() orelse {
            self.addError(AstError.UnexpectedEof, "Expected type expression but reached end of input");
            return AstError.UnexpectedEof;
        };

        var is_optional = false;
        var is_pointer = false;

        // Handle optional types (?Type)
        if (token.kind == .Question) {
            is_optional = true;
            self.advance();
            self.skipWhitespace();
        }

        // Handle pointer types (*Type)
        if (self.current()) |t| {
            if (t.kind == .Asterisk) {
                is_pointer = true;
                self.advance();
                self.skipWhitespace();
            }
        }

        const name_token = self.current() orelse {
            self.addError(AstError.ExpectedIdentifier, "Expected type name");
            return AstError.ExpectedIdentifier;
        };

        // Handle built-in type tokens and identifiers
        const name = switch (name_token.kind) {
            .Identifier => name_token.value.?,
            .String => "str",
            .I8 => "i8",
            .I16 => "i16",
            .I32 => "i32",
            .I64 => "i64",
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .F32 => "f32",
            .F64 => "f64",
            .Bool => "bool",
            .Void => "void",
            .ISize => "isize",
            .USize => "usize",
            else => {
                const message = std.fmt.allocPrint(self.allocator, "Expected type name but found '{s}'", .{@tagName(name_token.kind)}) catch "Expected type name";
                self.addError(AstError.ExpectedIdentifier, message);
                // Free the message since addError makes its own copy
                if (!std.mem.eql(u8, message, "Expected type name")) {
                    self.allocator.free(message);
                }
                return AstError.ExpectedIdentifier;
            },
        };
        self.advance();

        // TODO: Handle generic type arguments [T, U]
        var args = std.ArrayList(*Node).init(self.allocator);
        defer args.deinit();

        return self.create(Node, .{ .type_ref = TypeRef{
            .name = name,
            .args = try args.toOwnedSlice(),
            .is_optional = is_optional,
            .is_pointer = is_pointer,
        } });
    }

    // ========================================================================
    // Debug and Utility
    // ========================================================================

    pub fn printAst(node: *Node, indent: usize) void {
        const spaces = "                                                    ";
        const indent_str = spaces[0..@min(indent, spaces.len - 1)];

        switch (node.*) {
            .int => |val| std.debug.print("{s}Int: {d}\n", .{ indent_str, val }),
            .float => |val| std.debug.print("{s}Float: {d}\n", .{ indent_str, val }),
            .string => |val| std.debug.print("{s}String: \"{s}\"\n", .{ indent_str, val }),
            .char => |val| std.debug.print("{s}Char: '{c}'\n", .{ indent_str, val }),
            .bool => |val| std.debug.print("{s}Bool: {}\n", .{ indent_str, val }),
            .identifier => |val| std.debug.print("{s}Identifier: {s}\n", .{ indent_str, val }),

            .binary => |expr| {
                std.debug.print("{s}Binary: {s}\n", .{ indent_str, @tagName(expr.op) });
                printAst(expr.left, indent + 2);
                printAst(expr.right, indent + 2);
            },
            .unary => |expr| {
                std.debug.print("{s}Unary: {s}\n", .{ indent_str, @tagName(expr.op) });
                printAst(expr.operand, indent + 2);
            },
            .call => |expr| {
                std.debug.print("{s}Call:\n", .{indent_str});
                std.debug.print("{s}  Callee:\n", .{indent_str});
                printAst(expr.callee, indent + 4);
                std.debug.print("{s}  Args:\n", .{indent_str});
                for (expr.args) |arg| {
                    printAst(arg, indent + 4);
                }
            },
            .declaration => |decl| {
                std.debug.print("{s}Declaration: {s} {s} (public: {})\n", .{ indent_str, @tagName(decl.kind), decl.name, decl.is_public });
                if (decl.type_hint) |type_hint| {
                    std.debug.print("{s}  Type:\n", .{indent_str});
                    printAst(type_hint, indent + 4);
                }
                std.debug.print("{s}  Value:\n", .{indent_str});
                printAst(decl.value, indent + 4);
            },
            .function => |func| {
                std.debug.print("{s}Function: {s} (public: {})\n", .{ indent_str, func.name, func.is_public });
                std.debug.print("{s}  Params: {d}\n", .{ indent_str, func.params.len });
                if (func.return_type) |ret_type| {
                    std.debug.print("{s}  Return Type:\n", .{indent_str});
                    printAst(ret_type, indent + 4);
                }
                std.debug.print("{s}  Body:\n", .{indent_str});
                printAst(func.body, indent + 4);
            },
            .block => |block| {
                std.debug.print("{s}Block: {} statements\n", .{ indent_str, block.statements.len });
                for (block.statements) |stmt| {
                    printAst(stmt, indent + 2);
                }
            },
            .struct_def => {
                std.debug.print("{s}Struct: {s} (public: {})\n", .{
                    indent_str,
                    node.*.struct_def.name,
                    node.*.struct_def.is_public,
                });
                std.debug.print("{s}  Fields:\n", .{indent_str});
                for (node.*.struct_def.fields) |field| {
                    std.debug.print("{s}    {s}: ", .{ indent_str, field.name });
                    printAst(field.type_hint, indent + 4);
                }
                if (node.*.struct_def.methods.len > 0) {
                    std.debug.print("{s}  Methods:\n", .{indent_str});
                    for (node.*.struct_def.methods) |method| {
                        printAst(method, indent + 4);
                    }
                }
            },
            else => {
                std.debug.print("{s}{s}\n", .{ indent_str, @tagName(node.*) });
            },
        }
    }
};
