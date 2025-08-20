const std = @import("std");
const ast = @import("ast.zig");
const ErrorSystem = @import("error_system.zig");
const Token = @import("token.zig").Token;

// ============================================================================
// Modern Parser with Error Recovery
// ============================================================================

const RecoveryToken = enum {
    semicolon,
    closing_brace,
    closing_paren,
    newline,
    eof,
    let,
    @"fn",
    @"if",
    @"while",
    @"for",
    @"return",
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const Token,
    current: usize,
    arena: *ast.AstArena,
    errors: *ErrorSystem.ErrorCollector,
    source_map: ?*const ErrorSystem.SourceMap,
    file_path: []const u8,
    
    // Error recovery state
    panic_mode: bool,
    in_recovery: bool,
    recovery_tokens: []const RecoveryToken,
    
    // Debug state
    debug_mode: bool,
    
    // Recursion tracking to prevent stack overflow
    recursion_depth: usize,
    max_recursion_depth: usize,
    
    fn debugPrint(self: *Parser, comptime format: []const u8, args: anytype) void {
        if (self.debug_mode) {
            std.debug.print("[PARSER] " ++ format ++ "\n", args);
        }
    }

    fn checkRecursionDepth(self: *Parser) !void {
        if (self.recursion_depth >= self.max_recursion_depth) {
            self.debugPrint("RECURSION LIMIT HIT: depth={}", .{self.recursion_depth});
            try self.reportError(.unexpected_token, "Maximum recursion depth exceeded", self.getCurrentSourceLoc());
            return error.MaxRecursionDepthExceeded;
        }
    }

    fn enterRecursion(self: *Parser) !void {
        // Temporarily disabled for debugging
        // try self.checkRecursionDepth();
        // self.recursion_depth += 1;
        _ = self;
    }

    fn exitRecursion(self: *Parser) void {
        // Temporarily disabled for debugging
        // if (self.recursion_depth > 0) {
        //     self.recursion_depth -= 1;
        // }
        _ = self;
    }

    pub fn init(
        allocator: std.mem.Allocator,
        tokens: []const Token,
        arena: *ast.AstArena,
        errors: *ErrorSystem.ErrorCollector,
        file_path: []const u8,
        source_map: ?*const ErrorSystem.SourceMap,
    ) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .arena = arena,
            .errors = errors,
            .source_map = source_map,
            .file_path = file_path,
            .panic_mode = false,
            .in_recovery = false,
            .recovery_tokens = &[_]RecoveryToken{.semicolon, .closing_brace, .eof},
            .debug_mode = false, // Disable debug mode
            .recursion_depth = 0,
            .max_recursion_depth = 1000, // Reasonable limit
        };
    }

    // ============================================================================
    // Whitespace Enforcement Functions
    // ============================================================================
    
    /// Check if there's required whitespace before the current token
    fn requireWhitespaceBefore(self: *Parser, error_type: ErrorSystem.ErrorCode, message: []const u8) !void {
        if (self.current == 0) return; // At start of file
        
        const prev_token = self.tokens[self.current - 1];
        const curr_token = self.peek();
        
        // Calculate positions to check for whitespace
        const prev_end = prev_token.getEndPos();
        const curr_start = curr_token.getPos();
        
        // If there's no gap between tokens, it's an error
        if (prev_end >= curr_start) {
            try self.reportError(error_type, message, self.getCurrentSourceLoc());
        }
    }
    
    /// Check if there's required whitespace after a token before advancing
    fn requireWhitespaceAfter(self: *Parser, token_tag: std.meta.Tag(Token), error_type: ErrorSystem.ErrorCode, message: []const u8) !void {
        if (!self.check(token_tag)) return;
        
        const token_pos = self.current;
        _ = self.advance(); // Consume the token
        
        // Check if next token immediately follows (no whitespace)
        if (!self.isAtEnd()) {
            const token = self.tokens[token_pos];
            const next_token = self.peek();
            
            const token_end = token.getEndPos();
            const next_start = next_token.getPos();
            
            if (token_end >= next_start) {
                try self.reportError(error_type, message, self.getCurrentSourceLoc());
            }
        }
    }
    
    /// Require space around binary operators
    fn requireSpaceAroundOperator(self: *Parser, operator_tag: std.meta.Tag(Token)) !void {
        if (!self.check(operator_tag)) return;
        
        // Check space before operator
        try self.requireWhitespaceBefore(.missing_space_around_operator, "Missing space before operator");
        
        // Consume operator and check space after
        _ = self.advance();
        if (!self.isAtEnd()) {
            const prev_token = self.previous();
            const next_token = self.peek();
            
            const prev_end = prev_token.getEndPos();
            const next_start = next_token.getPos();
            
            if (prev_end >= next_start) {
                try self.reportError(.missing_space_around_operator, "Missing space after operator", self.getCurrentSourceLoc());
            }
        }
    }
    
    /// Ensure statements are on separate lines by checking for newlines
    fn requireNewlineAfterStatement(self: *Parser) !void {
        // Skip to find newline or end of file
        var lookahead = self.current;
        var found_newline = false;
        
        while (lookahead < self.tokens.len) {
            const token = self.tokens[lookahead];
            if (std.meta.activeTag(token) == .Newline) {
                found_newline = true;
                break;
            } else if (std.meta.activeTag(token) == .Whitespace) {
                lookahead += 1;
                continue;
            } else {
                // Found another non-whitespace token on same line
                break;
            }
        }
        
        // If we find another statement token without a newline, it's an error
        if (lookahead < self.tokens.len and !found_newline) {
            const next_token = self.tokens[lookahead];
            if (self.isStatementStart(next_token)) {
                try self.reportError(.multiple_statements_per_line, "Multiple statements on the same line", self.getCurrentSourceLoc());
            }
        }
    }
    
    /// Check if a token can start a statement
    fn isStatementStart(self: *const Parser, token: Token) bool {
        _ = self; // Mark as intentionally unused
        return switch (std.meta.activeTag(token)) {
            .Identifier, .Pub, .Let, .Mut, .If, .While, .For, .Return, .LeftBrace => true,
            else => false,
        };
    }

    // ============================================================================
    // Error Reporting with Source Location  
    // ============================================================================

    fn getCurrentSourceLoc(self: *const Parser) ast.SourceLoc {
        if (self.current >= self.tokens.len) {
            return ast.SourceLoc{
                .file_path = self.file_path,
                .start_pos = if (self.tokens.len > 0) self.tokens[self.tokens.len - 1].getPos() else 0,
                .end_pos = if (self.tokens.len > 0) self.tokens[self.tokens.len - 1].getPos() else 0,
                .line = 1,
                .column = 1,
            };
        }

        const token = self.tokens[self.current];
        const pos = token.getPos();
        
        // Use source map if available for accurate line/column
        if (self.source_map) |sm| {
            const line_col = sm.getLineColumn(pos);
            return ast.SourceLoc{
                .file_path = self.file_path,
                .start_pos = pos,
                .end_pos = pos,
                .line = line_col.line,
                .column = line_col.column,
            };
        }

        return ast.SourceLoc{
            .file_path = self.file_path,
            .start_pos = pos,
            .end_pos = pos,
            .line = 1,
            .column = pos + 1,
        };
    }

    fn reportError(
        self: *Parser,
        code: ErrorSystem.ErrorCode,
        message: []const u8,
        source_loc: ast.SourceLoc,
    ) !void {
        const err = try self.errors.createAndAddError(
            code,
            .parser,
            .error_,
            message,
            source_loc.toSourceSpan(),
        );

        // Add helpful suggestions based on the error type
        switch (code) {
            .unexpected_token => {
                // Use arena allocator instead of main allocator to avoid leaks
                var arena_allocator = std.heap.ArenaAllocator.init(self.allocator);
                defer arena_allocator.deinit();
                // Skip suggestions to avoid memory leaks for now
                // try err.withSuggestions(arena_allocator.allocator(), &[_][]const u8{
                //     "Check for missing semicolons or parentheses",
                //     "Verify correct syntax for the current statement",
                // });
                _ = err; // Acknowledge the error is used
            },
            .missing_semicolon => {
                // Skip suggestions to avoid memory leaks for now
                // try err.withSuggestions(self.allocator, &[_][]const u8{
                //     "Add a semicolon ';' at the end of the statement",
                // });
                _ = err; // Acknowledge the error is used
            },
            .missing_closing_brace => {
                // Skip suggestions to avoid memory leaks for now
                // try err.withSuggestions(self.allocator, &[_][]const u8{
                //     "Add a closing brace '}' to match the opening brace",
                //     "Check for proper nesting of code blocks",
                // });
                _ = err; // Acknowledge the error is used
            },
            else => {
                _ = err; // Acknowledge the error is used
            },
        }

        self.panic_mode = true;
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;
        self.in_recovery = true;

        while (!self.isAtEnd()) {
            // Look for recovery tokens
            const current_token = self.peek();
            
            if (self.isRecoveryToken(current_token)) {
                self.in_recovery = false;
                return;
            }

            // Skip to the next token
            _ = self.advance();
        }

        self.in_recovery = false;
    }

    fn isRecoveryToken(self: *const Parser, token: Token) bool {
        _ = self; // Mark as intentionally unused
        return switch (token) {
            .Semicolon => true,
            .RightBrace => true,
            .RightParen => true,
            .Let => true,
            .Fn => true,
            .If => true,
            .While => true,
            .For => true,
            .Return => true,
            .EOF => true,
            else => false,
        };
    }

    // ============================================================================
    // Token Management
    // ============================================================================

    fn isAtEnd(self: *const Parser) bool {
        return self.current >= self.tokens.len or self.peek() == .EOF;
    }

    fn peek(self: *const Parser) Token {
        if (self.current >= self.tokens.len) return Token{ .EOF = .{ .pos = 0 } };
        return self.tokens[self.current];
    }

    fn previous(self: *const Parser) Token {
        if (self.current == 0) return Token{ .EOF = .{ .pos = 0 } };
        return self.tokens[self.current - 1];
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn check(self: *const Parser, token_type: std.meta.Tag(Token)) bool {
        if (self.isAtEnd()) return false;
        return std.meta.activeTag(self.peek()) == token_type;
    }

    fn match(self: *Parser, token_types: []const std.meta.Tag(Token)) bool {
        for (token_types) |token_type| {
            if (self.check(token_type)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(
        self: *Parser,
        token_type: std.meta.Tag(Token),
        error_code: ErrorSystem.ErrorCode,
        message: []const u8,
    ) !Token {
        if (self.check(token_type)) {
            return self.advance();
        }

        const source_loc = self.getCurrentSourceLoc();
        try self.reportError(error_code, message, source_loc);
        
        // Create an error node for recovery
        _ = try ast.createErrorNode(self.arena, source_loc, error_code, message);
        
        return Token{ .EOF = .{ .pos = 0 } }; // Return a safe default
    }

    // ============================================================================
    // Expression Parsing with Precedence Climbing
    // ============================================================================

    pub fn parseExpression(self: *Parser) anyerror!ast.NodeId {
        try self.enterRecursion();
        defer self.exitRecursion();
        return self.parseAssignment();
    }

    fn parseAssignment(self: *Parser) !ast.NodeId {
        try self.enterRecursion();
        defer self.exitRecursion();
        
        self.debugPrint("parseAssignment: current={d}, token={}", .{self.current, self.peek()});
        const expr = try self.parseLogicalOr();

        if (self.match(&[_]std.meta.Tag(Token){.Equals, .ColonEquals})) {
            self.debugPrint("Found assignment operator, parsing right side", .{});
            const source_loc = self.getCurrentSourceLoc();
            const value = try self.parseAssignment();
            return ast.createBinaryExpr(self.arena, source_loc, .assign, expr, value);
        }

        return expr;
    }

    fn parseLogicalOr(self: *Parser) !ast.NodeId {
        var expr = try self.parseLogicalAnd();

        while (self.match(&[_]std.meta.Tag(Token){.Or})) {
            const source_loc = self.getCurrentSourceLoc();
            const right = try self.parseLogicalAnd();
            expr = try ast.createBinaryExpr(self.arena, source_loc, .logical_or, expr, right);
        }

        return expr;
    }

    fn parseLogicalAnd(self: *Parser) !ast.NodeId {
        var expr = try self.parseEquality();

        while (self.match(&[_]std.meta.Tag(Token){.And})) {
            const source_loc = self.getCurrentSourceLoc();
            const right = try self.parseEquality();
            expr = try ast.createBinaryExpr(self.arena, source_loc, .logical_and, expr, right);
        }

        return expr;
    }

    fn parseEquality(self: *Parser) !ast.NodeId {
        var expr = try self.parseComparison();

        while (self.match(&[_]std.meta.Tag(Token){.DoubleEquals, .ExclamationEquals})) {
            const source_loc = self.getCurrentSourceLoc();
            const op: ast.BinaryOp = switch (self.previous()) {
                .DoubleEquals => .eq,
                .ExclamationEquals => .ne,
                else => unreachable,
            };
            const right = try self.parseComparison();
            expr = try ast.createBinaryExpr(self.arena, source_loc, op, expr, right);
        }

        return expr;
    }

    fn parseComparison(self: *Parser) !ast.NodeId {
        var expr = try self.parseTerm();

        while (self.match(&[_]std.meta.Tag(Token){.GreaterThan, .GreaterThanEquals, .LessThan, .LessThanEquals})) {
            const source_loc = self.getCurrentSourceLoc();
            const op: ast.BinaryOp = switch (self.previous()) {
                .GreaterThan => .gt,
                .GreaterThanEquals => .ge,
                .LessThan => .lt,
                .LessThanEquals => .le,
                else => unreachable,
            };
            const right = try self.parseTerm();
            expr = try ast.createBinaryExpr(self.arena, source_loc, op, expr, right);
        }

        return expr;
    }

    fn parseTerm(self: *Parser) !ast.NodeId {
        var expr = try self.parseFactor();

        while (true) {
            // Skip whitespace before checking for operators
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            if (self.match(&[_]std.meta.Tag(Token){.Plus, .Minus, .PlusPlus})) {
                const source_loc = self.getCurrentSourceLoc();
                const op: ast.BinaryOp = switch (self.previous()) {
                    .Plus => .add,
                    .Minus => .sub,
                    .PlusPlus => .concat,
                    else => unreachable,
                };
                
                // Skip whitespace after operator
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                const right = try self.parseFactor();
                expr = try ast.createBinaryExpr(self.arena, source_loc, op, expr, right);
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseFactor(self: *Parser) !ast.NodeId {
        var expr = try self.parseUnary();

        while (true) {
            // Skip whitespace before checking for operators
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            if (self.match(&[_]std.meta.Tag(Token){.Asterisk, .Slash, .Percent})) {
                const source_loc = self.getCurrentSourceLoc();
                const op: ast.BinaryOp = switch (self.previous()) {
                    .Asterisk => .mul,
                    .Slash => .div,
                    .Percent => .mod,
                    else => unreachable,
                };
                
                // Skip whitespace after operator
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                const right = try self.parseUnary();
                expr = try ast.createBinaryExpr(self.arena, source_loc, op, expr, right);
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseUnary(self: *Parser) anyerror!ast.NodeId {
        if (self.match(&[_]std.meta.Tag(Token){.Exclamation, .Minus})) {
            const source_loc = self.getCurrentSourceLoc();
            const op: ast.UnaryOp = switch (self.previous()) {
                .Exclamation => .not,
                .Minus => .negate,
                else => unreachable,
            };
            const right = try self.parseUnary();
            return ast.createUnaryExpr(self.arena, source_loc, op, right);
        }

        return self.parseCall();
    }

    fn parseCall(self: *Parser) !ast.NodeId {
        var expr = try self.parsePrimary();

        while (true) {
            if (self.match(&[_]std.meta.Tag(Token){.LeftParen})) {
                expr = try self.finishCall(expr);
            } else if (self.match(&[_]std.meta.Tag(Token){.LeftBracket})) {
                const source_loc = self.getCurrentSourceLoc();
                const index = try self.parseExpression();
                _ = try self.consume(.RightBracket, .unexpected_token, "Expected ']' after array index");
                expr = try self.arena.createNode(ast.AstNode.init(.{
                    .index_expr = .{ .object = expr, .index = index }
                }, source_loc));
            } else if (self.match(&[_]std.meta.Tag(Token){.Dot})) {
                const source_loc = self.getCurrentSourceLoc();
                const name_token = try self.consume(.Identifier, .unexpected_token, "Expected property name after '.'");
                if (name_token == .Identifier) {
                    const field_name = name_token.Identifier.value;
                    expr = try self.arena.createNode(ast.AstNode.init(.{
                        .member_expr = .{ .object = expr, .field = field_name }
                    }, source_loc));
                }
            } else if (self.match(&[_]std.meta.Tag(Token){.LeftBrace})) {
                // Handle struct initialization: MyStruct{ .field1 = value1, .field2 = value2 }
                expr = try self.finishStructInitialization(expr);
            } else {
                // Skip whitespace before checking for catch
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                if (self.match(&[_]std.meta.Tag(Token){.Catch})) {
                    // Handle catch expression: expr catch |err| { ... } or expr catch "default"
                    const source_loc = self.getCurrentSourceLoc();
                    
                    // Skip whitespace after catch keyword
                    while (self.check(.Whitespace)) {
                        _ = self.advance();
                    }
                    
                    var error_capture: ?[]const u8 = null;
                    var catch_body: ?ast.NodeId = null;
                    var fallback_value: ?ast.NodeId = null;
                    
                    // Check for error capture variable |err|
                    if (self.match(&[_]std.meta.Tag(Token){.Pipe})) {
                        const capture_token = try self.consume(.Identifier, .expected_identifier, "Expected identifier in error capture");
                        if (capture_token == .Identifier) {
                            error_capture = capture_token.Identifier.value;
                        }
                        _ = try self.consume(.Pipe, .unexpected_token, "Expected '|' after error capture");
                        
                        // Skip whitespace before catch body
                        while (self.check(.Whitespace)) {
                            _ = self.advance();
                        }
                        
                        // Parse catch body (block or single expression)
                        catch_body = try self.parseExpression();
                    } else {
                        // Direct fallback value: expr catch "default"
                        fallback_value = try self.parseExpression();
                    }
                    
                    expr = try ast.createCatchExpr(self.arena, source_loc, expr, error_capture, catch_body, fallback_value);
                } else {
                    break;
                }
            }
        }

        return expr;
    }

    fn parseCall_withCallee(self: *Parser, callee: ast.NodeId) !ast.NodeId {
        var expr = callee;

        while (true) {
            if (self.match(&[_]std.meta.Tag(Token){.LeftParen})) {
                expr = try self.finishCall(expr);
            } else if (self.match(&[_]std.meta.Tag(Token){.LeftBracket})) {
                const source_loc = self.getCurrentSourceLoc();
                const index = try self.parseExpression();
                _ = try self.consume(.RightBracket, .unexpected_token, "Expected ']' after array index");
                expr = try self.arena.createNode(ast.AstNode.init(.{
                    .index_expr = .{ .object = expr, .index = index }
                }, source_loc));
            } else if (self.match(&[_]std.meta.Tag(Token){.Dot})) {
                const source_loc = self.getCurrentSourceLoc();
                const name_token = try self.consume(.Identifier, .unexpected_token, "Expected property name after '.'");
                if (name_token == .Identifier) {
                    const field_name = name_token.Identifier.value;
                    expr = try self.arena.createNode(ast.AstNode.init(.{
                        .member_expr = .{ .object = expr, .field = field_name }
                    }, source_loc));
                }
            } else {
                break;
            }
        }

        return expr;
    }

    fn finishCall(self: *Parser, callee: ast.NodeId) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        var args = std.ArrayList(ast.NodeId).init(self.allocator);

        if (!self.check(.RightParen)) {
            while (true) {
                if (args.items.len >= 255) {
                    try self.reportError(.wrong_argument_count, "Can't have more than 255 arguments", self.getCurrentSourceLoc());
                }
                
                const arg = try self.parseExpression();
                try args.append(arg);
                
                if (!self.match(&[_]std.meta.Tag(Token){.Comma})) break;
            }
        }

        _ = try self.consume(.RightParen, .missing_closing_paren, "Expected ')' after arguments");

        // Check if this is a generic type instantiation (e.g., List(T))
        // by looking at the callee and checking if it could be a type constructor
        if (try self.isTypeConstructor(callee)) {
            // This is a generic type instantiation like List(T)
            return self.arena.createNode(ast.AstNode.init(.{
                .generic_type_expr = .{ .base_type = callee, .type_params = args }
            }, source_loc));
        } else {
            // This is a regular function call
            return self.arena.createNode(ast.AstNode.init(.{
                .call_expr = .{ .callee = callee, .args = args }
            }, source_loc));
        }
    }

    fn finishStructInitialization(self: *Parser, struct_type: ast.NodeId) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Parse struct initialization fields: { .field1 = value1, .field2 = value2 }
        // We already consumed the opening brace in parseCall
        
        var fields = std.ArrayList(ast.FieldInit).init(self.allocator);
        
        // Skip whitespace after opening brace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        // Handle empty struct initialization
        if (self.check(.RightBrace)) {
            _ = self.advance(); // consume }
            
            // Extract type name from struct_type node if it's an identifier
            var type_name: ?[]const u8 = null;
            if (self.arena.getNode(struct_type)) |node| {
                if (node.data == .identifier) {
                    type_name = node.data.identifier.name;
                }
            }
            
            return self.arena.createNode(ast.AstNode.init(.{
                .struct_init = .{
                    .type_name = type_name,
                    .fields = fields,
                    .use_gc = false,
                }
            }, source_loc));
        }
        
        // Parse field initializations
        while (true) {
            // Skip whitespace before field
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            // Expect .fieldname = value
            _ = try self.consume(.Dot, .unexpected_token, "Expected '.' before field name in struct initialization");
            
            const field_name_token = try self.consume(.Identifier, .expected_identifier, "Expected field name after '.' in struct initialization");
            const field_name = if (field_name_token == .Identifier) field_name_token.Identifier.value else "error";
            
            // Skip whitespace around =
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            if (!self.match(&[_]std.meta.Tag(Token){.Assignment}) and !self.match(&[_]std.meta.Tag(Token){.Equals})) {
                try self.reportError(.unexpected_token, "Expected '=' after field name in struct initialization", self.getCurrentSourceLoc());
                break;
            }
            
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Parse field value
            const field_value = try self.parseExpression();
            
            // Add field to list
            try fields.append(ast.FieldInit{
                .name = field_name,
                .value = field_value,
                .source_loc = source_loc,
            });
            
            // Skip whitespace after field value
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            // Check for comma or end of struct
            if (self.match(&[_]std.meta.Tag(Token){.Comma})) {
                // Continue to next field
                continue;
            } else if (self.check(.RightBrace)) {
                // End of struct
                break;
            } else {
                try self.reportError(.unexpected_token, "Expected ',' or '}' in struct initialization", self.getCurrentSourceLoc());
                break;
            }
        }
        
        _ = try self.consume(.RightBrace, .missing_closing_brace, "Expected '}' to close struct initialization");
        
        // Extract type name from struct_type node if it's an identifier  
        var type_name: ?[]const u8 = null;
        if (self.arena.getNode(struct_type)) |node| {
            if (node.data == .identifier) {
                type_name = node.data.identifier.name;
            }
        }
        
        return self.arena.createNode(ast.AstNode.init(.{
            .struct_init = .{
                .type_name = type_name,
                .fields = fields,
                .use_gc = false,
            }
        }, source_loc));
    }

    fn isTypeConstructor(self: *Parser, callee: ast.NodeId) !bool {
        const node = self.arena.getNode(callee) orelse return false;
        
        // Check if it's an identifier that could be a type (starts with uppercase)
        if (node.data == .identifier) {
            const ident = node.data.identifier;
            if (ident.name.len > 0 and std.ascii.isUpper(ident.name[0])) {
                return true;
            }
        }
        
        // Check if it's a member expression like std.List
        if (node.data == .member_expr) {
            const member = node.data.member_expr;
            // If the field name starts with uppercase, it's likely a type
            if (member.field.len > 0 and std.ascii.isUpper(member.field[0])) {
                return true;
            }
        }
        
        return false;
    }

    fn parsePrimary(self: *Parser) !ast.NodeId {
        try self.enterRecursion();
        defer self.exitRecursion();
        
        const source_loc = self.getCurrentSourceLoc();
        self.debugPrint("parsePrimary: current={d}, token={}", .{self.current, self.peek()});

        // Skip whitespace at the start of primary expressions
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }

        // Handle literals
        if (self.match(&[_]std.meta.Tag(Token){.True})) {
            return ast.createLiteralExpr(self.arena, source_loc, ast.Literal.bool_true);
        }

        if (self.match(&[_]std.meta.Tag(Token){.False})) {
            return ast.createLiteralExpr(self.arena, source_loc, ast.Literal.bool_false);
        }

        if (self.match(&[_]std.meta.Tag(Token){.None})) {
            return ast.createLiteralExpr(self.arena, source_loc, ast.Literal.none);
        }

        if (self.match(&[_]std.meta.Tag(Token){.IntegerLiteral})) {
            const token = self.previous();
            if (ast.Literal.fromToken(token)) |literal| {
                return ast.createLiteralExpr(self.arena, source_loc, literal);
            }
        }

        if (self.match(&[_]std.meta.Tag(Token){.FloatLiteral})) {
            const token = self.previous();
            if (ast.Literal.fromToken(token)) |literal| {
                return ast.createLiteralExpr(self.arena, source_loc, literal);
            }
        }

        if (self.match(&[_]std.meta.Tag(Token){.StringLiteral})) {
            const token = self.previous();
            if (ast.Literal.fromToken(token)) |literal| {
                return ast.createLiteralExpr(self.arena, source_loc, literal);
            }
        }

        // Handle error handling expressions
        if (self.match(&[_]std.meta.Tag(Token){.Try})) {
            // Parse try expression - now independent of catch
            const expr = try self.parseUnary();
            return ast.createTryExpr(self.arena, source_loc, expr);
        }

        // Handle @import specifically
        if (self.match(&[_]std.meta.Tag(Token){.Import})) {
            // Create an identifier for @import
            const import_id = try ast.createIdentifier(self.arena, source_loc, "import");
            
            // Parse as function call if followed by parentheses
            if (self.check(.LeftParen)) {
                return self.parseCall_withCallee(import_id);
            } else {
                return import_id;
            }
        }

        // Handle other built-in functions like @import and compile-time expressions
        if (self.match(&[_]std.meta.Tag(Token){.At})) {
            const builtin_name_token = try self.consume(.Identifier, .expected_identifier, "Expected built-in function name after '@'");
            if (builtin_name_token == .Identifier) {
                const builtin_name = builtin_name_token.Identifier.value;
                
                // Handle @compile.target and @compile.insert specially
                if (std.mem.eql(u8, builtin_name, "compile")) {
                    // Expect dot after compile
                    _ = try self.consume(.Dot, .unexpected_token, "Expected '.' after '@compile'");
                    
                    const member_token = try self.consume(.Identifier, .expected_identifier, "Expected member name after '@compile.'");
                    if (member_token == .Identifier) {
                        const member_name = member_token.Identifier.value;
                        
                        if (std.mem.eql(u8, member_name, "target")) {
                            // @compile.target - return compile target expression
                            return ast.createCompileTargetExpr(self.arena, source_loc);
                        } else if (std.mem.eql(u8, member_name, "insert")) {
                            // @compile.insert(...) - handle function call
                            if (self.check(.LeftParen)) {
                                _ = self.advance(); // consume (
                                
                                // Skip whitespace
                                while (self.check(.Whitespace)) {
                                    _ = self.advance();
                                }
                                
                                // Expect string literal with code to insert
                                const code_token = try self.consume(.StringLiteral, .unexpected_token, "Expected string literal for @compile.insert");
                                var code: []const u8 = "";
                                if (code_token == .StringLiteral) {
                                    code = code_token.StringLiteral.value;
                                }
                                
                                // Skip whitespace
                                while (self.check(.Whitespace)) {
                                    _ = self.advance();
                                }
                                
                                _ = try self.consume(.RightParen, .missing_closing_paren, "Expected ')' after @compile.insert");
                                
                                return ast.createCompileInsertExpr(self.arena, source_loc, code);
                            } else {
                                try self.reportError(.unexpected_token, "Expected '(' after '@compile.insert'", self.getCurrentSourceLoc());
                                return ast.createErrorNode(self.arena, source_loc, .unexpected_token, "Expected '(' after '@compile.insert'");
                            }
                        } else {
                            // Handle @compile.print and other compile-time functions as member expressions
                            const compile_id = try ast.createIdentifier(self.arena, source_loc, "compile");
                            const member_expr = try self.arena.createNode(ast.AstNode.init(.{
                                .member_expr = .{ .object = compile_id, .field = member_name }
                            }, source_loc));
                            
                            // If followed by parentheses, parse as function call
                            if (self.check(.LeftParen)) {
                                return self.parseCall_withCallee(member_expr);
                            } else {
                                return member_expr;
                            }
                        }
                    }
                } else {
                    // Create a special identifier for other built-ins
                    const builtin_id = try ast.createIdentifier(self.arena, source_loc, builtin_name);
                    
                    // Parse as function call if followed by parentheses
                    if (self.check(.LeftParen)) {
                        return self.parseCall_withCallee(builtin_id);
                    } else {
                        return builtin_id;
                    }
                }
            }
        }

        // Handle if expression (ternary: if condition then_expr else else_expr)
        if (self.match(&[_]std.meta.Tag(Token){.If})) {
            return self.parseIfExpression();
        }

        // Handle match expression
        if (self.match(&[_]std.meta.Tag(Token){.Match})) {
            return self.parseMatchExpression();
        }

        // Handle anonymous struct literals .{...} and enum member access .identifier
        if (self.match(&[_]std.meta.Tag(Token){.Dot})) {
            if (self.check(.LeftBrace)) {
                _ = self.advance(); // consume {
                
                // For now, parse this as a simple struct literal
                // TODO: Implement proper struct literal parsing with named fields
                var args = std.ArrayList(ast.NodeId).init(self.allocator);
                // Don't defer deinit since we'll transfer ownership to AST
                
                // Skip whitespace
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                // Parse comma-separated values inside the struct
                if (!self.check(.RightBrace)) {
                    const first_value = try self.parseExpression();
                    try args.append(first_value);
                    
                    while (self.match(&[_]std.meta.Tag(Token){.Comma})) {
                        // Skip whitespace after comma
                        while (self.check(.Whitespace)) {
                            _ = self.advance();
                        }
                        
                        if (self.check(.RightBrace)) break; // Trailing comma
                        
                        const value = try self.parseExpression();
                        try args.append(value);
                    }
                }
                
                _ = try self.consume(.RightBrace, .missing_closing_brace, "Expected '}' after struct literal");
                
                // Create a struct literal AST node (simplified as call for now)
                const struct_name = try ast.createIdentifier(self.arena, source_loc, "__anonymous_struct");
                return self.arena.createNode(ast.AstNode.init(.{
                    .call_expr = .{ .callee = struct_name, .args = args }
                }, source_loc));
            } else if (self.check(.Identifier)) {
                // Enum member access: .identifier
                const member_token = self.advance();
                const member_name = if (member_token == .Identifier) member_token.Identifier.value else "error";
                
                return ast.createLiteralExpr(self.arena, source_loc, ast.Literal{ 
                    .enum_member = .{ .name = member_name } 
                });
            } else {
                // Just a dot, not followed by { or identifier, might be an error
                try self.reportError(.unexpected_token, "Unexpected '.' not followed by '{' or identifier", source_loc);
                return ast.createErrorNode(self.arena, source_loc, .unexpected_token, "Unexpected '.'");
            }
        }

        // Handle built-in type tokens
        if (self.match(&[_]std.meta.Tag(Token){.I8})) {
            return ast.createIdentifier(self.arena, source_loc, "i8");
        }
        if (self.match(&[_]std.meta.Tag(Token){.I16})) {
            return ast.createIdentifier(self.arena, source_loc, "i16");
        }
        if (self.match(&[_]std.meta.Tag(Token){.I32})) {
            return ast.createIdentifier(self.arena, source_loc, "i32");
        }
        if (self.match(&[_]std.meta.Tag(Token){.I64})) {
            return ast.createIdentifier(self.arena, source_loc, "i64");
        }
        if (self.match(&[_]std.meta.Tag(Token){.I128})) {
            return ast.createIdentifier(self.arena, source_loc, "i128");
        }
        if (self.match(&[_]std.meta.Tag(Token){.U8})) {
            return ast.createIdentifier(self.arena, source_loc, "u8");
        }
        if (self.match(&[_]std.meta.Tag(Token){.U16})) {
            return ast.createIdentifier(self.arena, source_loc, "u16");
        }
        if (self.match(&[_]std.meta.Tag(Token){.U32})) {
            return ast.createIdentifier(self.arena, source_loc, "u32");
        }
        if (self.match(&[_]std.meta.Tag(Token){.U64})) {
            return ast.createIdentifier(self.arena, source_loc, "u64");
        }
        if (self.match(&[_]std.meta.Tag(Token){.U128})) {
            return ast.createIdentifier(self.arena, source_loc, "u128");
        }
        if (self.match(&[_]std.meta.Tag(Token){.F8})) {
            return ast.createIdentifier(self.arena, source_loc, "f8");
        }
        if (self.match(&[_]std.meta.Tag(Token){.F16})) {
            return ast.createIdentifier(self.arena, source_loc, "f16");
        }
        if (self.match(&[_]std.meta.Tag(Token){.F32})) {
            return ast.createIdentifier(self.arena, source_loc, "f32");
        }
        if (self.match(&[_]std.meta.Tag(Token){.F64})) {
            return ast.createIdentifier(self.arena, source_loc, "f64");
        }
        if (self.match(&[_]std.meta.Tag(Token){.Str})) {
            return ast.createIdentifier(self.arena, source_loc, "str");
        }
        if (self.match(&[_]std.meta.Tag(Token){.Bool})) {
            return ast.createIdentifier(self.arena, source_loc, "bool");
        }
        if (self.match(&[_]std.meta.Tag(Token){.Void})) {
            return ast.createIdentifier(self.arena, source_loc, "void");
        }

        if (self.match(&[_]std.meta.Tag(Token){.Identifier})) {
            const token = self.previous();
            if (token == .Identifier) {
                return ast.createIdentifier(self.arena, source_loc, token.Identifier.value);
            }
        }

        // Garbage collected initialization: $[1, 2, 3] or $MyStruct{ .field = value }
        if (self.match(&[_]std.meta.Tag(Token){.Dollar})) {
            // Skip whitespace after $
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Parse array literal: $[1, 2, 3]
            if (self.match(&[_]std.meta.Tag(Token){.LeftBracket})) {
                var elements = std.ArrayList(ast.NodeId).init(self.allocator);
                
                // Skip whitespace and newlines after [
                while (self.check(.Whitespace) or self.check(.Newline)) {
                    _ = self.advance();
                }
                
                if (!self.check(.RightBracket)) {
                    while (true) {
                        const element = try self.parseExpression();
                        try elements.append(element);
                        
                        if (!self.match(&[_]std.meta.Tag(Token){.Comma})) break;
                        
                        // Skip whitespace and newlines after comma
                        while (self.check(.Whitespace) or self.check(.Newline)) {
                            _ = self.advance();
                        }
                        
                        if (self.check(.RightBracket)) break; // Trailing comma
                    }
                }
                
                _ = try self.consume(.RightBracket, .unexpected_token, "Expected ']' after array elements");
                
                return self.arena.createNode(ast.AstNode.init(.{
                    .array_init = .{ .elements = elements, .use_gc = true }
                }, source_loc));
            }
            
            // For now, handle other cases as errors
            try self.reportError(.invalid_expression, "Only GC arrays are currently supported", source_loc);
            _ = self.advance(); // Skip next token to avoid infinite loop
            return ast.createIdentifier(self.arena, source_loc, "error");
        }

        // Array literal: [1, 2, 3]
        if (self.match(&[_]std.meta.Tag(Token){.LeftBracket})) {
            var elements = std.ArrayList(ast.NodeId).init(self.allocator);
            
            // Skip whitespace after [
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            if (!self.check(.RightBracket)) {
                while (true) {
                    const element = try self.parseExpression();
                    try elements.append(element);
                    
                    if (!self.match(&[_]std.meta.Tag(Token){.Comma})) break;
                    
                    // Skip whitespace after comma
                    while (self.check(.Whitespace)) {
                        _ = self.advance();
                    }
                    
                    if (self.check(.RightBracket)) break; // Trailing comma
                }
            }
            
            _ = try self.consume(.RightBracket, .unexpected_token, "Expected ']' after array elements");
            
            return self.arena.createNode(ast.AstNode.init(.{
                .array_init = .{ .elements = elements, .use_gc = false }
            }, source_loc));
        }

        // Grouped expression
        if (self.match(&[_]std.meta.Tag(Token){.LeftParen})) {
            const expr = try self.parseExpression();
            _ = try self.consume(.RightParen, .missing_closing_paren, "Expected ')' after expression");
            return expr;
        }

        // If we can't parse anything, report an error but try to recover
        self.debugPrint("parsePrimary: no match found, reporting error", .{});
        try self.reportError(.invalid_expression, "Expected expression", source_loc);
        
        // CRUCIAL: Advance past the problematic token to avoid infinite loops
        if (!self.isAtEnd()) {
            _ = self.advance();
        }
        
        // Return an error node
        return ast.createErrorNode(self.arena, source_loc, .invalid_expression, "Expected expression");
    }

    // ============================================================================
    // Statement Parsing
    // ============================================================================

    pub fn parseStatement(self: *Parser) !ast.NodeId {
        try self.enterRecursion();
        defer self.exitRecursion();
        
        self.debugPrint("parseStatement: current={d}, token={}", .{self.current, self.peek()});
        
        // Safety check: store current position to detect if we're making progress
        const start_position = self.current;
        
        // Skip errors and try to recover
        if (self.panic_mode) {
            self.synchronize();
        }

        // Skip StartOfFile tokens if present
        while (self.check(.StartOfFile)) {
            self.debugPrint("Skipping StartOfFile token", .{});
            _ = self.advance();
        }

        // Skip newlines and whitespace
        while (self.check(.Newline) or self.check(.Whitespace)) {
            self.debugPrint("Skipping newline/whitespace token", .{});
            _ = self.advance();
        }
        
        // If we're at the end after skipping, don't create a dummy node - just return an error
        if (self.isAtEnd()) {
            self.debugPrint("At end of tokens after skipping whitespace", .{});
            return error.UnexpectedEndOfFile;
        }
        
        // If we've skipped whitespace and now see a closing brace, don't create a dummy node 
        if (self.check(.RightBrace)) {
            self.debugPrint("Found unexpected closing brace after skipping whitespace", .{});
            return error.UnexpectedToken;
        }
        
        // If we're at the end after skipping, don't create a dummy node - just return an error
        if (self.isAtEnd()) {
            self.debugPrint("At end of tokens after skipping whitespace", .{});
            return error.UnexpectedEndOfFile;
        }
        
        // If we've skipped whitespace and now see a closing brace, don't create a dummy node 
        if (self.check(.RightBrace)) {
            std.debug.print("parseStatement: Found unexpected closing brace after skipping whitespace\n", .{});
            return error.UnexpectedToken;
        }
        
        // Handle return statements
        if (self.check(.Return)) {
            self.debugPrint("Found return statement, parsing return", .{});
            return self.parseReturnStatement();
        }

        // Handle import statements
        if (self.check(.Import)) {
            self.debugPrint("Found import statement, parsing import", .{});
            return self.parseImportStatement();
        }

        // Handle for statements
        if (self.check(.For)) {
            self.debugPrint("Found for statement, parsing for loop", .{});
            return self.parseForStatement();
        }

        // Try to parse Howl-style declarations with whitespace enforcement
        // Check for: [pub] identifier :: 
        if (self.check(.Identifier) or self.check(.Pub)) {
            const saved_pos = self.current;
            
            // Optional pub
            if (self.check(.Pub)) {
                _ = self.advance();
                // Require space after 'pub' keyword
                if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
                    try self.reportError(.missing_space_after_keyword, "Missing space after 'pub'", self.getCurrentSourceLoc());
                }
                // Skip whitespace after pub
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
            
            // Check for identifier
            if (self.check(.Identifier)) {
                const name_token = self.advance();
                
                // Skip whitespace after identifier
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                // Check for different declaration patterns
                if (self.check(.ColonEquals)) {
                    // Pattern: identifier := expression (mutable without type)
                    _ = self.advance(); // consume :=
                    
                    // Skip whitespace after :=
                    while (self.check(.Whitespace)) {
                        _ = self.advance();
                    }
                    
                    // Get the name
                    const name = if (name_token == .Identifier) name_token.Identifier.value else "error";
                    const source_loc = self.getCurrentSourceLoc();
                    
                    // Parse the initializer expression
                    const initializer = try self.parseExpression();
                    
                    // Ensure newline after variable declaration
                    try self.requireNewlineAfterStatement();
                    
                    return self.arena.createNode(ast.AstNode.init(.{
                        .var_decl = .{
                            .name = name,
                            .type_annotation = null,
                            .initializer = initializer,
                            .is_mutable = true, // := creates mutable variables
                        }
                    }, source_loc));
                } else if (self.check(.Colon)) {
                    // Pattern: identifier : type = expression OR identifier : type : expression
                    _ = self.advance(); // consume :
                    
                    // Skip whitespace after :
                    while (self.check(.Whitespace)) {
                        _ = self.advance();
                    }
                    
                    // Parse the type annotation
                    const type_annotation = try self.parseType();
                    
                    // Skip whitespace after type
                    while (self.check(.Whitespace)) {
                        _ = self.advance();
                    }
                    
                    const name = if (name_token == .Identifier) name_token.Identifier.value else "error";
                    const source_loc = self.getCurrentSourceLoc();
                    
                    if (self.check(.Equals)) {
                        // Pattern: identifier : type = expression (mutable with type)
                        _ = self.advance(); // consume =
                        
                        // Skip whitespace after =
                        while (self.check(.Whitespace)) {
                            _ = self.advance();
                        }
                        
                        const initializer = try self.parseExpression();
                        
                        // Ensure newline after variable declaration
                        try self.requireNewlineAfterStatement();
                        
                        return self.arena.createNode(ast.AstNode.init(.{
                            .var_decl = .{
                                .name = name,
                                .type_annotation = type_annotation,
                                .initializer = initializer,
                                .is_mutable = true, // : type = creates mutable variables
                            }
                        }, source_loc));
                    } else if (self.check(.Colon)) {
                        // Pattern: identifier : type : expression (immutable with type)
                        _ = self.advance(); // consume second :
                        
                        // Skip whitespace after :
                        while (self.check(.Whitespace)) {
                            _ = self.advance();
                        }
                        
                        const initializer = try self.parseExpression();
                        
                        // Ensure newline after constant declaration
                        try self.requireNewlineAfterStatement();
                        
                        return self.arena.createNode(ast.AstNode.init(.{
                            .var_decl = .{
                                .name = name,
                                .type_annotation = type_annotation,
                                .initializer = initializer,
                                .is_mutable = false, // : type : creates immutable constants
                            }
                        }, source_loc));
                    } else {
                        // Type annotation without assignment - this might be an error or incomplete
                        try self.reportError(.unexpected_token, "Expected '=' or ':' after type annotation", self.getCurrentSourceLoc());
                        self.current = saved_pos; // Reset position
                    }
                } else if (self.check(.DoubleColon)) {
                    // Pattern: identifier :: expression (immutable without type)
                    // Require space before double colon (this check was moved from earlier)
                    
                    _ = self.advance(); // consume ::
                    
                    // Require space after double colon
                    if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
                        try self.reportError(.missing_space_around_operator, "Missing space after '::'", self.getCurrentSourceLoc());
                    }
                    
                    // Skip whitespace after double colon
                    while (self.check(.Whitespace)) {
                        _ = self.advance();
                    }
                    
                    // Get the name
                    const name = if (name_token == .Identifier) name_token.Identifier.value else "error";
                    const source_loc = self.getCurrentSourceLoc();
                    
                    // Check what follows the double colon
                    if (self.check(.Fn)) {
                        // Function declaration: [pub] name :: fn(...)
                        self.current = saved_pos; // Reset and parse as function
                        const func_result = self.parseHowlFunctionDeclaration() catch |err| {
                            // If function parsing fails, try to recover
                            self.debugPrint("Function declaration parsing failed: {}, recovering", .{err});
                            try self.reportError(.unexpected_token, "Error parsing function declaration", self.getCurrentSourceLoc());
                            // Advance to try to get unstuck
                            if (self.current == saved_pos) {
                                _ = self.advance();
                            }
                            return self.arena.createNode(ast.AstNode.init(.{
                                .error_node = .{
                                    .error_code = .unexpected_token,
                                    .message = "Failed to parse function declaration",
                                }
                            }, self.getCurrentSourceLoc()));
                        };
                        // Ensure newline after function declaration
                        try self.requireNewlineAfterStatement();
                        return func_result;
                    } else if (self.check(.Enum)) {
                        // Enum declaration: [pub] name :: enum { ... }
                        return try self.parseEnumDeclaration(name, source_loc);
                    } else if (self.check(.Error)) {
                        // Error set declaration: [pub] name :: error { ... }
                        return try self.parseErrorSetDeclaration(name, source_loc);
                    } else if (self.check(.Struct)) {
                        // Struct declaration: [pub] name :: struct { ... }
                        return try self.parseStructDeclaration(name, source_loc);
                    } else if (self.check(.Union)) {
                        // Union declaration: [pub] name :: union { ... }
                        return try self.parseUnionDeclaration(name, source_loc);
                    } else if (self.check(.Tag)) {
                        // Tagged union declaration: [pub] name :: tag { ... }
                        return try self.parseTaggedUnionDeclaration(name, source_loc);
                    } else {
                        // Constant definition: name :: expression
                        const value = try self.parseExpression();
                        
                        // Ensure newline after constant declaration
                        try self.requireNewlineAfterStatement();
                        
                        return self.arena.createNode(ast.AstNode.init(.{
                            .var_decl = .{
                                .name = name,
                                .type_annotation = null,
                                .initializer = value,
                                .is_mutable = false,
                            }
                        }, source_loc));
                    }
                } else {
                    // Not a recognized declaration pattern, reset position and parse as expression
                    self.current = saved_pos;
                }
            }
        }

        // Continue parsing assignment if needed
        const expr_result = self.parseExpression() catch |err| {
            if (self.current == start_position) {
                try self.reportError(.unexpected_token, "Error parsing expression, skipping token", self.getCurrentSourceLoc());
                _ = self.advance();
                return ast.createErrorNode(self.arena, self.getCurrentSourceLoc(), .unexpected_token, "Failed to parse expression");
            }
            return err;
        };
        
        // Howl doesn't use semicolons - if we see one, it's an error
        if (self.check(.Semicolon)) {
            try self.reportError(.unexpected_token, "Semicolons are not allowed in Howl", self.getCurrentSourceLoc());
            _ = self.advance();
        }
        
        return expr_result;
    }

    fn parseHowlFunctionDeclaration(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Parse optional 'pub'
        var is_public = false;
        if (self.match(&[_]std.meta.Tag(Token){.Pub})) {
            is_public = true;
            // Require space after 'pub'
            if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
                try self.reportError(.missing_space_after_keyword, "Missing space after 'pub'", self.getCurrentSourceLoc());
            }
        }
        
        // Skip whitespace after pub
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Parse function name
        const name_token = try self.consume(.Identifier, .expected_identifier, "Expected function name");
        const name = if (name_token == .Identifier) name_token.Identifier.value else "error";

        // Require space before ::
        if (!self.check(.Whitespace)) {
            try self.reportError(.missing_space_around_operator, "Missing space before '::'", self.getCurrentSourceLoc());
        }

        // Skip whitespace after function name
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }

        // Consume ::
        _ = try self.consume(.DoubleColon, .unexpected_token, "Expected '::' after function name");
        
        // Require space after ::
        if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
            try self.reportError(.missing_space_around_operator, "Missing space after '::'", self.getCurrentSourceLoc());
        }
        
        // Skip whitespace after ::
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Consume fn
        _ = try self.consume(.Fn, .expected_identifier, "Expected 'fn' after '::'");

        // No space required between 'fn' and '(' - allow fn() or fn ()

        // Skip whitespace after fn
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }

        _ = try self.consume(.LeftParen, .missing_closing_brace, "Expected '(' after 'fn'");
        
        // Parse parameters
        var params = std.ArrayList(ast.Parameter).init(self.allocator);
        
        // Skip whitespace after (
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        if (!self.check(.RightParen)) {
            var param_count: usize = 0;
            const max_params = 100;
            
            while (true) {
                param_count += 1;
                if (param_count > max_params) {
                    try self.reportError(.unexpected_token, "Too many parameters", self.getCurrentSourceLoc());
                    break;
                }
                
                // Skip whitespace
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                if (self.check(.RightParen)) {
                    break;
                }
                
                const param_name_token = try self.consume(.Identifier, .expected_identifier, "Expected parameter name");
                const param_name = if (param_name_token == .Identifier) param_name_token.Identifier.value else "error";
                
                // Skip whitespace after parameter name
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                var param_type: ?ast.NodeId = null;
                if (self.match(&[_]std.meta.Tag(Token){.Colon})) {
                    // Require space after colon
                    if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
                        try self.reportError(.missing_space_around_operator, "Missing space after ':'", self.getCurrentSourceLoc());
                    }
                    // Skip whitespace after colon
                    while (self.check(.Whitespace)) {
                        _ = self.advance();
                    }
                    param_type = try self.parseType();
                }
                
                try params.append(ast.Parameter{
                    .name = param_name,
                    .type_annotation = param_type,
                    .default_value = null,
                    .source_loc = self.getCurrentSourceLoc(),
                });
                
                // Skip whitespace before comma
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                if (!self.match(&[_]std.meta.Tag(Token){.Comma})) {
                    break;
                }
                
                // Require space after comma
                if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
                    try self.reportError(.missing_space_around_operator, "Missing space after ','", self.getCurrentSourceLoc());
                }
                
                // Skip whitespace after comma
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
        }

        _ = try self.consume(.RightParen, .missing_closing_paren, "Expected ')' after parameters");

        // Skip whitespace after )
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }

        // Handle return type (for !void, -> i32, or direct types like i32)
        var return_type: ?ast.NodeId = null;
        if (self.check(.Exclamation)) {
            // Handle error union type like !void
            _ = self.advance(); // consume !
            // Skip whitespace after !
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            return_type = try self.parseType();
        } else if (self.match(&[_]std.meta.Tag(Token){.Arrow})) {
            // Require space after ->
            if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
                try self.reportError(.missing_space_around_operator, "Missing space after '->'", self.getCurrentSourceLoc());
            }
            // Skip whitespace after ->
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            return_type = try self.parseType();
        } else if (self.check(.Identifier) or self.check(.Void) or self.check(.I32) or self.check(.I64) or self.check(.U32) or self.check(.U64) or self.check(.F32) or self.check(.F64) or self.check(.Str) or self.check(.Bool) or self.check(.QuestionMark)) {
            // Handle direct return type like: fn(x: i32) i32 or optional types like: fn(x: i32) ?i32
            return_type = try self.parseType();
        }

        // Skip whitespace before body
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }

        // Require space before opening brace
        if (self.check(.LeftBrace)) {
            // Check if there was whitespace before this brace
            if (self.current > 0) {
                const prev_token = self.tokens[self.current - 1];
                const curr_token = self.peek();
                
                if (prev_token.getEndPos() >= curr_token.getPos() and std.meta.activeTag(prev_token) != .Whitespace) {
                    try self.reportError(.missing_space_before_brace, "Missing space before '{'", self.getCurrentSourceLoc());
                }
            }
        }

        // Consume opening brace
        _ = try self.consume(.LeftBrace, .missing_closing_brace, "Expected '{' before function body");

        const body = try self.parseBlockStatement();
        
        // Transfer ownership of params ArrayList to AST node
        return self.arena.createNode(ast.AstNode.init(.{
            .function_decl = .{
                .name = name,
                .params = params,
                .return_type = return_type,
                .body = body,
            }
        }, source_loc));
    }

    fn parseWhileStatement(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        const condition = try self.parseExpression();
        const body = try self.parseStatement();

        return self.arena.createNode(ast.AstNode.init(.{
            .while_expr = .{
                .condition = condition,
                .body = body,
            }
        }, source_loc));
    }

    fn parseForStatement(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Consume the 'for' token
        _ = try self.consume(.For, .unexpected_token, "Expected 'for'");
        
        // Skip whitespace after 'for'
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Parse: for (iterable) |captures| body OR for (range) |captures| body
        _ = try self.consume(.LeftParen, .missing_closing_paren, "Expected '(' after 'for'");
        
        // Check if this starts with a range expression (..<, ..=)
        var iterable: ast.NodeId = undefined;
        
        if (self.check(.DotDotLessThan) or self.check(.DotDotEquals)) {
            // Handle `..<end` or `..=end` syntax
            const is_inclusive = self.check(.DotDotEquals);
            _ = if (is_inclusive) self.advance() else self.advance(); // consume ..< or ..=
            
            // Skip whitespace after range operator
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            const end_expr = try self.parseExpression();
            
                iterable = try self.arena.createNode(ast.AstNode.init(.{
                .range_expr = .{
                    .start = null,
                    .end = end_expr,
                    .inclusive = is_inclusive,
                }
            }, source_loc));
        } else {
            // Parse the first expression (could be start of range or regular iterable)
            const first_expr = try self.parseExpression();
            
            // Skip whitespace
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Check if this is a range expression (start..=end or start..<end)
            if (self.check(.DotDotLessThan) or self.check(.DotDotEquals)) {
                const is_inclusive = self.check(.DotDotEquals);
                _ = if (is_inclusive) self.advance() else self.advance(); // consume ..< or ..=
                
                // Skip whitespace after range operator
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                const end_expr = try self.parseExpression();
                
            iterable = try self.arena.createNode(ast.AstNode.init(.{
                    .range_expr = .{
                        .start = first_expr,
                        .end = end_expr,
                        .inclusive = is_inclusive,
                    }
                }, source_loc));
            } else {
                // This is a regular iterable, not a range
                iterable = first_expr;
            }
        }
        
        // Skip whitespace
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Check for optional range specification (like 0..) - keep old syntax for backward compatibility
        var range_start: ?ast.NodeId = null;
        if (self.match(&[_]std.meta.Tag(Token){.Comma})) {
            // Skip whitespace after comma
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Parse range start (e.g., "0" from "0..")
            range_start = try self.parseExpression();
            
            // Expect ".." after range start
            _ = try self.consume(.DotDot, .unexpected_token, "Expected '..' in range specification");
        }
        
        _ = try self.consume(.RightParen, .missing_closing_paren, "Expected ')' after for expression");
        
        // Skip whitespace before captures
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Parse captures: |value| or |value, index| or |_, index|
        _ = try self.consume(.Pipe, .unexpected_token, "Expected '|' to start captures");
        
        var captures = std.ArrayList(ast.ForCapture).init(self.allocator);
        
        // Parse first capture
        var first_capture_name: []const u8 = undefined;
        if (self.check(.Identifier)) {
            const first_capture_token = try self.consume(.Identifier, .expected_identifier, "Expected capture variable name");
            first_capture_name = first_capture_token.Identifier.value;
        } else if (self.check(.Underscore)) {
            _ = try self.consume(.Underscore, .unexpected_token, "Expected capture variable name");
            first_capture_name = "_";
        } else {
            try self.reportError(.expected_identifier, "Expected identifier or '_' for capture variable", self.getCurrentSourceLoc());
            first_capture_name = "_"; // Fallback
        }
        
        try captures.append(ast.ForCapture{
            .name = first_capture_name,
            .capture_type = if (std.mem.eql(u8, first_capture_name, "_")) .ignored else .value,
            .source_loc = source_loc,
        });
        
        // Check for second capture (index)
        if (self.match(&[_]std.meta.Tag(Token){.Comma})) {
            // Skip whitespace after comma
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            var second_capture_name: []const u8 = undefined;
            if (self.check(.Identifier)) {
                const second_capture_token = try self.consume(.Identifier, .expected_identifier, "Expected second capture variable name");
                second_capture_name = second_capture_token.Identifier.value;
            } else if (self.check(.Underscore)) {
                _ = try self.consume(.Underscore, .unexpected_token, "Expected capture variable name");
                second_capture_name = "_";
            } else {
                try self.reportError(.expected_identifier, "Expected identifier or '_' for second capture variable", self.getCurrentSourceLoc());
                second_capture_name = "_"; // Fallback
            }
            
            try captures.append(ast.ForCapture{
                .name = second_capture_name,
                .capture_type = if (std.mem.eql(u8, second_capture_name, "_")) .ignored else .index,
                .source_loc = source_loc,
            });
        }
        
        _ = try self.consume(.Pipe, .unexpected_token, "Expected '|' to end captures");
        
        // Skip whitespace before body
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Consume opening brace
        _ = try self.consume(.LeftBrace, .missing_closing_brace, "Expected '{' to start for loop body");
        
        // Parse the body
        const body = try self.parseBlockStatement();
        
        return self.arena.createNode(ast.AstNode.init(.{
            .for_expr = .{
                .iterable = iterable,
                .captures = captures,
                .body = body,
            }
        }, source_loc));
    }

    fn parseReturnStatement(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Consume the 'return' token
        _ = try self.consume(.Return, .unexpected_token, "Expected 'return'");
        
        var value: ?ast.NodeId = null;
        
        // Skip whitespace after return
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        if (!self.check(.RightBrace) and !self.isAtEnd() and !self.check(.Semicolon)) {
            value = try self.parseExpression();
        }
        
        // Howl doesn't use semicolons - if we see one, it's an error
        if (self.check(.Semicolon)) {
            try self.reportError(.unexpected_token, "Semicolons are not allowed in Howl", self.getCurrentSourceLoc());
            _ = self.advance(); // Skip the semicolon
        }

        return self.arena.createNode(ast.AstNode.init(.{
            .return_stmt = .{ .value = value }
        }, source_loc));
    }

    fn parseImportStatement(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Consume the @import token
        _ = try self.consume(.Import, .unexpected_token, "Expected '@import'");
        
        // Skip whitespace after @import
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect opening parenthesis
        _ = try self.consume(.LeftParen, .unexpected_token, "Expected '(' after '@import'");
        
        // Skip whitespace after (
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect string literal for the module path
        const module_path_token = try self.consume(.StringLiteral, .unexpected_token, "Expected string literal for module path");
        const module_path = if (module_path_token == .StringLiteral) module_path_token.StringLiteral.value else "error";
        
        // Skip whitespace before )
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect closing parenthesis
        _ = try self.consume(.RightParen, .unexpected_token, "Expected ')' after module path");
        
        // Ensure newline after import statement
        try self.requireNewlineAfterStatement();
        
        return self.arena.createNode(ast.AstNode.init(.{
            .import_decl = .{ .module_path = module_path }
        }, source_loc));
    }

    fn parseEnumDeclaration(self: *Parser, name: []const u8, source_loc: ast.SourceLoc) !ast.NodeId {
        // Consume the 'enum' token
        _ = try self.consume(.Enum, .unexpected_token, "Expected 'enum'");
        
        // Require space after 'enum'
        if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
            try self.reportError(.missing_space_after_keyword, "Missing space after 'enum'", self.getCurrentSourceLoc());
        }
        
        // Skip whitespace after enum
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect opening brace
        _ = try self.consume(.LeftBrace, .unexpected_token, "Expected '{' after 'enum'");
        
        // Skip whitespace after opening brace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        // Parse enum members
        var members = std.ArrayList(ast.EnumMember).init(self.allocator);
        
        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            // Skip whitespace and newlines
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            if (self.check(.RightBrace)) break;
            
            // Parse enum member name
            const member_token = try self.consume(.Identifier, .expected_identifier, "Expected enum member name");
            const member_name = if (member_token == .Identifier) member_token.Identifier.value else "error";
            const member_loc = self.getCurrentSourceLoc();
            
            // Skip whitespace after member name
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Check for explicit value assignment (= value)
            var member_value: ?ast.NodeId = null;
            if (self.check(.Equals)) {
                _ = self.advance(); // consume =
                
                // Skip whitespace after =
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                member_value = try self.parseExpression();
                
                // Skip whitespace after value
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
            
            try members.append(ast.EnumMember{
                .name = member_name,
                .value = member_value,
                .source_loc = member_loc,
            });
            
            // Skip whitespace after member
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Optional comma
            if (self.check(.Comma)) {
                _ = self.advance();
                // Skip whitespace after comma
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
            
            // Skip trailing newlines
            while (self.check(.Newline)) {
                _ = self.advance();
            }
        }
        
        // Expect closing brace
        _ = try self.consume(.RightBrace, .unexpected_token, "Expected '}' after enum members");
        
        // Ensure newline after enum declaration
        try self.requireNewlineAfterStatement();
        
        return self.arena.createNode(ast.AstNode.init(.{
            .enum_decl = .{
                .name = name,
                .members = members,
            }
        }, source_loc));
    }
    
    fn parseErrorSetDeclaration(self: *Parser, name: []const u8, source_loc: ast.SourceLoc) !ast.NodeId {
        // Consume the 'error' token
        _ = try self.consume(.Error, .unexpected_token, "Expected 'error'");
        
        // Require space after 'error'
        if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
            try self.reportError(.missing_space_after_keyword, "Missing space after 'error'", self.getCurrentSourceLoc());
        }
        
        // Skip whitespace after error
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect opening brace
        _ = try self.consume(.LeftBrace, .unexpected_token, "Expected '{' after 'error'");
        
        // Skip whitespace after opening brace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        // Parse error names
        var errors = std.ArrayList([]const u8).init(self.allocator);
        
        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            // Skip whitespace and newlines
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            if (self.check(.RightBrace)) break;
            
            // Parse error name
            const error_token = try self.consume(.Identifier, .expected_identifier, "Expected error name");
            const error_name = if (error_token == .Identifier) error_token.Identifier.value else "error";
            
            try errors.append(error_name);
            
            // Skip whitespace after error name
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Optional comma
            if (self.check(.Comma)) {
                _ = self.advance();
                // Skip whitespace after comma
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
            
            // Skip trailing newlines
            while (self.check(.Newline)) {
                _ = self.advance();
            }
        }
        
        // Expect closing brace
        _ = try self.consume(.RightBrace, .unexpected_token, "Expected '}' after error names");
        
        // Ensure newline after error set declaration
        try self.requireNewlineAfterStatement();
        
        return ast.createErrorSetDecl(self.arena, source_loc, name, errors);
    }

    fn parseStructDeclaration(self: *Parser, name: []const u8, source_loc: ast.SourceLoc) !ast.NodeId {
        // Consume the 'struct' token
        _ = try self.consume(.Struct, .unexpected_token, "Expected 'struct'");
        
        // Require space after 'struct'
        if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
            try self.reportError(.missing_space_after_keyword, "Missing space after 'struct'", self.getCurrentSourceLoc());
        }
        
        // Skip whitespace after struct
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect opening brace
        _ = try self.consume(.LeftBrace, .unexpected_token, "Expected '{' after 'struct'");
        
        // Skip whitespace after opening brace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        // Parse struct fields
        var fields = std.ArrayList(ast.Field).init(self.allocator);
        
        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            // Skip whitespace and newlines
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            if (self.check(.RightBrace)) break;
            
            // Parse field name
            const field_token = try self.consume(.Identifier, .expected_identifier, "Expected struct field name");
            const field_name = if (field_token == .Identifier) field_token.Identifier.value else "error";
            const field_loc = self.getCurrentSourceLoc();
            
            // Skip whitespace after field name
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Expect colon after field name
            _ = try self.consume(.Colon, .unexpected_token, "Expected ':' after struct field name");
            
            // Skip whitespace after colon
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Parse field type
            const field_type = try self.parseType();
            
            try fields.append(ast.Field{
                .name = field_name,
                .type_annotation = field_type,
                .default_value = null,
                .source_loc = field_loc,
            });
            
            // Skip whitespace after field type
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Optional comma
            if (self.check(.Comma)) {
                _ = self.advance();
                // Skip whitespace after comma
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
            
            // Skip trailing newlines
            while (self.check(.Newline)) {
                _ = self.advance();
            }
        }
        
        // Expect closing brace
        _ = try self.consume(.RightBrace, .unexpected_token, "Expected '}' after struct fields");
        
        // Ensure newline after struct declaration
        try self.requireNewlineAfterStatement();
        
        return ast.createStructDecl(self.arena, source_loc, name, fields, false);
    }

    fn parseUnionDeclaration(self: *Parser, name: []const u8, source_loc: ast.SourceLoc) !ast.NodeId {
        // Consume the 'union' token
        _ = try self.consume(.Union, .unexpected_token, "Expected 'union'");
        
        // Require space after 'union'
        if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
            try self.reportError(.missing_space_after_keyword, "Missing space after 'union'", self.getCurrentSourceLoc());
        }
        
        // Skip whitespace after union
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect opening brace
        _ = try self.consume(.LeftBrace, .unexpected_token, "Expected '{' after 'union'");
        
        // Skip whitespace after opening brace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        // Parse union variants
        var variants = std.ArrayList(ast.UnionVariant).init(self.allocator);
        
        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            // Skip whitespace and newlines
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            if (self.check(.RightBrace)) break;
            
            // Parse variant name
            const variant_token = try self.consume(.Identifier, .expected_identifier, "Expected union variant name");
            const variant_name = if (variant_token == .Identifier) variant_token.Identifier.value else "error";
            const variant_loc = self.getCurrentSourceLoc();
            
            // Skip whitespace after variant name
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Check for optional payload type: variant_name(payload_type)
            var payload_type: ?ast.NodeId = null;
            if (self.check(.LeftParen)) {
                _ = self.advance(); // consume (
                
                // Skip whitespace after (
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                // Parse payload type
                payload_type = try self.parseType();
                
                // Skip whitespace before )
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
                
                _ = try self.consume(.RightParen, .missing_closing_paren, "Expected ')' after union variant payload type");
            }
            
            try variants.append(ast.UnionVariant{
                .name = variant_name,
                .payload_type = payload_type,
                .source_loc = variant_loc,
            });
            
            // Skip whitespace after variant
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Optional comma
            if (self.check(.Comma)) {
                _ = self.advance();
                // Skip whitespace after comma
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
            
            // Skip trailing newlines
            while (self.check(.Newline)) {
                _ = self.advance();
            }
        }
        
        // Expect closing brace
        _ = try self.consume(.RightBrace, .unexpected_token, "Expected '}' after union variants");
        
        // Ensure newline after union declaration
        try self.requireNewlineAfterStatement();
        
        return ast.createUnionDecl(self.arena, source_loc, name, variants);
    }

    fn parseTaggedUnionDeclaration(self: *Parser, name: []const u8, source_loc: ast.SourceLoc) !ast.NodeId {
        // Consume the 'tag' token
        _ = try self.consume(.Tag, .unexpected_token, "Expected 'tag'");
        
        // Require space after 'tag'
        if (!self.isAtEnd() and !self.check(.Whitespace) and !self.check(.Newline)) {
            try self.reportError(.missing_space_after_keyword, "Missing space after 'tag'", self.getCurrentSourceLoc());
        }
        
        // Skip whitespace after tag
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Expect opening brace
        _ = try self.consume(.LeftBrace, .unexpected_token, "Expected '{' after 'tag'");
        
        // Skip whitespace after opening brace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        // Parse tagged union variants (same as regular union variants)
        var variants = std.ArrayList(ast.UnionVariant).init(self.allocator);
        
        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            // Skip whitespace and newlines
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            if (self.check(.RightBrace)) break;
            
            // Parse variant name
            const variant_token = try self.consume(.Identifier, .expected_identifier, "Expected tagged union variant name");
            const variant_name = if (variant_token == .Identifier) variant_token.Identifier.value else "error";
            const variant_loc = self.getCurrentSourceLoc();
            
            // Skip whitespace after variant name
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Expect colon for tagged union: variant_name: payload_type
            _ = try self.consume(.Colon, .unexpected_token, "Expected ':' after tagged union variant name");
            
            // Skip whitespace after colon
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Parse payload type (required for tagged unions)
            const payload_type = try self.parseType();
            
            try variants.append(ast.UnionVariant{
                .name = variant_name,
                .payload_type = payload_type,
                .source_loc = variant_loc,
            });
            
            // Skip whitespace after variant
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Optional comma
            if (self.check(.Comma)) {
                _ = self.advance();
                // Skip whitespace after comma
                while (self.check(.Whitespace)) {
                    _ = self.advance();
                }
            }
            
            // Skip trailing newlines
            while (self.check(.Newline)) {
                _ = self.advance();
            }
        }
        
        // Expect closing brace
        _ = try self.consume(.RightBrace, .unexpected_token, "Expected '}' after tagged union variants");
        
        // Ensure newline after tagged union declaration
        try self.requireNewlineAfterStatement();
        
        return ast.createUnionDecl(self.arena, source_loc, name, variants);
    }

    fn parseBlockStatement(self: *Parser) anyerror!ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        var statements = std.ArrayList(ast.NodeId).init(self.allocator);

        // Skip whitespace tokens at the start of the block
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }

        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            // Skip whitespace before each statement
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Check again for RightBrace after skipping whitespace
            if (self.check(.RightBrace)) {
                break;
            }
            
            const stmt = self.parseStatement() catch |err| {
                if (err == error.UnexpectedToken and self.check(.RightBrace)) {
                    // This is expected - we've reached the end of the block
                    break;
                }
                // Error recovery for block statements
                try self.reportError(.unexpected_token, "Error parsing statement in block", self.getCurrentSourceLoc());
                self.synchronize();
                continue;
            };
            try statements.append(stmt);
        }

        _ = try self.consume(.RightBrace, .missing_closing_brace, "Expected '}' after block");

        // Use the ArrayList's items directly - they'll live as long as the ArrayList
        // TODO: This creates a small memory leak. The dupe'd slice should be tracked and freed.
        // Transfer ownership of ArrayList to AST node
        return ast.createBlock(self.arena, source_loc, statements);
    }

    // ============================================================================
    // Public API
    // ============================================================================

    pub fn parseProgram(self: *Parser) !ast.NodeId {
        self.debugPrint("Starting parseProgram, tokens: {d}", .{self.tokens.len});
        const source_loc = self.getCurrentSourceLoc();
        var statements = std.ArrayList(ast.NodeId).init(self.allocator);
        
        var iteration_count: usize = 0;
        var stuck_counter: usize = 0;
        const max_iterations = self.tokens.len * 10; // Allow 10x token count iterations

        while (!self.isAtEnd()) {
            iteration_count += 1;
            self.debugPrint("parseProgram iteration {d}, current: {d}, token: {}", .{iteration_count, self.current, self.peek()});
            
            if (iteration_count > max_iterations) {
                try self.reportError(.unexpected_token, "parser reached maximum iterations, stopping to prevent infinite loop", self.getCurrentSourceLoc());
                break;
            }
            
            if (self.panic_mode) {
                self.synchronize();
                continue;
            }
            
            // Safety check: prevent infinite loops
            const old_current = self.current;
            
            const stmt = self.parseStatement() catch |err| blk: {
                self.debugPrint("parseStatement failed with error: {}, advancing", .{err});
                // If parsing failed due to end of file or unexpected token, don't create dummy nodes
                if (err == error.UnexpectedEndOfFile or err == error.UnexpectedToken) {
                    self.debugPrint("Skipping dummy node creation for error: {}", .{err});
                    continue; // Skip to next iteration without appending anything
                }
                // If statement parsing fails, try to recover by advancing
                if (self.current == old_current and !self.isAtEnd()) {
                    try self.reportError(.unexpected_token, "Error parsing statement, skipping token", self.getCurrentSourceLoc());
                    _ = self.advance();
                }
                // Create an error node to continue parsing
                break :blk ast.createErrorNode(self.arena, self.getCurrentSourceLoc(), .unexpected_token, "Failed to parse statement") catch unreachable;
            };
            self.debugPrint("Parsed statement NodeId: {}, appending to statements", .{stmt});
            try statements.append(stmt);
            
            // Ensure we're making progress
            if (self.current == old_current and !self.isAtEnd()) {
                // Force advance to prevent infinite loop, but only report error once per token
                if (self.current > 0) { // Only report if we haven't already reported for this token
                    try self.reportError(.unexpected_token, "Unexpected token, skipping", self.getCurrentSourceLoc());
                }
                _ = self.advance();
                // Add a safety counter to prevent getting truly stuck
                stuck_counter += 1;
                if (stuck_counter > 100) { // Prevent infinite loops
                    try self.reportError(.unexpected_token, "Parser appears to be in infinite loop, stopping", self.getCurrentSourceLoc());
                    break;
                }
            } else {
                stuck_counter = 0; // Reset counter when we make progress
            }
        }

        // Use the ArrayList's items directly - they'll live as long as the ArrayList
        // TODO: This creates a small memory leak. The dupe'd slice should be tracked and freed.
        // Transfer ownership of ArrayList to AST node
        return ast.createBlock(self.arena, source_loc, statements);
    }

    fn parseIfExpression(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Skip any whitespace after 'if'
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        // Parse: if (condition) then_expr else else_expr
        _ = try self.consume(.LeftParen, .missing_closing_paren, "Expected '(' after 'if'");
        const condition = try self.parseExpression();
        _ = try self.consume(.RightParen, .missing_closing_paren, "Expected ')' after if condition");
        
        // Skip whitespace before then_branch
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        const then_branch = try self.parseExpression();
        
        // Skip whitespace before 'else'
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        _ = try self.consume(.Else, .unexpected_token, "Expected 'else' in if expression");
        
        // Skip whitespace before else_branch
        while (self.check(.Whitespace)) {
            _ = self.advance();
        }
        
        const else_branch = try self.parseExpression();
        
        return self.arena.createNode(ast.AstNode.init(.{
            .if_expr = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            }
        }, source_loc));
    }

    fn parseMatchExpression(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Parse: match expression { | pattern => body | pattern => body }
        const expression = try self.parseExpression();
        _ = try self.consume(.LeftBrace, .missing_closing_brace, "Expected '{' after match expression");
        
        var arms = std.ArrayList(ast.MatchArm).init(self.allocator);
        
        // Skip initial whitespace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            // Parse match arm: | pattern => body
            _ = try self.consume(.Pipe, .unexpected_token, "Expected '|' at start of match arm");
            
            const pattern = try self.parseMatchPattern();
            
            // Skip whitespace after pattern
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            // Optional guard condition
            var guard: ?ast.NodeId = null;
            if (self.match(&[_]std.meta.Tag(Token){.If})) {
                guard = try self.parseExpression();
            }
            
            _ = try self.consume(.DoubleArrow, .unexpected_token, "Expected '=>' after match pattern");
            
            // Skip whitespace after '=>'
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
            
            // Handle variable binding syntax: |variable|
            var binding_var: ?[]const u8 = null;
            if (self.match(&[_]std.meta.Tag(Token){.Pipe})) {
                if (self.match(&[_]std.meta.Tag(Token){.Identifier})) {
                    const var_token = self.previous();
                    if (var_token == .Identifier) {
                        binding_var = var_token.Identifier.value;
                    }
                } else {
                    try self.reportError(.unexpected_token, "Expected variable name in binding pattern", self.getCurrentSourceLoc());
                }
                _ = try self.consume(.Pipe, .unexpected_token, "Expected '|' after binding variable");
                
                // Skip whitespace after binding
                while (self.check(.Whitespace) or self.check(.Newline)) {
                    _ = self.advance();
                }
            }
            
            // If we have a binding variable and the pattern is Some, update the pattern
            var final_pattern = pattern;
            if (binding_var) |var_name| {
                if (pattern == .some) {
                    final_pattern = ast.MatchPattern{ .some = .{ .bind_variable = var_name } };
                }
            }
            
            const body = if (self.check(.LeftBrace)) blk: {
                _ = try self.consume(.LeftBrace, .missing_closing_brace, "Expected '{' at start of block");
                break :blk try self.parseBlockStatement();
            } else 
                try self.parseExpression();
            
            try arms.append(ast.MatchArm{
                .pattern = final_pattern,
                .guard = guard,
                .body = body,
                .source_loc = self.getCurrentSourceLoc(),
            });
            
            // Skip trailing whitespace and newlines
            while (self.check(.Whitespace) or self.check(.Newline)) {
                _ = self.advance();
            }
        }
        
        _ = try self.consume(.RightBrace, .missing_closing_brace, "Expected '}' after match arms");
        
        // Check if this is a compile-time match expression (match @compile.target)
        const expr_node = self.arena.getNodeConst(expression);
        if (expr_node) |node| {
            if (node.data == .compile_target_expr) {
                // This is match @compile.target - create a special compile-time match expression
                var compile_arms = std.ArrayList(ast.CompileMatchArm).init(self.allocator);
                for (arms.items) |arm| {
                    // Extract target from enum member pattern (e.g., .c -> CompileTarget.c)
                    const target = if (arm.pattern == .enum_member) blk: {
                        if (std.mem.eql(u8, arm.pattern.enum_member, "c")) {
                            break :blk ast.CompileTarget.c;
                        } else if (std.mem.eql(u8, arm.pattern.enum_member, "javascript")) {
                            break :blk ast.CompileTarget.javascript;
                        } else {
                            // Default fallback - could report error here
                            break :blk ast.CompileTarget.c;
                        }
                    } else ast.CompileTarget.c; // Default fallback
                    
                    try compile_arms.append(ast.CompileMatchArm{
                        .target = target,
                        .body = arm.body,
                        .source_loc = arm.source_loc,
                    });
                }
                arms.deinit(); // Clean up the regular arms
                
                return self.arena.createNode(ast.AstNode.init(.{
                    .match_compile_expr = .{ .target_expr = expression, .arms = compile_arms }
                }, source_loc));
            }
        }
        
        // Transfer ownership of arms ArrayList to AST node
        return self.arena.createNode(ast.AstNode.init(.{
            .match_expr = .{ .expression = expression, .arms = arms }
        }, source_loc));
    }

    fn parseMatchPattern(self: *Parser) !ast.MatchPattern {
        // Skip any whitespace
        while (self.check(.Whitespace) or self.check(.Newline)) {
            _ = self.advance();
        }
        
        // Handle wildcard pattern
        if (self.match(&[_]std.meta.Tag(Token){.Underscore})) {
            return ast.MatchPattern.wildcard;
        }
        
        // Handle enum member pattern (.identifier)
        if (self.match(&[_]std.meta.Tag(Token){.Dot})) {
            if (self.match(&[_]std.meta.Tag(Token){.Identifier})) {
                const token = self.previous();
                if (token == .Identifier) {
                    return ast.MatchPattern{ .enum_member = token.Identifier.value };
                }
            }
            try self.reportError(.unexpected_token, "Expected identifier after '.' in match pattern", self.getCurrentSourceLoc());
            return ast.MatchPattern.wildcard; // fallback
        }
        
        // Handle literal patterns  
        if (self.match(&[_]std.meta.Tag(Token){.True}) or 
           self.match(&[_]std.meta.Tag(Token){.False}) or 
           self.match(&[_]std.meta.Tag(Token){.IntegerLiteral}) or 
           self.match(&[_]std.meta.Tag(Token){.FloatLiteral}) or 
           self.match(&[_]std.meta.Tag(Token){.StringLiteral}) or 
           self.match(&[_]std.meta.Tag(Token){.CharLiteral})) {
            const token = self.previous();
            if (ast.Literal.fromToken(token)) |literal| {
                return ast.MatchPattern{ .literal = literal };
            }
        }
        
        // Handle None pattern (optional matching)
        if (self.match(&[_]std.meta.Tag(Token){.None})) {
            return ast.MatchPattern.none_pattern;
        }
        
        // Handle Some pattern with value capture: Some => |variable|
        if (self.match(&[_]std.meta.Tag(Token){.Some})) {
            // Look ahead to see if this is followed by => |variable|
            // We need to check if we're in the context of parsing a match arm
            // Since this is called from the match parsing, we can assume we're looking for pattern => body
            return ast.MatchPattern{ .some = .{ .bind_variable = "value" } }; // Default variable name for now
        }
        
        // Handle identifier patterns
        if (self.match(&[_]std.meta.Tag(Token){.Identifier})) {
            const token = self.previous();
            if (token == .Identifier) {
                return ast.MatchPattern{ .identifier = token.Identifier.value };
            }
        }
        
        // Handle comparison patterns (< value, > value, == value, etc.)
        if (self.match(&[_]std.meta.Tag(Token){.LessThan}) or 
           self.match(&[_]std.meta.Tag(Token){.GreaterThan}) or 
           self.match(&[_]std.meta.Tag(Token){.LessThanEqual}) or 
           self.match(&[_]std.meta.Tag(Token){.GreaterThanEqual}) or 
           self.match(&[_]std.meta.Tag(Token){.EqualEqual}) or 
           self.match(&[_]std.meta.Tag(Token){.NotEqual})) {
            const operator_token = self.previous();
            const operator = std.meta.activeTag(operator_token);
            
            // Parse the value to compare against
            const value_node = try self.parseExpression();
            
            return ast.MatchPattern{ .comparison = .{
                .operator = operator,
                .value = value_node,
            }};
        }
        
        // Handle range patterns (simplified for now)
        // TODO: Implement proper range pattern parsing
        
        try self.reportError(.unexpected_token, "Invalid match pattern", self.getCurrentSourceLoc());
        return ast.MatchPattern.wildcard; // fallback
    }

    fn parseType(self: *Parser) !ast.NodeId {
        const source_loc = self.getCurrentSourceLoc();
        
        // Handle optional types (e.g., ?i32, ?str)
        if (self.match(&[_]std.meta.Tag(Token){.QuestionMark})) {
            const inner_type = try self.parseType();
            return ast.createOptionalTypeExpr(self.arena, source_loc, inner_type);
        }
        
        // Handle error union types (e.g., !void, !i32)
        if (self.match(&[_]std.meta.Tag(Token){.Exclamation})) {
            const payload_type = try self.parseType();
            return ast.createErrorUnionType(self.arena, source_loc, null, payload_type);
        }
        
        // Handle array types [N]T or []T
        if (self.match(&[_]std.meta.Tag(Token){.LeftBracket})) {
            var array_size: ?ast.NodeId = null;
            
            // Skip whitespace after [
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            // Check if this is a sized array [N] or slice []
            if (!self.check(.RightBracket)) {
                array_size = try self.parseExpression();
            }
            
            _ = try self.consume(.RightBracket, .unexpected_token, "Expected ']' in array type");
            
            // Skip whitespace after ]
            while (self.check(.Whitespace)) {
                _ = self.advance();
            }
            
            const element_type = try self.parseType();
            
            // Create proper array/slice type AST node
            if (array_size) |size| {
                // TODO: Create proper sized array type node
                _ = size;
                return ast.createIdentifier(self.arena, source_loc, "[N]T"); // Simplified sized array for now
            } else {
                return ast.createSliceTypeExpr(self.arena, source_loc, element_type);
            }
        }
        
        // Handle built-in types
        if (self.match(&[_]std.meta.Tag(Token){.Void})) {
            return ast.createIdentifier(self.arena, source_loc, "void");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.I32})) {
            return ast.createIdentifier(self.arena, source_loc, "i32");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.I64})) {
            return ast.createIdentifier(self.arena, source_loc, "i64");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.U32})) {
            return ast.createIdentifier(self.arena, source_loc, "u32");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.U64})) {
            return ast.createIdentifier(self.arena, source_loc, "u64");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.F32})) {
            return ast.createIdentifier(self.arena, source_loc, "f32");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.F64})) {
            return ast.createIdentifier(self.arena, source_loc, "f64");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.Str})) {
            return ast.createIdentifier(self.arena, source_loc, "str");
        }
        
        if (self.match(&[_]std.meta.Tag(Token){.Bool})) {
            return ast.createIdentifier(self.arena, source_loc, "bool");
        }
        
        // Handle identifier types (including ErrorSet!Type)
        if (self.match(&[_]std.meta.Tag(Token){.Identifier})) {
            const token = self.previous();
            if (token == .Identifier) {
                const identifier = try ast.createIdentifier(self.arena, source_loc, token.Identifier.value);
                
                // Check for error union syntax: ErrorSet!Type
                if (self.check(.Exclamation)) {
                    _ = self.advance(); // consume !
                    const payload_type = try self.parseType();
                    return ast.createErrorUnionType(self.arena, source_loc, identifier, payload_type);
                }
                
                return identifier;
            }
        }
        
        // TODO: Handle more complex types like pointers, arrays, etc.
        
        try self.reportError(.unexpected_token, "Expected type", source_loc);
        return ast.createIdentifier(self.arena, source_loc, "error"); // Return error type instead of throwing
    }
};