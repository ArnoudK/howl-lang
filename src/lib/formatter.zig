const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Lexer = @import("lexer_enhanced.zig").Lexer;
const LexerFile = @import("lexer_enhanced.zig").LexerFile;
const Parser = @import("parser.zig").Parser;
const ErrorSystem = @import("error_system.zig");

pub const FormatterOptions = struct {
    indent_size: u32 = 4,
    use_tabs: bool = false,
    max_line_length: u32 = 100,
    preserve_blank_lines: bool = true,
    max_blank_lines: u32 = 2,
    trailing_comma_threshold: u32 = 3, // Format multi-line when more than N items, even without trailing comma
    always_multiline_trailing_comma: bool = true, // Always format multi-line when trailing comma is present
};

pub const FormatResult = struct {
    formatted_code: []const u8,
    success: bool,
    error_message: ?[]const u8,

    pub fn deinit(self: FormatResult, allocator: std.mem.Allocator) void {
        allocator.free(self.formatted_code);
        if (self.error_message) |err| {
            allocator.free(err);
        }
    }
};

pub const Formatter = struct {
    allocator: std.mem.Allocator,
    options: FormatterOptions,
    output: std.ArrayList(u8),
    current_indent: u32,
    last_token_was_newline: bool,
    consecutive_blank_lines: u32,

    pub fn init(allocator: std.mem.Allocator, options: FormatterOptions) Formatter {
        return Formatter{
            .allocator = allocator,
            .options = options,
            .output = std.ArrayList(u8).init(allocator),
            .current_indent = 0,
            .last_token_was_newline = false,
            .consecutive_blank_lines = 0,
        };
    }

    pub fn deinit(self: *Formatter) void {
        self.output.deinit();
    }

    pub fn format(self: *Formatter, source_code: []const u8) !FormatResult {
        // Parse the source code into AST
        var lexer = Lexer.init(self.allocator);
        defer lexer.deinit();

        const src_copy = try self.allocator.dupe(u8, source_code);
        defer self.allocator.free(src_copy);

        // Create a lexer file
        var lexer_file = LexerFile.init(self.allocator, &lexer, "<formatter>", src_copy);
        defer lexer_file.deinit();

        // Tokenize the input
        try lexer_file.tokenize();

        // Get the tokens from the lexer_file
        const tokens = lexer_file.tokens.items;

        // Set up required components for parsing
        var arena = ast.AstArena.init(self.allocator);
        defer arena.deinit();

        var errors = ErrorSystem.ErrorCollector.init(self.allocator);
        defer errors.deinit();

        var source_map = ErrorSystem.SourceMap.init(self.allocator, src_copy) catch {
            // Fallback to token-based formatting if source map fails
            return self.formatTokens(tokens);
        };
        defer source_map.deinit();

        // Try to parse into AST using the same approach as compile_process.zig
        var parser = Parser{
            .allocator = self.allocator,
            .tokens = tokens,
            .current = 0,
            .arena = &arena,
            .errors = &errors,
            .source_map = &source_map,
            .file_path = "<formatter>",
            .panic_mode = false,
            .in_recovery = false,
            .recovery_tokens = &.{},
            .debug_mode = false,
            .recursion_depth = 0,
            .max_recursion_depth = 1000,
        };

        const ast_result = parser.parseProgram() catch |err| {
            // If parsing fails, fall back to token-based formatting
            std.debug.print("AST parsing failed with error: {}\n", .{err});
            return self.formatTokens(tokens);
        };

        // Use AST-based formatting if parsing succeeded
        return self.formatFromAST(ast_result, &arena);
    }

    fn formatTokens(self: *Formatter, tokens: []Token) !FormatResult {
        var i: usize = 0;
        while (i < tokens.len) {
            const token = tokens[i];
            if (token == .EOF or token == .EndOfFile) break;

            // Check for trailing comma patterns and format multi-line
            if (token == .LeftBrace or token == .LeftBracket or token == .LeftParen) {
                const has_trailing_comma = self.hasTrailingComma(tokens, i);
                if (has_trailing_comma) {
                    i = try self.formatMultiLineList(tokens, i);
                } else {
                    try self.writeToken(token);
                    i += 1;
                }
            } else {
                try self.writeToken(token);
                i += 1;
            }
        }

        const formatted_code = try self.allocator.dupe(u8, self.output.items);
        return FormatResult{
            .formatted_code = formatted_code,
            .success = true,
            .error_message = null,
        };
    }

    fn formatProgram(self: *Formatter, program: *ast.Program) !void {
        for (program.statements.items) |stmt| {
            try self.formatStatement(stmt);
            try self.writeNewline();
        }
    }

    fn formatStatement(self: *Formatter, stmt: ast.Statement) !void {
        switch (stmt) {
            .function_declaration => |func| try self.formatFunctionDeclaration(func),
            .variable_declaration => |var_decl| try self.formatVariableDeclaration(var_decl),
            .if_statement => |if_stmt| try self.formatIfStatement(if_stmt),
            .while_statement => |while_stmt| try self.formatWhileStatement(while_stmt),
            .for_statement => |for_stmt| try self.formatForStatement(for_stmt),
            .return_statement => |ret_stmt| try self.formatReturnStatement(ret_stmt),
            .expression_statement => |expr| try self.formatExpression(expr),
            .struct_declaration => |struct_decl| try self.formatStructDeclaration(struct_decl),
            .type_declaration => |type_decl| try self.formatTypeDeclaration(type_decl),
            .import_statement => |import_stmt| try self.formatImportStatement(import_stmt),
            .block_statement => |block| try self.formatBlockStatement(block),
        }
    }

    fn formatFunctionDeclaration(self: *Formatter, func: *ast.FunctionDeclaration) !void {
        try self.writeIndent();
        if (func.is_pub) {
            try self.writeString("pub ");
        }
        try self.writeString(func.name.literal);
        try self.writeString(" :: fn");

        // Check if parameters should be formatted multi-line
        const should_format_multiline = func.parameters.items.len > self.options.trailing_comma_threshold;

        if (should_format_multiline) {
            // Multi-line parameter format
            try self.writeString("(");
            try self.writeNewline();
            self.current_indent += 1;

            for (func.parameters.items, 0..) |param, i| {
                try self.writeIndent();
                try self.writeString(param.name.literal);
                try self.writeString(": ");
                try self.formatTypeExpression(param.type_expr);
                try self.writeString(",");
                if (i < func.parameters.items.len - 1) {
                    try self.writeNewline();
                }
            }

            self.current_indent -= 1;
            try self.writeNewline();
            try self.writeIndent();
            try self.writeString(")");
        } else {
            // Single-line parameter format
            try self.writeString("(");
            for (func.parameters.items, 0..) |param, i| {
                if (i > 0) try self.writeString(", ");
                try self.writeString(param.name.literal);
                try self.writeString(": ");
                try self.formatTypeExpression(param.type_expr);
            }
            try self.writeString(")");
        }

        if (func.return_type) |ret_type| {
            try self.writeString(" -> ");
            try self.formatTypeExpression(ret_type);
        }

        try self.writeString(" ");
        try self.formatBlockStatement(func.body);
    }

    fn formatVariableDeclaration(self: *Formatter, var_decl: *ast.VariableDeclaration) !void {
        try self.writeIndent();
        try self.writeString(var_decl.name.literal);

        if (var_decl.is_mutable) {
            if (var_decl.type_annotation) |type_ann| {
                try self.writeString(" : ");
                try self.formatTypeExpression(type_ann);
                try self.writeString(" = ");
            } else {
                try self.writeString(" := ");
            }
        } else {
            if (var_decl.type_annotation) |type_ann| {
                try self.writeString(" : ");
                try self.formatTypeExpression(type_ann);
                try self.writeString(" : ");
            } else {
                try self.writeString(" :: ");
            }
        }

        if (var_decl.initializer) |initializer| {
            try self.formatExpression(initializer);
        }
    }

    fn formatIfStatement(self: *Formatter, if_stmt: *ast.IfStatement) !void {
        try self.writeIndent();
        try self.writeString("if ");
        try self.formatExpression(if_stmt.condition);
        try self.writeString(" ");
        try self.formatBlockStatement(if_stmt.then_block);

        if (if_stmt.else_block) |else_block| {
            try self.writeString(" else ");
            switch (else_block.*) {
                .if_statement => |nested_if| try self.formatIfStatement(nested_if),
                .block_statement => |block| try self.formatBlockStatement(block),
                else => try self.formatStatement(else_block.*),
            }
        }
    }

    fn formatWhileStatement(self: *Formatter, while_stmt: *ast.WhileStatement) !void {
        try self.writeIndent();
        try self.writeString("while ");
        try self.formatExpression(while_stmt.condition);
        try self.writeString(" ");
        try self.formatBlockStatement(while_stmt.body);
    }

    fn formatForStatement(self: *Formatter, for_stmt: *ast.ForStatement) !void {
        try self.writeIndent();
        try self.writeString("for (");
        try self.formatExpression(for_stmt.iterable);
        try self.writeString(") |");
        try self.writeString(for_stmt.iterator.literal);
        try self.writeString("| ");
        try self.formatBlockStatement(for_stmt.body);
    }

    fn formatReturnStatement(self: *Formatter, ret_stmt: *ast.ReturnStatement) !void {
        try self.writeIndent();
        try self.writeString("return");
        if (ret_stmt.value) |value| {
            try self.writeString(" ");
            try self.formatExpression(value);
        }
    }

    fn formatStructDeclaration(self: *Formatter, struct_decl: *ast.StructDeclaration) !void {
        try self.writeIndent();
        try self.writeString(struct_decl.name.literal);
        try self.writeString(" :: struct {");
        try self.writeNewline();

        self.current_indent += 1;
        for (struct_decl.fields.items) |field| {
            try self.writeIndent();
            try self.writeString(field.name.literal);
            try self.writeString(": ");
            try self.formatTypeExpression(field.type_expr);
            try self.writeString(",");
            try self.writeNewline();
        }
        self.current_indent -= 1;

        try self.writeIndent();
        try self.writeString("}");
    }

    fn formatTypeDeclaration(self: *Formatter, type_decl: *ast.TypeDeclaration) !void {
        try self.writeIndent();
        try self.writeString(type_decl.name.literal);
        try self.writeString(" :: ");
        try self.formatTypeExpression(type_decl.type_expr);
    }

    fn formatImportStatement(self: *Formatter, import_stmt: *ast.ImportStatement) !void {
        try self.writeIndent();
        try self.writeString(import_stmt.name);
        try self.writeString(" :: @import(\"");
        try self.writeString(import_stmt.module_path.literal);
        try self.writeString("\")");
    }

    fn formatBlockStatement(self: *Formatter, block: *ast.BlockStatement) !void {
        try self.writeString("{");
        if (block.statements.items.len > 0) {
            try self.writeNewline();
            self.current_indent += 1;

            for (block.statements.items, 0..) |stmt, i| {
                if (i > 0) try self.writeNewline();
                try self.formatStatement(stmt);
            }

            self.current_indent -= 1;
            try self.writeNewline();
            try self.writeIndent();
        }
        try self.writeString("}");
    }

    fn formatExpression(self: *Formatter, expr: ast.Expression) !void {
        switch (expr) {
            .literal => |lit| try self.formatLiteral(lit),
            .identifier => |id| try self.writeString(id.literal),
            .binary_op => |bin_op| try self.formatBinaryOperation(bin_op),
            .unary_op => |un_op| try self.formatUnaryOperation(un_op),
            .call => |call| try self.formatCallExpression(call),
            .member_access => |member| try self.formatMemberAccess(member),
            .array_access => |arr| try self.formatArrayAccess(arr),
            .array_literal => |arr_lit| try self.formatArrayLiteral(arr_lit),
            .struct_literal => |struct_lit| try self.formatStructLiteral(struct_lit),
            .anonymous_struct => |anon| try self.formatAnonymousStruct(anon),
            .range => |range| try self.formatRangeExpression(range),
        }
    }

    fn formatLiteral(self: *Formatter, literal: *ast.Literal) !void {
        switch (literal.*) {
            .string => |str| {
                try self.writeString("\"");
                try self.writeString(str.literal);
                try self.writeString("\"");
            },
            .number => |num| try self.writeString(num.literal),
            .boolean => |b| try self.writeString(if (b.value) "true" else "false"),
        }
    }

    fn formatBinaryOperation(self: *Formatter, bin_op: *ast.BinaryOperation) !void {
        try self.formatExpression(bin_op.left.*);
        try self.writeString(" ");
        try self.writeString(bin_op.operator.literal);
        try self.writeString(" ");
        try self.formatExpression(bin_op.right.*);
    }

    fn formatUnaryOperation(self: *Formatter, un_op: *ast.UnaryOperation) !void {
        try self.writeString(un_op.operator.literal);
        try self.formatExpression(un_op.operand.*);
    }

    fn formatCallExpression(self: *Formatter, call: *ast.CallExpression) !void {
        try self.formatExpression(call.callee.*);
        try self.writeString("(");

        for (call.arguments.items, 0..) |arg, i| {
            if (i > 0) try self.writeString(", ");
            try self.formatExpression(arg);
        }

        try self.writeString(")");
    }

    fn formatMemberAccess(self: *Formatter, member: *ast.MemberAccess) !void {
        try self.formatExpression(member.object.*);
        try self.writeString(".");
        try self.writeString(member.member.literal);
    }

    fn formatArrayAccess(self: *Formatter, arr: *ast.ArrayAccess) !void {
        try self.formatExpression(arr.array.*);
        try self.writeString("[");
        try self.formatExpression(arr.index.*);
        try self.writeString("]");
    }

    fn formatArrayLiteral(self: *Formatter, arr_lit: *ast.ArrayLiteral) !void {
        // Check if we should format multi-line
        const should_format_multiline = arr_lit.elements.items.len > self.options.trailing_comma_threshold;

        if (should_format_multiline) {
            // Multi-line format
            try self.writeString("[");
            try self.writeNewline();
            self.current_indent += 1;

            for (arr_lit.elements.items, 0..) |elem, i| {
                try self.writeIndent();
                try self.formatExpression(elem);
                try self.writeString(",");
                if (i < arr_lit.elements.items.len - 1) {
                    try self.writeNewline();
                }
            }

            self.current_indent -= 1;
            try self.writeNewline();
            try self.writeIndent();
            try self.writeString("]");
        } else {
            // Single-line format
            try self.writeString("[");
            for (arr_lit.elements.items, 0..) |elem, i| {
                if (i > 0) try self.writeString(", ");
                try self.formatExpression(elem);
            }
            try self.writeString("]");
        }
    }

    fn formatStructLiteral(self: *Formatter, struct_lit: *ast.StructLiteral) !void {
        try self.writeString(struct_lit.name.literal);

        // Check if we should format multi-line (trailing comma indicates this)
        const should_format_multiline = struct_lit.fields.items.len > self.options.trailing_comma_threshold;

        if (should_format_multiline) {
            // Multi-line format
            try self.writeString(" {");
            try self.writeNewline();
            self.current_indent += 1;

            for (struct_lit.fields.items, 0..) |field, i| {
                try self.writeIndent();
                try self.writeString(field.name.literal);
                try self.writeString(": ");
                try self.formatExpression(field.value);
                try self.writeString(",");
                if (i < struct_lit.fields.items.len - 1) {
                    try self.writeNewline();
                }
            }

            self.current_indent -= 1;
            try self.writeNewline();
            try self.writeIndent();
            try self.writeString("}");
        } else {
            // Single-line format
            try self.writeString(" { ");
            for (struct_lit.fields.items, 0..) |field, i| {
                if (i > 0) try self.writeString(", ");
                try self.writeString(field.name.literal);
                try self.writeString(": ");
                try self.formatExpression(field.value);
            }
            try self.writeString(" }");
        }
    }

    fn formatAnonymousStruct(self: *Formatter, anon: *ast.AnonymousStruct) !void {
        try self.writeString(".{");

        for (anon.values.items, 0..) |value, i| {
            if (i > 0) try self.writeString(", ");
            try self.formatExpression(value);
        }

        try self.writeString("}");
    }

    fn formatRangeExpression(self: *Formatter, range: *ast.RangeExpression) !void {
        try self.formatExpression(range.start.*);
        try self.writeString(range.operator.literal);
        try self.formatExpression(range.end.*);
    }

    fn formatTypeExpression(self: *Formatter, type_expr: ast.TypeExpression) !void {
        switch (type_expr) {
            .identifier => |id| try self.writeString(id.literal),
            .array_type => |arr_type| {
                try self.writeString("[");
                if (arr_type.size) |size| {
                    try self.formatExpression(size.*);
                }
                try self.writeString("]");
                try self.formatTypeExpression(arr_type.element_type.*);
            },
            .pointer_type => |ptr_type| {
                try self.writeString("*");
                try self.formatTypeExpression(ptr_type.pointed_type.*);
            },
            .optional_type => |opt_type| {
                try self.writeString("?");
                try self.formatTypeExpression(opt_type.inner_type.*);
            },
        }
    }

    fn writeToken(self: *Formatter, token: Token) !void {
        switch (token) {
            .Newline => {
                if (self.options.preserve_blank_lines) {
                    if (self.last_token_was_newline) {
                        self.consecutive_blank_lines += 1;
                        if (self.consecutive_blank_lines <= self.options.max_blank_lines) {
                            try self.writeNewline();
                        }
                    } else {
                        try self.writeNewline();
                        self.consecutive_blank_lines = 1;
                    }
                } else {
                    if (!self.last_token_was_newline) {
                        try self.writeNewline();
                    }
                }
                self.last_token_was_newline = true;
            },
            .Whitespace => {
                // Skip whitespace in token-based formatting - spacing is handled by needsSpaceBefore
            },
            .StringLiteral => |t| {
                // String literals need quotes around them
                try self.writeString("\"");
                try self.writeString(t.value);
                try self.writeString("\"");
                self.last_token_was_newline = false;
                self.consecutive_blank_lines = 0;
            },
            else => {
                if (self.needsSpaceBefore(token)) {
                    try self.writeString(" ");
                }
                const literal = self.getTokenLiteral(token);
                if (literal.len > 0) { // Only write non-empty literals
                    try self.writeString(literal);
                }
                self.last_token_was_newline = false;
                self.consecutive_blank_lines = 0;
            },
        }
    }

    fn needsSpaceBefore(self: *Formatter, token: Token) bool {
        _ = self;
        return switch (token) {
            // No space before these punctuation marks
            .LeftParen, .RightParen, .LeftBracket, .RightBracket, .LeftBrace, .RightBrace, .Semicolon, .Dot => false,

            // Space before commas in some contexts, but generally no
            .Comma => false,

            // Space before most operators
            .Plus, .PlusPlus, .Minus, .Asterisk, .Slash, .Modulo, .Remainder, .EqualEqual, .NotEqual, .LessThan, .GreaterThan, .LessThanEqual, .GreaterThanEqual, .And, .Or => true,

            // Space before assignment operators
            .Assignment, .ColonEquals, .DoubleColon => true,

            // Space before keywords
            .If, .Else, .For, .While, .Return, .Fn, .Struct, .Pub => true,

            // Space before identifiers and literals in most contexts
            .Identifier, .StringLiteral, .IntegerLiteral, .FloatLiteral => true,

            // No space for most other tokens
            else => false,
        };
    }

    fn writeIndent(self: *Formatter) !void {
        if (self.options.use_tabs) {
            for (0..self.current_indent) |_| {
                try self.output.append('\t');
            }
        } else {
            for (0..self.current_indent * self.options.indent_size) |_| {
                try self.output.append(' ');
            }
        }
    }

    fn writeString(self: *Formatter, str: []const u8) !void {
        try self.output.appendSlice(str);
    }

    fn writeNewline(self: *Formatter) !void {
        try self.output.append('\n');
        self.last_token_was_newline = true;
        self.consecutive_blank_lines = 0;
    }

    fn hasTrailingComma(self: *Formatter, tokens: []Token, start_idx: usize) bool {
        _ = self;

        const open_token = tokens[start_idx];
        const close_type: std.meta.Tag(Token) = switch (open_token) {
            .LeftBrace => .RightBrace,
            .LeftBracket => .RightBracket,
            .LeftParen => .RightParen,
            else => return false,
        };

        var depth: u32 = 0;
        var i = start_idx;

        while (i < tokens.len) : (i += 1) {
            const token = tokens[i];

            if (std.meta.activeTag(token) == std.meta.activeTag(open_token)) {
                depth += 1;
            } else if (std.meta.activeTag(token) == close_type) {
                depth -= 1;
                if (depth == 0) {
                    // Found matching close token, check for trailing comma before it
                    var j = i;
                    while (j > start_idx) {
                        j -= 1;
                        const prev_token = tokens[j];
                        if (prev_token == .Comma) {
                            // Found a comma, check if there's only whitespace between comma and close
                            var k = j + 1;
                            while (k < i) {
                                if (tokens[k] != .Whitespace and tokens[k] != .Newline) {
                                    return false; // Non-whitespace found, not a trailing comma
                                }
                                k += 1;
                            }
                            return true; // Trailing comma found
                        } else if (prev_token != .Whitespace and prev_token != .Newline) {
                            return false; // Non-whitespace/comma found
                        }
                    }
                    return false;
                }
            }
        }

        return false;
    }

    fn formatFromAST(self: *Formatter, root_node: ast.NodeId, arena: *ast.AstArena) !FormatResult {
        // Format the AST starting from the root (treat root as special case)
        try self.formatASTNodeInternal(root_node, arena, true);

        const formatted_code = try self.allocator.dupe(u8, self.output.items);
        return FormatResult{
            .formatted_code = formatted_code,
            .success = true,
            .error_message = null,
        };
    }

    fn formatASTNode(self: *Formatter, node_id: ast.NodeId, arena: *ast.AstArena) !void {
        try self.formatASTNodeInternal(node_id, arena, false);
    }

    fn formatASTNodeInternal(self: *Formatter, node_id: ast.NodeId, arena: *ast.AstArena, is_root: bool) !void {
        const node = arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .block => |block| {
                if (is_root) {
                    // Root program block: don't add braces, just format statements
                    for (block.statements.items) |stmt_id| {
                        try self.formatASTNodeInternal(stmt_id, arena, false);
                        try self.writeNewline();
                    }
                } else {
                    // Regular block: add braces and indentation
                    try self.writeString("{");
                    if (block.statements.items.len > 0) {
                        try self.writeNewline();
                        self.current_indent += 1;
                        for (block.statements.items) |stmt_id| {
                            try self.formatASTNodeInternal(stmt_id, arena, false);
                            try self.writeNewline();
                        }
                        self.current_indent -= 1;
                        try self.writeIndent();
                    }
                    try self.writeString("}");
                }
            },
            .var_decl => |var_decl| {
                try self.writeIndent();
                try self.writeString(var_decl.name);

                if (var_decl.is_mutable) {
                    if (var_decl.type_annotation) |type_node| {
                        try self.writeString(" : ");
                        try self.formatASTNodeInternal(type_node, arena, false);
                        try self.writeString(" = ");
                    } else {
                        try self.writeString(" := ");
                    }
                } else {
                    if (var_decl.type_annotation) |type_node| {
                        try self.writeString(" : ");
                        try self.formatASTNodeInternal(type_node, arena, false);
                        try self.writeString(" : ");
                    } else {
                        try self.writeString(" :: ");
                    }
                }

                if (var_decl.initializer) |init_id| {
                    try self.formatASTNodeInternal(init_id, arena, false);
                }
            },
            .function_decl => |func_decl| {
                try self.writeIndent();
                try self.writeString(func_decl.name);
                try self.writeString(" :: fn(");

                for (func_decl.params.items, 0..) |param, i| {
                    if (i > 0) try self.writeString(", ");
                    try self.writeString(param.name);
                    try self.writeString(": ");
                    if (param.type_annotation) |type_node| {
                        try self.formatASTNodeInternal(type_node, arena, false);
                    }
                }

                try self.writeString(")");
                if (func_decl.return_type) |ret_type| {
                    try self.writeString(" ");
                    try self.formatASTNodeInternal(ret_type, arena, false);
                }
                try self.writeString(" ");
                try self.formatASTNodeInternal(func_decl.body, arena, false);
            },
            .binary_expr => |binary| {
                try self.formatASTNodeInternal(binary.left, arena, false);
                try self.writeString(" ");
                try self.writeString(binary.op.toString());
                try self.writeString(" ");
                try self.formatASTNodeInternal(binary.right, arena, false);
            },
            .unary_expr => |unary| {
                try self.writeString(unary.op.toString());
                try self.formatASTNodeInternal(unary.operand, arena, false);
            },
            .call_expr => |call| {
                try self.formatASTNodeInternal(call.callee, arena, false);
                try self.writeString("(");
                for (call.args.items, 0..) |arg_id, i| {
                    if (i > 0) try self.writeString(", ");
                    try self.formatASTNodeInternal(arg_id, arena, false);
                }
                try self.writeString(")");
            },
            .literal => |literal| {
                const literal_str = try literal.toString(self.allocator);
                defer self.allocator.free(literal_str);
                try self.writeString(literal_str);
            },
            .identifier => |ident| {
                try self.writeString(ident.name);
            },
            .struct_decl => |struct_decl| {
                try self.writeIndent();
                try self.writeString(struct_decl.name);
                try self.writeString(" :: struct {");
                try self.writeNewline();

                self.current_indent += 1;
                for (struct_decl.fields.items) |field| {
                    try self.writeIndent();
                    try self.writeString(field.name);
                    try self.writeString(": ");
                    if (field.type_annotation) |type_id| {
                        try self.formatASTNodeInternal(type_id, arena, false);
                    }
                    try self.writeString(",");
                    try self.writeNewline();
                }
                self.current_indent -= 1;
                try self.writeIndent();
                try self.writeString("}");
            },
            .enum_decl => |enum_decl| {
                try self.writeIndent();
                try self.writeString(enum_decl.name);
                try self.writeString(" :: enum {");
                try self.writeNewline();

                self.current_indent += 1;
                for (enum_decl.members.items) |member| {
                    try self.writeIndent();
                    try self.writeString(member.name);
                    if (member.value) |value_id| {
                        try self.writeString(" = ");
                        try self.formatASTNodeInternal(value_id, arena, false);
                    }
                    try self.writeString(",");
                    try self.writeNewline();
                }
                self.current_indent -= 1;
                try self.writeIndent();
                try self.writeString("}");
            },
            .struct_type_expr => |struct_type| {
                try self.writeString("struct {");
                try self.writeNewline();

                self.current_indent += 1;
                for (struct_type.fields.items) |field| {
                    try self.writeIndent();
                    try self.writeString(field.name);
                    try self.writeString(": ");
                    if (field.type_annotation) |type_id| {
                        try self.formatASTNodeInternal(type_id, arena, false);
                    }
                    try self.writeString(",");
                    try self.writeNewline();
                }
                self.current_indent -= 1;
                try self.writeIndent();
                try self.writeString("}");
            },
            .return_stmt => |return_stmt| {
                try self.writeIndent();
                try self.writeString("return");
                if (return_stmt.value) |value_id| {
                    try self.writeString(" ");
                    try self.formatASTNodeInternal(value_id, arena, false);
                }
            },
            .import_decl => |import_decl| {
                try self.writeIndent();
                try self.writeString("@import(\"");
                try self.writeString(import_decl.module_path);
                try self.writeString("\")");
            },
            .member_expr => |member| {
                try self.formatASTNodeInternal(member.object, arena, false);
                try self.writeString(".");
                try self.writeString(member.field);
            },
            .array_init => |array_init| {
                try self.writeString("[");
                for (array_init.elements.items, 0..) |elem_id, i| {
                    if (i > 0) try self.writeString(", ");
                    try self.formatASTNodeInternal(elem_id, arena, false);
                }
                try self.writeString("]");
            },
            .if_expr => |if_expr| {
                try self.writeString("if ");
                try self.formatASTNodeInternal(if_expr.condition, arena, false);
                try self.writeString(" ");
                try self.formatASTNodeInternal(if_expr.then_branch, arena, false);
                if (if_expr.else_branch) |else_branch| {
                    try self.writeString(" else ");
                    try self.formatASTNodeInternal(else_branch, arena, false);
                }
            },
            .while_expr => |while_expr| {
                try self.writeString("while ");
                try self.formatASTNodeInternal(while_expr.condition, arena, false);
                try self.writeString(" ");
                try self.formatASTNodeInternal(while_expr.body, arena, false);
            },
            .for_expr => |for_expr| {
                try self.writeString("for (");
                try self.formatASTNodeInternal(for_expr.iterable, arena, false);
                try self.writeString(") |");
                for (for_expr.captures.items, 0..) |capture, i| {
                    if (i > 0) try self.writeString(", ");
                    if (std.mem.eql(u8, capture.name, "_")) {
                        try self.writeString("_");
                    } else {
                        try self.writeString(capture.name);
                    }
                }
                try self.writeString("| ");
                try self.formatASTNodeInternal(for_expr.body, arena, false);
            },
            .index_expr => |index_expr| {
                try self.formatASTNodeInternal(index_expr.object, arena, false);
                try self.writeString("[");
                try self.formatASTNodeInternal(index_expr.index, arena, false);
                try self.writeString("]");
            },
            .range_expr => |range_expr| {
                if (range_expr.start) |start| {
                    try self.formatASTNodeInternal(start, arena, false);
                }
                if (range_expr.inclusive) {
                    try self.writeString("..=");
                } else {
                    try self.writeString("..<");
                }
                if (range_expr.end) |end| {
                    try self.formatASTNodeInternal(end, arena, false);
                }
            },
            .struct_init => |struct_init| {
                if (struct_init.type_name) |type_name| {
                    try self.writeString(type_name);
                }
                try self.writeString(" {");
                if (struct_init.fields.items.len > 0) {
                    try self.writeNewline();
                    self.current_indent += 1;
                    for (struct_init.fields.items, 0..) |field, i| {
                        if (i > 0) try self.writeNewline();
                        try self.writeIndent();
                        try self.writeString(field.name);
                        try self.writeString(": ");
                        try self.formatASTNodeInternal(field.value, arena, false);
                        try self.writeString(",");
                    }
                    self.current_indent -= 1;
                    try self.writeNewline();
                    try self.writeIndent();
                }
                try self.writeString("}");
            },
            .match_expr => |match_expr| {
                try self.writeString("match ");
                try self.formatASTNodeInternal(match_expr.expression, arena, false);
                try self.writeString(" {");
                try self.writeNewline();
                self.current_indent += 1;
                for (match_expr.arms.items, 0..) |arm, i| {
                    if (i > 0) try self.writeNewline();
                    try self.writeIndent();
                    // Format pattern (simplified - would need full pattern formatter)
                    try self.writeString("_ => ");
                    try self.formatASTNodeInternal(arm.body, arena, false);
                    try self.writeString(",");
                }
                self.current_indent -= 1;
                try self.writeNewline();
                try self.writeIndent();
                try self.writeString("}");
            },
            .type_decl => |type_decl| {
                try self.writeIndent();
                try self.writeString(type_decl.name);
                try self.writeString(" :: ");
                try self.formatASTNodeInternal(type_decl.type_expr, arena, false);
            },
            .type_expr => |type_expr| {
                try self.formatASTNodeInternal(type_expr.base_type, arena, false);
            },
            .generic_type_expr => |generic_type| {
                try self.formatASTNodeInternal(generic_type.base_type, arena, false);
                try self.writeString("(");
                for (generic_type.type_params.items, 0..) |param_id, i| {
                    if (i > 0) try self.writeString(", ");
                    try self.formatASTNodeInternal(param_id, arena, false);
                }
                try self.writeString(")");
            },
            .extern_fn_decl => |extern_fn| {
                try self.writeIndent();
                try self.writeString("extern ");
                try self.writeString(extern_fn.name);
                try self.writeString(" :: fn(");
                for (extern_fn.params.items, 0..) |param, i| {
                    if (i > 0) try self.writeString(", ");
                    try self.writeString(param.name);
                    try self.writeString(": ");
                    if (param.type_annotation) |type_node| {
                        try self.formatASTNodeInternal(type_node, arena, false);
                    }
                }
                try self.writeString(")");
                if (extern_fn.return_type) |ret_type| {
                    try self.writeString(" ");
                    try self.formatASTNodeInternal(ret_type, arena, false);
                }
                try self.writeString(" ");
                try self.formatASTNodeInternal(extern_fn.compile_time_body, arena, false);
            },
            .break_stmt => {
                try self.writeIndent();
                try self.writeString("break");
            },
            .continue_stmt => {
                try self.writeIndent();
                try self.writeString("continue");
            },
            .match_compile_expr => |match_compile| {
                try self.writeString("@match_compile(");
                try self.formatASTNodeInternal(match_compile.target_expr, arena, false);
                try self.writeString(") {");
                try self.writeNewline();
                self.current_indent += 1;
                for (match_compile.arms.items, 0..) |arm, i| {
                    if (i > 0) try self.writeNewline();
                    try self.writeIndent();
                    try self.writeString(".");
                    try self.writeString(arm.target.toString());
                    try self.writeString(" => ");
                    try self.formatASTNodeInternal(arm.body, arena, false);
                    try self.writeString(",");
                }
                self.current_indent -= 1;
                try self.writeNewline();
                try self.writeIndent();
                try self.writeString("}");
            },
            .compile_target_expr => {
                try self.writeString("@compile.target");
            },
            .compile_insert_expr => |compile_insert| {
                try self.writeString("@compile.insert(\"");
                try self.writeString(compile_insert.code);
                try self.writeString("\")");
            },
            .error_node => |error_node| {
                try self.writeString("/* ERROR: ");
                try self.writeString(error_node.message);
                try self.writeString(" */");
            },
        }
    }

    fn formatMultiLineList(self: *Formatter, tokens: []Token, start_idx: usize) !usize {
        const open_token = tokens[start_idx];
        const close_type: std.meta.Tag(Token) = switch (open_token) {
            .LeftBrace => .RightBrace,
            .LeftBracket => .RightBracket,
            .LeftParen => .RightParen,
            else => return start_idx + 1,
        };

        try self.writeToken(open_token);
        try self.writeNewline();
        self.current_indent += 1;

        var depth: u32 = 0;
        var i = start_idx;
        var current_line_has_content = false;

        while (i < tokens.len) : (i += 1) {
            const token = tokens[i];

            if (std.meta.activeTag(token) == std.meta.activeTag(open_token)) {
                depth += 1;
                if (depth > 1) {
                    try self.writeToken(token);
                }
            } else if (std.meta.activeTag(token) == close_type) {
                depth -= 1;
                if (depth == 0) {
                    // End of our list
                    if (current_line_has_content) {
                        try self.writeNewline();
                    }
                    self.current_indent -= 1;
                    try self.writeIndent();
                    try self.writeToken(token);
                    return i + 1;
                } else {
                    try self.writeToken(token);
                }
            } else if (token == .Comma and depth == 1) {
                // This is a comma at our level
                try self.writeToken(token);
                try self.writeNewline();
                current_line_has_content = false;
            } else if (token == .Newline) {
                // Skip original newlines, we control them
            } else if (token == .Whitespace) {
                // Handle whitespace appropriately
                if (!current_line_has_content) {
                    // Start of line, add our indentation
                    try self.writeIndent();
                    current_line_has_content = true;
                }
                // Skip the whitespace, we handle spacing
            } else {
                // Regular token
                if (!current_line_has_content) {
                    try self.writeIndent();
                    current_line_has_content = true;
                }
                try self.writeToken(token);
            }
        }

        return i;
    }

    fn getTokenLiteral(self: *Formatter, token: Token) []const u8 {
        _ = self;
        return switch (token) {
            .Identifier => |t| t.value,
            // StringLiteral value doesn't include quotes, but we need them in formatted output
            .StringLiteral => |t| {
                // Allocate new string with quotes - this is handled in writeTokenWithQuotes
                return t.value;
            },
            .IntegerLiteral => |t| {
                // Convert the integer value back to string
                var buf: [64]u8 = undefined;
                return std.fmt.bufPrint(&buf, "{d}", .{t.value}) catch "0";
            },
            .FloatLiteral => |t| {
                // Convert the float value back to string
                var buf: [64]u8 = undefined;
                return std.fmt.bufPrint(&buf, "{d}", .{t.value}) catch "0.0";
            },
            .CharLiteral => |t| &[_]u8{t.char},
            .Comment => |t| t.value,
            .DocComment => |t| t.value,

            // Keywords
            .For => "for",
            .If => "if",
            .Else => "else",
            .Match => "match",
            .Fn => "fn",
            .Let => "let", // This might not exist in Howl
            .Mut => "mut", // This might not exist in Howl
            .While => "while",
            .Return => "return",
            .Import => "@import",
            .Struct => "struct",
            .Enum => "enum",
            .Type => "type", // This might not exist in Howl
            .True => "true",
            .False => "false",
            .Pub => "pub",
            .Void => "void",

            // Types
            .I8 => "i8",
            .I16 => "i16",
            .I32 => "i32",
            .I64 => "i64",
            .I128 => "i128",
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .U128 => "u128",
            .F8 => "f8",
            .F16 => "f16",
            .F32 => "f32",
            .F64 => "f64",
            .Str => "Str",
            .StrB => "StrB",
            .Bool => "bool",

            // Operators
            .Plus => "+",
            .PlusPlus => "++",
            .Minus => "-",
            .Asterisk => "*",
            .Slash => "/",
            .Modulo => "mod",
            .Remainder => "rem",
            .EqualEqual => "==",
            .NotEqual => "!=",
            .Assignment => "=",
            .LessThan => "<",
            .GreaterThan => ">",
            .LessThanEqual => "<=",
            .GreaterThanEqual => ">=",
            .And => "and",
            .Or => "or",
            .Not => "not",

            // Punctuation
            .LeftParen => "(",
            .RightParen => ")",
            .LeftBracket => "[",
            .RightBracket => "]",
            .LeftBrace => "{",
            .RightBrace => "}",
            .Semicolon => ";",
            .Colon => ":",
            .DoubleColon => "::",
            .ColonEquals => ":=",
            .Comma => ",",
            .Dot => ".",
            .DotDot => "..",
            .DotDotDot => "...",
            .Arrow => "->",
            .DoubleArrow => "=>",
            .Pipe => "|",
            .PipeGreaterThan => "|>",
            .Underscore => "_",
            .QuestionMark => "?",

            else => "",
        };
    }
};

pub fn formatCode(allocator: std.mem.Allocator, source_code: []const u8, options: FormatterOptions) !FormatResult {
    var formatter = Formatter.init(allocator, options);
    defer formatter.deinit();

    return formatter.format(source_code);
}
