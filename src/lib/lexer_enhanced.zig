const std = @import("std");
const Token = @import("./token.zig").Token;
const BaseToken = @import("./token.zig").BaseToken;
const ErrorSystem = @import("./error_system.zig");
const ErrorFormatter = @import("./error_formatter.zig");

const parseIntegerWithBase = @import("./lexer_numeric_parser.zig").parseIntegerWithBase;
const parseDecimalNumber = @import("./lexer_numeric_parser.zig").parseDecimalNumber;

const ErrorCollector = ErrorSystem.ErrorCollector;
const ErrorCode = ErrorSystem.ErrorCode;
const ErrorCategory = ErrorSystem.ErrorCategory;
const ErrorSeverity = ErrorSystem.ErrorSeverity;
const SourceSpan = ErrorSystem.SourceSpan;
const SourceMap = ErrorSystem.SourceMap;

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    files: std.StringHashMap(LexerFile),
    error_collector: ErrorCollector,
    source_maps: std.StringHashMap(SourceMap),

    pub fn init(allocator: std.mem.Allocator) Lexer {
        return Lexer{
            .allocator = allocator,
            .files = std.StringHashMap(LexerFile).init(allocator),
            .error_collector = ErrorCollector.init(allocator),
            .source_maps = std.StringHashMap(SourceMap).init(allocator),
        };
    }

    pub fn deinit(self: *Lexer) void {
        var file_iterator = self.files.iterator();
        while (file_iterator.next()) |entry| {
            entry.value_ptr.deinit();
            // Free the duplicated file path key
            self.allocator.free(entry.key_ptr.*);
        }
        self.files.deinit();

        var source_map_iterator = self.source_maps.iterator();
        while (source_map_iterator.next()) |entry| {
            entry.value_ptr.deinit();
            // Free the duplicated file path key
            self.allocator.free(entry.key_ptr.*);
        }
        self.source_maps.deinit();

        self.error_collector.deinit();
    }

    pub fn addFile(self: *Lexer, file_path: []const u8, file_content: []const u8) !void {
        // Create source map for error reporting
        const source_map = try SourceMap.init(self.allocator, file_content);
        try self.source_maps.put(try self.allocator.dupe(u8, file_path), source_map);

        // Create lexer file
        const lexer_file = LexerFile.init(self.allocator, self, file_path, file_content);
        try self.files.put(try self.allocator.dupe(u8, file_path), lexer_file);
    }

    pub fn tokenizeAll(self: *Lexer) !void {
        var iterator = self.files.iterator();
        while (iterator.next()) |entry| {
            try entry.value_ptr.tokenize();
        }
    }

    pub fn hasErrors(self: *const Lexer) bool {
        return self.error_collector.hasErrors();
    }

    pub fn printErrors(self: *const Lexer, writer: anytype) !void {
        const formatter = ErrorFormatter.init(self.allocator);
        try formatter.formatErrors(&self.error_collector, &self.source_maps, writer);
    }
};

/// Legacy error type for backward compatibility
pub const ErrorType = enum {
    CompileError,
    LexerError,
    ParserError,
    SemanticError,
    CompileExecutionError,
};

/// Legacy error struct for backward compatibility
pub const CompileError = struct {
    file_path: []const u8,
    message: []const u8,
    pos: usize,
    error_type: ErrorType,
};

pub const keywords = std.StaticStringMap(Token).initComptime(.{
    .{ "pub", Token{ .Pub = .{ .pos = 0 } } },
    .{ "for", Token{ .For = .{ .pos = 0 } } },
    .{ "if", Token{ .If = .{ .pos = 0 } } },
    .{ "else", Token{ .Else = .{ .pos = 0 } } },
    .{ "match", Token{ .Match = .{ .pos = 0 } } },
    .{ "i8", Token{ .I8 = .{ .pos = 0 } } },
    .{ "i16", Token{ .I16 = .{ .pos = 0 } } },
    .{ "i32", Token{ .I32 = .{ .pos = 0 } } },
    .{ "i64", Token{ .I64 = .{ .pos = 0 } } },
    .{ "i128", Token{ .I128 = .{ .pos = 0 } } },
    .{ "u8", Token{ .U8 = .{ .pos = 0 } } },
    .{ "u16", Token{ .U16 = .{ .pos = 0 } } },
    .{ "u32", Token{ .U32 = .{ .pos = 0 } } },
    .{ "u64", Token{ .U64 = .{ .pos = 0 } } },
    .{ "u128", Token{ .U128 = .{ .pos = 0 } } },
    .{ "f8", Token{ .F8 = .{ .pos = 0 } } },
    .{ "f16", Token{ .F16 = .{ .pos = 0 } } },
    .{ "f32", Token{ .F32 = .{ .pos = 0 } } },
    .{ "f64", Token{ .F64 = .{ .pos = 0 } } },
    .{ "str", Token{ .Str = .{ .pos = 0 } } },
    .{ "strb", Token{ .StrB = .{ .pos = 0 } } },
    .{ "bool", Token{ .Bool = .{ .pos = 0 } } },
    .{ "void", Token{ .Void = .{ .pos = 0 } } },
    .{ "Some", Token{ .Some = .{ .pos = 0 } } },
    .{ "None", Token{ .None = .{ .pos = 0 } } },
    .{ "Type", Token{ .Type = .{ .pos = 0 } } },
    .{ "error", Token{ .Error = .{ .pos = 0 } } },
    .{ "true", Token{ .True = .{ .pos = 0 } } },
    .{ "false", Token{ .False = .{ .pos = 0 } } },
    .{ "struct", Token{ .Struct = .{ .pos = 0 } } },
    .{ "enum", Token{ .Enum = .{ .pos = 0 } } },
    .{ "tag", Token{ .Tag = .{ .pos = 0 } } },
    .{ "fn", Token{ .Fn = .{ .pos = 0 } } },
    .{ "return", Token{ .Return = .{ .pos = 0 } } },
    .{ "try", Token{ .Try = .{ .pos = 0 } } },
    .{ "catch", Token{ .Catch = .{ .pos = 0 } } },
    .{ "and", Token{ .And = .{ .pos = 0 } } },
    .{ "or", Token{ .Or = .{ .pos = 0 } } },
    .{ "not", Token{ .Not = .{ .pos = 0 } } },
    .{ "bXor", Token{ .BitwiseXor = .{ .pos = 0 } } },
    .{ "bAnd", Token{ .BitwiseAnd = .{ .pos = 0 } } },
    .{ "bOr", Token{ .BitwiseOr = .{ .pos = 0 } } },
    .{ "bNot", Token{ .BitwiseNot = .{ .pos = 0 } } },
    .{ "bToggle", Token{ .BitwiseToggle = .{ .pos = 0 } } },
    .{ "bShiftLeft", Token{ .BitwiseShiftLeft = .{ .pos = 0 } } },
    .{ "bShiftRight", Token{ .BitwiseShiftRight = .{ .pos = 0 } } },
    .{ "bRotLeft", Token{ .BitwiseRotateLeft = .{ .pos = 0 } } },
    .{ "bRotRight", Token{ .BitwiseRotateRight = .{ .pos = 0 } } },
    .{ "mod", Token{ .Modulo = .{ .pos = 0 } } },
    .{ "rem", Token{ .Remainder = .{ .pos = 0 } } },
});

const TokenizeState = struct {
    current_pos: usize,
    next_pos: usize,
    next_next_pos: usize,
    char: u8,
    next_char: u8,
    next_next_char: u8,
};

pub const LexerFile = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,
    file_path: []const u8,
    file_content: []const u8,
    tokens: std.ArrayList(Token),
    tokenize_state: TokenizeState,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer, file_path: []const u8, file_content: []const u8) LexerFile {
        return LexerFile{
            .allocator = allocator,
            .lexer = lexer,
            .file_path = file_path,
            .file_content = file_content,
            .tokens = std.ArrayList(Token).init(allocator),
            .tokenize_state = TokenizeState{
                .current_pos = 0,
                .next_pos = 0,
                .next_next_pos = 0,
                .char = 0,
                .next_char = 0,
                .next_next_char = 0,
            },
        };
    }

    pub fn deinit(self: *LexerFile) void {
        // Free token contents before deinit
        for (self.tokens.items) |token| {
            switch (token) {
                .Identifier => |ident| {
                    self.allocator.free(ident.value);
                },
                .StringLiteral => |str| {
                    self.allocator.free(str.value);
                },
                else => {}, // Other tokens don't own memory
            }
        }
        self.tokens.deinit();
    }

    /// Report a lexer error with intelligent suggestions
    fn reportError(self: *LexerFile, code: ErrorCode, message: []const u8, pos: usize) !void {
        const source_map = self.lexer.source_maps.get(self.file_path) orelse return;
        const line_column = source_map.getLineColumn(pos);

        const span = SourceSpan.single(
            self.file_path,
            pos,
            line_column.line,
            line_column.column,
        );

        const err = try self.lexer.error_collector.createAndAddError(
            code,
            .lexer,
            .error_,
            message,
            span,
        );

        // Add context-specific suggestions
        switch (code) {
            .invalid_character => {
                const suggestions = [_][]const u8{
                    "check for invisible or non-ASCII characters",
                    "ensure proper encoding (UTF-8)",
                };
                try err.withSuggestions(self.allocator, &suggestions);
            },
            .unterminated_string => {
                const suggestions = [_][]const u8{
                    "add closing quote (\") to terminate string",
                    "check for unescaped quotes within the string",
                };
                try err.withSuggestions(self.allocator, &suggestions);
            },
            .invalid_number_format => {
                const suggestions = [_][]const u8{
                    "check number format (e.g., 123, 0x1F, 0b101)",
                    "ensure no invalid characters in number",
                };
                try err.withSuggestions(self.allocator, &suggestions);
            },
            .invalid_escape_sequence => {
                const suggestions = [_][]const u8{
                    "use valid escape sequences: \\n, \\t, \\r, \\\\, \\\"",
                    "check escape sequence syntax",
                };
                try err.withSuggestions(self.allocator, &suggestions);
            },
            else => {},
        }
    }

    pub fn advance(self: *LexerFile) void {
        const self_tokenize_state = &self.tokenize_state;

        // Advance position first
        self_tokenize_state.current_pos += 1;

        // If we're now at or past EOF, set everything to EOF
        if (self_tokenize_state.current_pos >= self.file_content.len) {
            self.tokenize_state.char = 0; // EOF
            self.tokenize_state.next_char = 0; // EOF
            self.tokenize_state.next_next_char = 0; // EOF
            return;
        }

        // Set current character
        self_tokenize_state.char = self.file_content[self_tokenize_state.current_pos];

        // Set next character
        if (self_tokenize_state.current_pos + 1 < self.file_content.len) {
            self_tokenize_state.next_pos = self_tokenize_state.current_pos + 1;
            self_tokenize_state.next_char = self.file_content[self_tokenize_state.current_pos + 1];
        } else {
            self_tokenize_state.next_pos = self.file_content.len;
            self_tokenize_state.next_char = 0; // EOF
        }

        // Set next-next character
        if (self_tokenize_state.current_pos + 2 < self.file_content.len) {
            self_tokenize_state.next_next_pos = self_tokenize_state.current_pos + 2;
            self_tokenize_state.next_next_char = self.file_content[self_tokenize_state.current_pos + 2];
        } else {
            self_tokenize_state.next_next_pos = self.file_content.len;
            self_tokenize_state.next_next_char = 0; // EOF
        }
    }

    pub fn tokenize(self: *LexerFile) !void {
        if (self.tokenize_state.current_pos > 0) {
            const message = "tokenize state already initialized, reset first";
            try self.reportError(.assertion_failed, message, 0);
            return;
        }

        if (self.file_content.len == 0) {
            try self.tokens.append(Token{ .StartOfFile = .{ .pos = 0 } });
            try self.tokens.append(Token{ .EOF = .{ .pos = 0 } });
            return;
        }

        self.tokenize_state.current_pos = 0;
        self.tokenize_state.char = if (self.file_content.len > 0) self.file_content[0] else 0;
        self.tokenize_state.next_pos = if (self.file_content.len > 1) 1 else 0;
        self.tokenize_state.next_char = if (self.file_content.len > 1) self.file_content[1] else 0;
        self.tokenize_state.next_next_pos = if (self.file_content.len > 2) 2 else 0;
        self.tokenize_state.next_next_char = if (self.file_content.len > 2) self.file_content[2] else 0;

        try self.tokens.append(Token{ .StartOfFile = .{ .pos = 0 } });
        const content_length = self.file_content.len;

        // Safety counter to prevent infinite loops
        var iteration_count: usize = 0;
        const max_iterations = content_length * 10; // Allow 10x file length iterations

        while (self.tokenize_state.current_pos < content_length) {
            iteration_count += 1;
            if (iteration_count > max_iterations) {
                const message = try std.fmt.allocPrint(self.allocator, "lexer stuck in infinite loop at position {d}", .{self.tokenize_state.current_pos});
                defer self.allocator.free(message);
                try self.reportError(.invalid_character, message, self.tokenize_state.current_pos);
                break;
            }

            const old_pos = self.tokenize_state.current_pos;

            switch (self.tokenize_state.char) {
                '\n' => {
                    try self.tokens.append(Token{ .Newline = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ' ', '\t', '\r' => {
                    try self.tokens.append(Token{ .Whitespace = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();

                    // advance while whitespace
                    while (self.tokenize_state.char == ' ' or self.tokenize_state.char == '\t' or self.tokenize_state.char == '\r') {
                        self.advance();
                    }
                },
                '0'...'9' => {
                    self.parseNumeric() catch |err| {
                        switch (err) {
                            error.InvalidNumberFormat => {
                                const message = "invalid number format";
                                try self.reportError(.invalid_number_format, message, self.tokenize_state.current_pos);
                                self.advance(); // Skip the invalid character and continue
                            },
                            else => return err,
                        }
                    };
                },
                '"' => {
                    // Handle string literals with better error reporting
                    const start_pos = self.tokenize_state.current_pos;
                    self.advance(); // skip opening quote

                    var string_content = std.ArrayList(u8).init(self.allocator);
                    defer string_content.deinit();

                    var terminated = false;
                    while (self.tokenize_state.char != 0) {
                        if (self.tokenize_state.char == '"') {
                            terminated = true;
                            break;
                        }

                        if (self.tokenize_state.char == '\\') {
                            // Handle escape sequences
                            self.advance();
                            if (self.tokenize_state.char == 0) {
                                break; // Unterminated string
                            }

                            const escaped_char = switch (self.tokenize_state.char) {
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                '\\' => '\\',
                                '"' => '"',
                                else => blk: {
                                    const message = try std.fmt.allocPrint(self.allocator, "invalid escape sequence '\\{c}'", .{self.tokenize_state.char});
                                    defer self.allocator.free(message);
                                    try self.reportError(.invalid_escape_sequence, message, self.tokenize_state.current_pos - 1);
                                    break :blk self.tokenize_state.char; // Use the character as-is
                                },
                            };
                            try string_content.append(escaped_char);
                        } else {
                            try string_content.append(self.tokenize_state.char);
                        }
                        self.advance();
                    }

                    if (!terminated) {
                        const message = "unterminated string literal";
                        try self.reportError(.unterminated_string, message, start_pos);
                    } else {
                        const owned_string = try string_content.toOwnedSlice();
                        try self.tokens.append(Token{ .StringLiteral = .{ .pos = start_pos, .value = owned_string } });
                        self.advance(); // skip closing quote
                    }
                },
                'a'...'z', 'A'...'Z', '_' => {
                    try self.parseIdentifierOrKeyword();
                },
                '/' => {
                    if (self.tokenize_state.next_char == '/') {
                        // Handle single-line comment
                        self.advance(); // skip first '/'
                        self.advance(); // skip second '/'

                        // Skip until end of line
                        while (self.tokenize_state.char != '\n' and self.tokenize_state.char != 0) {
                            self.advance();
                        }
                    } else if (self.tokenize_state.next_char == '*') {
                        // Handle multi-line comment
                        const start_pos = self.tokenize_state.current_pos;
                        self.advance(); // skip '/'
                        self.advance(); // skip '*'

                        var terminated = false;
                        while (self.tokenize_state.char != 0) {
                            if (self.tokenize_state.char == '*' and self.tokenize_state.next_char == '/') {
                                terminated = true;
                                self.advance(); // skip '*'
                                self.advance(); // skip '/'
                                break;
                            }
                            self.advance();
                        }

                        if (!terminated) {
                            const message = "unterminated block comment";
                            try self.reportError(.unterminated_string, message, start_pos);
                        }
                    } else {
                        // Division operator
                        try self.tokens.append(Token{ .Slash = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance();
                    }
                },
                // Add other token cases here...
                '+' => {
                    if (self.tokenize_state.next_char == '=') {
                        // Handle addition assignment operator (+=)
                        try self.tokens.append(Token{ .PlusAssign = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '+'
                        self.advance(); // skip '='
                    } else if (self.tokenize_state.next_char == '+') {
                        // Handle string concatenation operator (++)
                        try self.tokens.append(Token{ .PlusPlus = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip first '+'
                        self.advance(); // skip second '+'
                    } else {
                        try self.tokens.append(Token{ .Plus = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance();
                    }
                },
                '-' => {
                    if (self.tokenize_state.next_char == '=') {
                        // Handle subtraction assignment operator (-=)
                        try self.tokens.append(Token{ .MinusAssign = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '-'
                        self.advance(); // skip '='
                    } else {
                        try self.tokens.append(Token{ .Minus = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance();
                    }
                },
                '*' => {
                    try self.tokens.append(Token{ .Asterisk = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '(' => {
                    try self.tokens.append(Token{ .LeftParen = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ')' => {
                    try self.tokens.append(Token{ .RightParen = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '{' => {
                    try self.tokens.append(Token{ .LeftBrace = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '}' => {
                    try self.tokens.append(Token{ .RightBrace = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '[' => {
                    try self.tokens.append(Token{ .LeftBracket = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ']' => {
                    try self.tokens.append(Token{ .RightBracket = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '$' => {
                    try self.tokens.append(Token{ .Dollar = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ';' => {
                    try self.tokens.append(Token{ .Semicolon = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ',' => {
                    try self.tokens.append(Token{ .Comma = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '=' => {
                    if (self.tokenize_state.next_char == '=') {
                        try self.tokens.append(Token{ .DoubleEquals = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip second '='
                    } else if (self.tokenize_state.next_char == '>') {
                        try self.tokens.append(Token{ .DoubleArrow = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '>'
                    } else {
                        try self.tokens.append(Token{ .Equals = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                ':' => {
                    if (self.tokenize_state.next_char == ':') {
                        try self.tokens.append(Token{ .DoubleColon = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip second ':'
                    } else if (self.tokenize_state.next_char == '=') {
                        try self.tokens.append(Token{ .ColonEquals = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else {
                        try self.tokens.append(Token{ .Colon = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '!' => {
                    if (self.tokenize_state.next_char == '=') {
                        try self.tokens.append(Token{ .ExclamationEquals = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else {
                        try self.tokens.append(Token{ .Exclamation = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '@' => {
                    // Handle at sign (used for attributes or special tokens)
                    // Check if this is @import
                    if (self.tokenize_state.next_char == 'i' and
                        self.tokenize_state.current_pos + 6 < self.file_content.len and
                        std.mem.eql(u8, self.file_content[self.tokenize_state.current_pos + 1 .. self.tokenize_state.current_pos + 7], "import"))
                    {
                        // Skip @import
                        try self.tokens.append(Token{ .Import = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip @
                        // Skip "import"
                        for (0..6) |_| {
                            self.advance();
                        }
                    } else {
                        try self.tokens.append(Token{ .At = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance();
                    }
                },
                '|' => {
                    if (self.tokenize_state.next_char == '>') {
                        try self.tokens.append(Token{ .PipeGreaterThan = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '>'
                    } else {
                        try self.tokens.append(Token{ .Pipe = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '>' => {
                    if (self.tokenize_state.next_char == '=') {
                        try self.tokens.append(Token{ .GreaterThanEquals = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else {
                        try self.tokens.append(Token{ .GreaterThan = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '<' => {
                    if (self.tokenize_state.next_char == '=') {
                        try self.tokens.append(Token{ .LessThanEquals = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else {
                        try self.tokens.append(Token{ .LessThan = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '.' => {
                    if (self.tokenize_state.next_char == '.') {
                        if (self.tokenize_state.next_next_char == '.') {
                            // Handle ellipsis operator (...)
                            try self.tokens.append(Token{ .DotDotDot = .{ .pos = self.tokenize_state.current_pos } });
                            self.advance(); // skip second '.'
                            self.advance(); // skip third '.'
                        } else if (self.tokenize_state.next_next_char == '=') {
                            // Handle range operator (..=)
                            try self.tokens.append(Token{ .DotDotEquals = .{ .pos = self.tokenize_state.current_pos } });
                            self.advance(); // skip second '.'
                            self.advance(); // skip '='
                        } else if (self.tokenize_state.next_next_char == '<') {
                            // Handle range operator (..<)
                            try self.tokens.append(Token{ .DotDotLessThan = .{ .pos = self.tokenize_state.current_pos } });
                            self.advance(); // skip second '.'
                            self.advance(); // skip '<'
                        } else {
                            // Handle range operator (..)
                            try self.tokens.append(Token{ .DotDot = .{ .pos = self.tokenize_state.current_pos } });
                            self.advance(); // skip second '.'
                        }
                    } else {
                        // Handle dot operator (.)
                        try self.tokens.append(Token{ .Dot = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                else => {
                    // Invalid character
                    const message = try std.fmt.allocPrint(self.allocator, "invalid character '{c}' (ASCII {d})", .{ self.tokenize_state.char, self.tokenize_state.char });
                    defer self.allocator.free(message);
                    try self.reportError(.invalid_character, message, self.tokenize_state.current_pos);
                    self.advance(); // Skip invalid character and continue
                },
            }

            // Safety check: ensure we're making progress
            if (self.tokenize_state.current_pos == old_pos) {
                const message = try std.fmt.allocPrint(self.allocator, "lexer failed to advance from position {d} (char: '{c}' ASCII {d})", .{ old_pos, self.tokenize_state.char, self.tokenize_state.char });
                defer self.allocator.free(message);
                try self.reportError(.invalid_character, message, self.tokenize_state.current_pos);
                self.advance(); // Force advance to prevent infinite loop
            }
        }

        try self.tokens.append(Token{ .EOF = .{ .pos = content_length } });
    }

    fn parseNumeric(self: *LexerFile) !void {
        // Simplified numeric parsing with error handling
        const start_pos = self.tokenize_state.current_pos;

        // Basic integer parsing
        while (self.tokenize_state.char >= '0' and self.tokenize_state.char <= '9') {
            self.advance();
        }

        // Check for decimal point
        if (self.tokenize_state.char == '.' and self.tokenize_state.next_char >= '0' and self.tokenize_state.next_char <= '9') {
            self.advance(); // skip '.'
            while (self.tokenize_state.char >= '0' and self.tokenize_state.char <= '9') {
                self.advance();
            }

            // Parse as float
            const num_str = self.file_content[start_pos..self.tokenize_state.current_pos];
            const parsed_float = std.fmt.parseFloat(f64, num_str) catch {
                return error.InvalidNumberFormat;
            };
            try self.tokens.append(Token{ .FloatLiteral = .{ .pos = start_pos, .value = parsed_float } });
        } else {
            // Parse as integer
            const num_str = self.file_content[start_pos..self.tokenize_state.current_pos];
            const parsed_int = std.fmt.parseInt(i64, num_str, 10) catch {
                return error.InvalidNumberFormat;
            };
            try self.tokens.append(Token{ .IntegerLiteral = .{ .pos = start_pos, .value = parsed_int } });
        }
    }

    fn parseIdentifierOrKeyword(self: *LexerFile) !void {
        const start_pos = self.tokenize_state.current_pos;

        // Parse identifier characters
        while ((self.tokenize_state.char >= 'a' and self.tokenize_state.char <= 'z') or
            (self.tokenize_state.char >= 'A' and self.tokenize_state.char <= 'Z') or
            (self.tokenize_state.char >= '0' and self.tokenize_state.char <= '9') or
            self.tokenize_state.char == '_')
        {
            self.advance();
        }

        const identifier_str = self.file_content[start_pos..self.tokenize_state.current_pos];

        // Check if it's a keyword
        if (keywords.get(identifier_str)) |keyword_token| {
            var token = keyword_token;
            // Update position for the keyword token
            switch (token) {
                inline else => |*payload| payload.pos = start_pos,
            }
            try self.tokens.append(token);
        } else {
            // It's an identifier
            const owned_identifier = try self.allocator.dupe(u8, identifier_str);
            try self.tokens.append(Token{ .Identifier = .{ .pos = start_pos, .value = owned_identifier } });
        }
    }
};
