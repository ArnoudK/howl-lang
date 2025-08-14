const std = @import("std");
const Token = @import("./token.zig").Token;
const BaseToken = @import("./token.zig").BaseToken;

const parseIntegerWithBase = @import("./lexer_numeric_parser.zig").parseIntegerWithBase;
const parseDecimalNumber = @import("./lexer_numeric_parser.zig").parseDecimalNumber;

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    files: std.StringHashMap(LexerFile),

    pub fn init(allocator: std.mem.Allocator) Lexer {
        return Lexer{
            .allocator = allocator,
            .files = std.StringHashMap(LexerFile).init(allocator),
        };
    }
};

pub const ErrorType = enum {
    CompileError,
    LexerError,
    ParserError,
    SemanticError,
    CompileExecutionError,
};

pub const keywords = std.StaticStringMap(Token).initComptime(
    .{
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
        .{ "and", Token{ .And = .{ .pos = 0 } } },
        .{ "or", Token{ .Or = .{ .pos = 0 } } },
        .{ "not", Token{ .Not = .{ .pos = 0 } } },
        .{ "or", Token{ .Or = .{ .pos = 0 } } },
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
        .{ "let", Token{ .Let = .{ .pos = 0 } } },
        .{ "mut", Token{ .Mut = .{ .pos = 0 } } },
        .{ "return", Token{ .Return = .{ .pos = 0 } } },
        .{ "while", Token{ .While = .{ .pos = 0 } } },
    },
);

pub const CompileError = struct {
    file_path: []const u8,
    message: []const u8,
    pos: usize,
    error_type: ErrorType,
};

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
        self.tokens.deinit();
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
            return error.TokenizeStateAlreadyInitializedResetFirst;
        }
        self.tokenize_state.current_pos = 0;
        self.tokenize_state.char = if (self.file_content.len > 0) self.file_content[0] else 0;
        self.tokenize_state.next_pos = if (self.file_content.len > 1) 1 else 0;
        self.tokenize_state.next_char = if (self.file_content.len > 1) self.file_content[1] else 0;
        self.tokenize_state.next_next_pos = if (self.file_content.len > 2) 2 else 0;
        self.tokenize_state.next_next_char = if (self.file_content.len > 2) self.file_content[2] else 0;

        try self.tokens.append(Token{ .StartOfFile = .{ .pos = 0 } });
        const content_length = self.file_content.len;

        while (self.tokenize_state.current_pos < content_length) {
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
                    try self.parseNumeric();
                },
                '"' => {
                    // Handle string literals
                    const start_pos = self.tokenize_state.current_pos;
                    self.advance(); // skip opening quote
                    // get the end of the string literal and create a slice (we don't need to copy the string, just create a slice) (allow for escaped quotes)
                    var end_pos: usize = self.tokenize_state.current_pos;
                    while (self.tokenize_state.char != '"' and self.tokenize_state.char != 0) {
                        if (self.tokenize_state.char == '\\') {
                            // skip the next character (escaped character)
                            self.advance();
                        }
                        end_pos = self.tokenize_state.current_pos;
                        self.advance();
                    }
                    // If we reached the end of the file without finding a closing quote, it's an error
                    if (self.tokenize_state.char != '"') {
                        std.debug.print("Unterminated string literal at position {d}\n", .{start_pos});
                        return error.UnterminatedStringLiteral;
                    }
                    // Now we have the string literal from start_pos to end_pos
                    const string_literal = self.file_content[start_pos + 1 .. end_pos];
                    try self.tokens.append(Token{ .StringLiteral = .{ .pos = start_pos, .value = string_literal } });
                    self.advance(); // skip closing quote
                },
                '#' => {
                    try self.tokens.append(Token{ .Hash = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance(); // skip '#'
                },
                'a'...'z', 'A'...'Z' => {
                    // Handle identifiers and keywords (save it as a slice NO COPIES ALLOWED)
                    const start_pos = self.tokenize_state.current_pos;
                    while (self.tokenize_state.char == '_' or (self.tokenize_state.char >= 'a' and self.tokenize_state.char <= 'z') or (self.tokenize_state.char >= 'A' and self.tokenize_state.char <= 'Z') or (self.tokenize_state.char >= '0' and self.tokenize_state.char <= '9')) {
                        self.advance();
                    }
                    const identifier = self.file_content[start_pos..self.tokenize_state.current_pos];
                    // Check if it's a keyword
                    const maybe_keyword = keywords.get(identifier);
                    if (maybe_keyword) |token| {
                        // If it's a keyword, append the keyword token
                        var base: *BaseToken = @ptrCast(@constCast(&token));
                        base.pos = start_pos; // Set the position of the keyword token
                        const nToken: *Token = @alignCast(@ptrCast(base));
                        try self.tokens.append(nToken.*);
                    } else {
                        // If it's not a keyword, append an identifier token
                        try self.tokens.append(Token{ .Identifier = .{ .pos = start_pos, .name = identifier } });
                    }
                },
                '+' => {
                    if (self.tokenize_state.next_char == '+') {
                        try self.tokens.append(Token{ .LexerError = .{ .pos = self.tokenize_state.current_pos, .message = "Increment operator (++) is not supported in Howl." } });
                        self.advance();
                    } else if (self.tokenize_state.next_char == '=') {
                        // Handle addition assignment operator (+=)
                        try self.tokens.append(Token{ .PlusAssign = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else if (self.tokenize_state.next_char == '%') {
                        // Handle addition with overflow operator (%+)
                        try self.tokens.append(Token{ .PlusPercent = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '%'
                    } else {
                        // Handle addition operator (+)
                        try self.tokens.append(Token{ .Plus = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '-' => {
                    if (self.tokenize_state.next_char == '-') {
                        try self.tokens.append(Token{
                            .LexerError = .{
                                .pos = self.tokenize_state.current_pos,
                                .message = "Decrement operator (--) is not supported in Howl.",
                            },
                        });
                        self.advance();
                    } else if (self.tokenize_state.next_char == '=') {
                        // Handle subtraction assignment operator (-=)
                        try self.tokens.append(Token{ .MinusAssign = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else if (self.tokenize_state.next_char == '%') {
                        // Handle subtraction with overflow operator (%-)
                        try self.tokens.append(Token{ .MinusPercent = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '%'
                    } else {
                        // Handle subtraction operator (-)
                        try self.tokens.append(Token{ .Minus = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '*' => {
                    if (self.tokenize_state.next_char == '*') {
                        // Handle exponentiation operator (**)
                        try self.tokens.append(Token{ .AsteriskAsterisk = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip second '*'
                    } else if (self.tokenize_state.next_char == '%') {
                        // Handle multiplication with overflow operator (%*)
                        try self.tokens.append(Token{ .AsteriskPercent = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '%'
                    } else {
                        // Handle multiplication operator (*)
                        try self.tokens.append(Token{ .Asterisk = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '=' => {
                    if (self.tokenize_state.next_char == '=') {
                        // Handle equality operator (==)
                        try self.tokens.append(Token{ .EqualEqual = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip second '='
                    } else if (self.tokenize_state.next_char == '>') {
                        // Handle arrow operator (=>)
                        try self.tokens.append(Token{ .DoubleArrow = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '>'
                    } else {
                        // Handle assignment operator (=)
                        try self.tokens.append(Token{ .Assignment = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '!' => {
                    if (self.tokenize_state.next_char == '=') {
                        // Handle not equal operator (!=)
                        try self.tokens.append(Token{ .NotEqual = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else {
                        // Handle error on type operator !Type or MyError!Type
                        try self.tokens.append(Token{ .Exclamation = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '?' => {
                    // Handle question mark operator (?)
                    try self.tokens.append(Token{ .QuestionMark = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '/' => {
                    if (self.tokenize_state.next_char == '/') {
                        var isDocComment = false;
                        const start_pos = self.tokenize_state.current_pos;
                        // Handle single-line comment
                        self.advance(); // skip first '/'

                        if (self.tokenize_state.next_next_char == '/') {
                            isDocComment = true;
                            self.advance(); // skip second '/'
                        }

                        while (self.tokenize_state.char != '\n' and self.tokenize_state.char != 0) {
                            self.advance();
                        }
                        if (isDocComment) {
                            // Handle doc comment
                            try self.tokens.append(Token{ .DocComment = .{ .pos = start_pos, .value = self.file_content[start_pos + 2 .. self.tokenize_state.current_pos] } });
                        } else {
                            // Handle single-line comment
                            try self.tokens.append(Token{ .Comment = .{ .pos = self.tokenize_state.current_pos, .value = self.file_content[start_pos + 2 .. self.tokenize_state.current_pos] } });
                        }
                    } else if (self.tokenize_state.next_char == '%') {
                        // Handle division with overflow operator (%/)
                        try self.tokens.append(Token{ .SlashPercent = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '%'
                    } else {
                        // Handle division operator (/)
                        try self.tokens.append(Token{ .Slash = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },

                '%' => {
                    try self.tokens.append(Token{ .Percent = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },

                '&' => {
                    try self.tokens.append(Token{ .Ampersand = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
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
                '$' => {
                    // Handle dollar sign (used for special tokens or identifiers)
                    try self.tokens.append(Token{ .Dollar = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },

                '`' => {
                    try self.tokens.append(Token{ .Backtick = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance(); // skip '`'
                },
                '\'' => {
                    self.advance();
                    if (self.tokenize_state.char == '\\') {
                        // Handle escaped character
                        self.advance(); // skip '\'
                    }
                    if (self.tokenize_state.next_char != '\'') {
                        std.debug.print("Unterminated character literal at position {d}\n", .{self.tokenize_state.current_pos});
                        return error.UnterminatedCharacterLiteral;
                    }
                    // Now we have the character literal from current_pos to next_pos
                    try self.tokens.append(Token{ .CharLiteral = .{ .pos = self.tokenize_state.current_pos, .char = self.file_content[self.tokenize_state.current_pos + 1] } });
                    self.advance(); // skip closing quote
                },
                '<' => {
                    if (self.tokenize_state.next_char == '=') {
                        // Handle less than or equal operator (<=)
                        try self.tokens.append(Token{ .LessThanEqual = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='

                    } else {
                        // Handle less than operator (<)
                        try self.tokens.append(Token{ .LessThan = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                '>' => {
                    if (self.tokenize_state.next_char == '=') {
                        // Handle greater than or equal operator (>=)
                        try self.tokens.append(Token{ .GreaterThanEqual = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else {
                        // Handle greater than operator (>)
                        try self.tokens.append(Token{ .GreaterThan = .{ .pos = self.tokenize_state.current_pos } });
                    }
                    self.advance();
                },
                ':' => {
                    if (self.tokenize_state.next_char == ':') {
                        try self.tokens.append(Token{ .DoubleColon = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip second ':'
                    } else if (self.tokenize_state.next_char == '=') {
                        // Handle type definition operator (:=)
                        try self.tokens.append(Token{ .ColonEquals = .{ .pos = self.tokenize_state.current_pos } });
                        self.advance(); // skip '='
                    } else {
                        // Handle colon operator (:)
                        try self.tokens.append(Token{ .Colon = .{ .pos = self.tokenize_state.current_pos } });
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
                            // Handle range operator (..>)
                            try self.tokens.append(Token{ .DotDotEquals = .{ .pos = self.tokenize_state.current_pos } });
                            self.advance(); // skip second '.'
                            self.advance(); // skip '>'
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
                '@' => {
                    // Handle at sign (used for attributes or special tokens)
                    // Check if this is @import
                    if (self.tokenize_state.next_char == 'i' and 
                        self.tokenize_state.current_pos + 6 < self.file_content.len and 
                        std.mem.eql(u8, self.file_content[self.tokenize_state.current_pos+1..self.tokenize_state.current_pos+7], "import")) {
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
                '\\' => {
                    // Handle backslash (used for escape sequences or special tokens)
                    try self.tokens.append(Token{ .Backslash = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '~' => {
                    // Handle tilde (used for bitwise NOT or special tokens)
                    try self.tokens.append(Token{ .Tilde = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '^' => {
                    // Handle caret (used for bitwise XOR or special tokens)
                    try self.tokens.append(Token{ .Caret = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                // Handle unrecognized characters'
                0 => {
                    // EOF reached, break the loop
                    break;
                },
                '(' => {
                    // Handle opening parenthesis
                    try self.tokens.append(Token{ .LeftParen = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ')' => {
                    // Handle closing parenthesis
                    try self.tokens.append(Token{ .RightParen = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '{' => {
                    // Handle opening brace
                    try self.tokens.append(Token{ .LeftBrace = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '}' => {
                    // Handle closing brace
                    try self.tokens.append(Token{ .RightBrace = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                '[' => {
                    // Handle opening bracket
                    try self.tokens.append(Token{ .LeftBracket = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ']' => {
                    // Handle closing bracket
                    try self.tokens.append(Token{ .RightBracket = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ';' => {
                    // Handle semicolon
                    try self.tokens.append(Token{ .Semicolon = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },
                ',' => {
                    // Handle comma
                    try self.tokens.append(Token{ .Comma = .{ .pos = self.tokenize_state.current_pos } });
                    self.advance();
                },

                else => {
                    std.debug.print("Unrecognized character: '{c}' at position {d}\n", .{ self.tokenize_state.char, self.tokenize_state.current_pos });
                    return error.NotImplemented; // Handle other characters (identifiers, operators, etc.)
                },
            }
        }
        // Append EndOfFile token
        try self.tokens.append(Token{ .EndOfFile = .{ .pos = self.tokenize_state.current_pos } });
    }

    /// This function will parse numeric literals (integers and floats)
    /// assumes the current LexingState is at the start of a numeric literal
    /// We also allow for underscores in numeric literals, e.g. 1_000_000
    /// We will also allow for scientific notation, e.g. 1.0e10
    /// For int literals, we convert `0b` (binary) or `0x` (hex) to i256
    fn parseNumeric(self: *LexerFile) !void {
        const start_pos = self.tokenize_state.current_pos;

        // Check for special prefixes (binary/hex)
        if (self.tokenize_state.char == '0') {
            switch (self.tokenize_state.next_char) {
                'b' => return parseIntegerWithBase(self, 2, start_pos),
                'x' => return parseIntegerWithBase(self, 16, start_pos),
                else => {},
            }
        }

        // Parse decimal number (could be int or float)
        try parseDecimalNumber(self, start_pos);
    }
};
test "Tokinize numeric" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const file_path = "test.howl";
    const file_content = "123 0x1a 0b1010 1_000 3.14 2.718e10 0.5e-2";
    const expectedIntegerLiterals = [_]i256{ 123, 0x1a, 0b1010, 1000 };
    const expectedFloatLiterals = [_]f128{ 3.14, 2.718e10, 0.5e-2 };
    var lexer = Lexer.init(allocator);
    var lexer_file = LexerFile.init(
        allocator,
        &lexer,
        file_path,
        file_content,
    );

    try lexer_file.tokenize();

    defer lexer_file.deinit();

    const tokens = lexer_file.tokens.items;
    var expected_float_index: usize = 0;
    var expected_int_index: usize = 0;
    for (tokens) |token| {
        switch (token) {
            .IntegerLiteral => |int_token| {
                //std.debug.print("Integer Literal: {d} at position {d}\n", .{ int_token.value, int_token.pos });
                try testing.expectEqual(expectedIntegerLiterals[expected_int_index], int_token.value);

                expected_int_index += 1;
            },
            .FloatLiteral => |float_token| {
                //std.debug.print("Float Literal: {d} at position {d}\n", .{ float_token.value, float_token.pos });
                try testing.expectApproxEqRel(expectedFloatLiterals[expected_float_index], float_token.value, 0.00001);

                expected_float_index += 1;
            },
            else => {
                //std.debug.print("Other Token: {any} \n", .{token});
            },
        }
    }

    try testing.expectEqual(expectedIntegerLiterals.len, expected_int_index);
    try testing.expectEqual(expectedFloatLiterals.len, expected_float_index);
}

test "keywords, builtin types and identifiers" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const file_path = "test.howl";
    const file_content = "for bShiftLeft bShiftRight i if banana Banana UwU8 bigi64";

    const expected = [_]Token{
        .{ .StartOfFile = .{ .pos = 0 } },
        .{ .For = .{ .pos = 0 } },
        .{ .Whitespace = .{ .pos = 3 } },
        .{ .BitwiseShiftLeft = .{ .pos = 4 } },
        .{ .Whitespace = .{ .pos = 14 } },
        .{ .BitwiseShiftRight = .{ .pos = 15 } },
        .{ .Whitespace = .{ .pos = 26 } },
        .{ .Identifier = .{ .pos = 27, .name = "i" } },
        .{ .Whitespace = .{ .pos = 28 } },
        .{ .If = .{ .pos = 29 } },
        .{ .Whitespace = .{ .pos = 31 } },
        .{ .Identifier = .{ .pos = 32, .name = "banana" } },
        .{ .Whitespace = .{ .pos = 38 } },
        .{ .Identifier = .{ .pos = 39, .name = "Banana" } },
        .{ .Whitespace = .{ .pos = 45 } },
        .{ .Identifier = .{ .pos = 46, .name = "UwU8" } },
        .{ .Whitespace = .{ .pos = 50 } },
        .{ .Identifier = .{ .pos = 51, .name = "bigi64" } },
        .{ .EndOfFile = .{ .pos = 57 } },
    };

    var lexer = Lexer.init(allocator);
    var lexer_file = LexerFile.init(
        allocator,
        &lexer,
        file_path,
        file_content,
    );

    try lexer_file.tokenize();

    defer lexer_file.deinit();

    const tokens = lexer_file.tokens.items;

    var offset: usize = 0;
    for (tokens) |token| {
        const expected_token = expected[offset];
        try testing.expectEqualDeep(expected_token, token);
        offset += 1;
    }
    try testing.expectEqual(expected.len, offset);
}

test "operator symbols" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const file_path = "test.howl";
    const file_content = "+ +% - -% -= * ** *% / /% = == != ! ? & | |> || #";

    const expected = [_]Token{
        .{ .StartOfFile = .{ .pos = 0 } },
        .{ .Plus = .{ .pos = 0 } },
        .{ .Whitespace = .{ .pos = 1 } },
        .{ .PlusPercent = .{ .pos = 2 } },
        .{ .Whitespace = .{ .pos = 4 } },
        .{ .Minus = .{ .pos = 5 } },
        .{ .Whitespace = .{ .pos = 6 } },
        .{ .MinusPercent = .{ .pos = 7 } },
        .{ .Whitespace = .{ .pos = 9 } },
        .{ .MinusAssign = .{ .pos = 10 } },
        .{ .Whitespace = .{ .pos = 12 } },
        .{ .Asterisk = .{ .pos = 13 } },
        .{ .Whitespace = .{ .pos = 14 } },
        .{ .AsteriskAsterisk = .{ .pos = 15 } },
        .{ .Whitespace = .{ .pos = 17 } },
        .{ .AsteriskPercent = .{ .pos = 18 } },
        .{ .Whitespace = .{ .pos = 20 } },
        .{ .Slash = .{ .pos = 21 } },
        .{ .Whitespace = .{ .pos = 22 } },
        .{ .SlashPercent = .{ .pos = 23 } },
        .{ .Whitespace = .{ .pos = 25 } },
        .{ .Assignment = .{ .pos = 26 } },
        .{ .Whitespace = .{ .pos = 27 } },
        .{ .EqualEqual = .{ .pos = 28 } },
        .{ .Whitespace = .{ .pos = 30 } },
        .{ .NotEqual = .{ .pos = 31 } },
        .{ .Whitespace = .{ .pos = 33 } },
        .{ .Exclamation = .{ .pos = 34 } },
        .{ .Whitespace = .{ .pos = 35 } },
        .{ .QuestionMark = .{ .pos = 36 } },
        .{ .Whitespace = .{ .pos = 37 } },
        .{ .Ampersand = .{ .pos = 38 } },
        .{ .Whitespace = .{ .pos = 39 } },
        .{ .Pipe = .{ .pos = 40 } },
        .{ .Whitespace = .{ .pos = 41 } },
        .{ .PipeGreaterThan = .{ .pos = 42 } },
        .{ .Whitespace = .{ .pos = 44 } },
        .{ .Pipe = .{ .pos = 45 } },
        .{ .Pipe = .{ .pos = 46 } },
        .{ .Whitespace = .{ .pos = 47 } },
        .{ .Hash = .{ .pos = 48 } },
        .{ .EndOfFile = .{ .pos = 49 } },
    };

    var lexer = Lexer.init(allocator);
    var lexer_file = LexerFile.init(
        allocator,
        &lexer,
        file_path,
        file_content,
    );

    try lexer_file.tokenize();

    defer lexer_file.deinit();

    const tokens = lexer_file.tokens.items;

    var offset: usize = 0;
    for (tokens) |token| {
        const expected_token = expected[offset];
        try testing.expectEqualDeep(expected_token, token);
        offset += 1;
    }
    try testing.expectEqual(expected.len, offset);
}
