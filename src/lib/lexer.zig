const LexerToken = @import("lextoken.zig").LexerToken;
const TokenError = @import("lextoken.zig").TokenError;
const TokenKinds = @import("tokenkinds.zig").TokenKinds;
const std = @import("std");

const ArrayListToken = std.ArrayList(LexerToken);
const ArrayListLexerFiles = std.ArrayList(LexerFile);

pub const LexerFile = struct {
    const Self = @This();
    name: []const u8,
    contents: []const u8,
    tokens: ArrayListToken,
    allocator: std.mem.Allocator,
    index: usize = 0,
    // Errors are saved as tokens to allow for error recovery
    // and to provide better error messages.
    hasErrorTokens: bool = false,

    // LexerFile owns the memory of name and contents
    pub fn init(
        name: []const u8,
        contents: []const u8,
        allocator: std.mem.Allocator,
    ) LexerFile {
        return LexerFile{
            .name = name,
            .contents = contents,
            .tokens = ArrayListToken.init(allocator),
            .allocator = allocator,
        };
    }
    pub fn deinit(self: Self) void {
        self.allocator.free(self.name);
        self.allocator.free(self.contents);
        self.tokens.deinit();
    }

    pub fn printTokens(self: Self) void {
        for (self.tokens.items) |tok| {
            std.debug.print("\t{s: <20}{s}\n", .{ @tagName(tok.kind), tok.value orelse "" });
        }
    }

    pub const LexPos = struct {
        line: usize,
        col: usize,
    };

    // get line and column from a file offset
    pub fn getLexPos(self: Self, pos: usize) !LexPos {
        if (pos >= self.contents.len) {
            return error.InvalidPosition;
        }
        var line: usize = 1;
        var col: usize = 1;
        for (self.contents[0..pos]) |c| {
            if (c == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        return LexPos{ .line = line, .col = col };
    }

    // format a LexerToken error message
    // Used to give a syntax error message to the user
    // caller own the memory of the returned string
    pub fn fmtLexorErrorToken(self: Self, tok: LexerToken, tmpAllocator: std.mem.Allocator) ![]const u8 {
        const lex_pos = try self.getLexPos(tok.filePos);

        const msg = std.fmt.allocPrint(
            tmpAllocator,
            "Error: {s}\nPostion: {s}:{d}:{d}\n",
            .{
                tok.value orelse "Unknown error",
                self.name,
                lex_pos.line,
                lex_pos.col,
            },
        ) catch unreachable;
        return msg;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn isIdentifierChar(c: u8) bool {
        return isAlphaNumeric(c) or c == '_';
    }

    fn getLiteral(self: *LexerFile, start: usize, end: usize) []const u8 {
        return self.contents[start..end];
    }

    fn peek(self: *const LexerFile, offset: usize) u8 {
        const pos = self.index + offset;
        return if (pos < self.contents.len) self.contents[pos] else 0;
    }

    fn advance(self: *LexerFile) void {
        if (self.index < self.contents.len) {
            self.index += 1;
        }
    }

    fn advanceBy(self: *LexerFile, count: usize) void {
        self.index = @min(self.index + count, self.contents.len);
    }

    fn currentChar(self: *const LexerFile) u8 {
        return if (self.index < self.contents.len) self.contents[self.index] else 0;
    }

    pub fn lex(self: *LexerFile, lexer: Lexer) void {
        while (self.index < self.contents.len) {
            const start_pos = self.index;
            const current = self.currentChar();

            // Skip whitespace (but don't create tokens for it)
            if (std.ascii.isWhitespace(current)) {
                if (current == '\n') {
                    self.tokens.append(LexerToken.init(.Newline, start_pos, null)) catch unreachable;
                }
                self.advance();
                continue;
            }

            switch (current) {
                // Comments
                '/' => {
                    if (self.peek(1) == '/') {
                        self.lexLineComment(start_pos);
                    } else if (self.peek(1) == '*') {
                        self.lexBlockComment(start_pos);
                    } else if (self.peek(1) == '=') {
                        self.advanceBy(2);
                        self.tokens.append(LexerToken.init(.SlashEquals, start_pos, null)) catch unreachable;
                    } else {
                        self.advance();
                        self.tokens.append(LexerToken.init(.Slash, start_pos, null)) catch unreachable;
                    }
                },

                // Numbers
                '0'...'9' => {
                    self.lexNumber(start_pos);
                },

                // Identifiers and Keywords
                'a'...'z', 'A'...'Z', '_' => {
                    self.lexIdentifier(start_pos, lexer);
                },

                // String literals
                '"' => {
                    self.lexString(start_pos);
                },

                // Character literals
                '\'' => {
                    self.lexChar(start_pos);
                },

                // Two-character operators
                '+' => self.lexOperator(start_pos, .Plus, '=', .PlusEquals),
                '-' => self.lexOperatorTwoOptions(start_pos, .Minus, '=', .MinusEquals, '>', .MinusGreaterThan),
                '*' => self.lexOperator(start_pos, .Asterisk, '=', .AsteriskEquals),
                '%' => self.lexOperator(start_pos, .Percent, '=', .PercentEquals),
                '=' => self.lexOperatorTwoOptions(start_pos, .Equals, '=', .DoubleEquals, '>', .EqualsGreaterThan),
                '!' => self.lexOperator(start_pos, .Exclamation, '=', .ExclamationEquals),
                '<' => self.lexOperatorTwoOptions(start_pos, .LessThan, '=', .LessThanEquals, '<', .DoubleLessThan),
                '>' => self.lexOperatorTwoOptions(start_pos, .GreaterThan, '=', .GreaterThanEquals, '>', .DoubleGreaterThan),
                '|' => self.lexOperator(start_pos, .Pipe, '>', .PipeGreaterThan),
                ':' => self.lexOperatorTwoOptions(start_pos, .Colon, ':', .ColonColon, '=', .ColonEquals),

                // Dot operators (special case for ranges)
                '.' => {
                    if (self.peek(1) == '.') {
                        if (self.peek(2) == '.') {
                            self.advanceBy(3);
                            self.tokens.append(LexerToken.init(.DotDotDot, start_pos, null)) catch unreachable;
                        } else if (self.peek(2) == '=') {
                            self.advanceBy(3);
                            self.tokens.append(LexerToken.init(.DotDotEquals, start_pos, null)) catch unreachable;
                        } else if (self.peek(2) == '<') {
                            self.advanceBy(3);
                            self.tokens.append(LexerToken.init(.DotDotLessThan, start_pos, null)) catch unreachable;
                        } else {
                            self.advanceBy(2);
                            self.tokens.append(LexerToken.init(.DotDot, start_pos, null)) catch unreachable;
                        }
                    } else {
                        self.advance();
                        self.tokens.append(LexerToken.init(.Dot, start_pos, null)) catch unreachable;
                    }
                },

                // Single-character operators
                '&' => self.lexSingleChar(start_pos, .Ampersand),
                '^' => self.lexSingleChar(start_pos, .Caret),
                '[' => self.lexSingleChar(start_pos, .BracketOpen),
                ']' => self.lexSingleChar(start_pos, .BracketClose),
                '(' => self.lexSingleChar(start_pos, .ParenOpen),
                ')' => self.lexSingleChar(start_pos, .ParenClose),
                '{' => self.lexSingleChar(start_pos, .CurlyOpen),
                '}' => self.lexSingleChar(start_pos, .CurlyClose),
                ',' => self.lexSingleChar(start_pos, .Comma),
                '$' => self.lexSingleChar(start_pos, .Dollar),
                '#' => self.lexSingleChar(start_pos, .Hash),
                '~' => self.lexSingleChar(start_pos, .Tilde),
                ';' => self.lexSingleChar(start_pos, .Semicolon),
                '?' => self.lexSingleChar(start_pos, .Question),

                // Builtin functions
                '@' => {
                    self.lexBuiltin(start_pos);
                },

                // Handle any other characters
                else => {
                    const error_msg = std.fmt.allocPrint(self.allocator, "Unexpected character: '{c}' (ASCII {d})", .{ current, current }) catch "Unexpected character";

                    self.hasErrorTokens = true;
                    self.tokens.append(LexerToken.init(.ErrorToken, start_pos, error_msg)) catch unreachable;
                    self.advance();
                },
            }
        }

        // Add EOF token
        self.tokens.append(LexerToken.init(.EOF, self.index, null)) catch unreachable;
    }

    fn lexSingleChar(self: *LexerFile, start_pos: usize, kind: TokenKinds) void {
        self.advance();
        self.tokens.append(LexerToken.init(kind, start_pos, null)) catch unreachable;
    }

    fn lexOperator(self: *LexerFile, start_pos: usize, single_kind: TokenKinds, next_char: u8, double_kind: TokenKinds) void {
        if (self.peek(1) == next_char) {
            self.advanceBy(2);
            self.tokens.append(LexerToken.init(double_kind, start_pos, null)) catch unreachable;
        } else {
            self.advance();
            self.tokens.append(LexerToken.init(single_kind, start_pos, null)) catch unreachable;
        }
    }

    fn lexOperatorTwoOptions(self: *LexerFile, start_pos: usize, single_kind: TokenKinds, next_char1: u8, double_kind1: TokenKinds, next_char2: u8, double_kind2: TokenKinds) void {
        const next = self.peek(1);
        if (next == next_char1) {
            self.advanceBy(2);
            self.tokens.append(LexerToken.init(double_kind1, start_pos, null)) catch unreachable;
        } else if (next == next_char2) {
            self.advanceBy(2);
            self.tokens.append(LexerToken.init(double_kind2, start_pos, null)) catch unreachable;
        } else {
            self.advance();
            self.tokens.append(LexerToken.init(single_kind, start_pos, null)) catch unreachable;
        }
    }

    fn lexLineComment(self: *LexerFile, start_pos: usize) void {
        self.advanceBy(2); // Skip '//'
        const comment_start = self.index;

        // Consume until end of line
        while (self.index < self.contents.len and self.currentChar() != '\n') {
            self.advance();
        }

        const comment_value = self.getLiteral(comment_start, self.index);
        self.tokens.append(LexerToken.init(.LineComment, start_pos, comment_value)) catch unreachable;
    }

    fn lexBlockComment(self: *LexerFile, start_pos: usize) void {
        self.advanceBy(2); // Skip '/*'
        const comment_start = self.index;
        var nesting: usize = 1;

        while (nesting > 0 and self.index < self.contents.len) {
            if (self.currentChar() == '/' and self.peek(1) == '*') {
                nesting += 1;
                self.advanceBy(2);
            } else if (self.currentChar() == '*' and self.peek(1) == '/') {
                nesting -= 1;
                if (nesting > 0) self.advanceBy(2);
            } else {
                self.advance();
            }
        }

        if (nesting == 0) {
            const comment_value = self.getLiteral(comment_start, self.index);
            self.advanceBy(2); // Skip final '*/'
            self.tokens.append(LexerToken.init(.BlockComment, start_pos, comment_value)) catch unreachable;
        } else {
            // Unterminated block comment
            self.hasErrorTokens = true;
            self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Unterminated block comment")) catch unreachable;
        }
    }

    fn lexNumber(self: *LexerFile, start_pos: usize) void {
        // Handle special number prefixes
        if (self.currentChar() == '0' and self.index + 1 < self.contents.len) {
            const next = self.peek(1);
            if (next == 'x' or next == 'X') {
                self.lexHexNumber(start_pos);
                return;
            } else if (next == 'b' or next == 'B') {
                self.lexBinaryNumber(start_pos);
                return;
            } else if (next == 'o' or next == 'O') {
                self.lexOctalNumber(start_pos);
                return;
            }
        }

        // Regular decimal number
        self.lexDecimalNumber(start_pos);
    }

    fn lexHexNumber(self: *LexerFile, start_pos: usize) void {
        self.advanceBy(2); // Skip '0x'

        while (self.index < self.contents.len) {
            const c = self.currentChar();
            if (isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F') or c == '_') {
                self.advance();
            } else {
                break;
            }
        }

        const number_value = self.getLiteral(start_pos, self.index);
        self.tokens.append(LexerToken.init(.NumberLiteral, start_pos, number_value)) catch unreachable;
    }

    fn lexBinaryNumber(self: *LexerFile, start_pos: usize) void {
        self.advanceBy(2); // Skip '0b'

        while (self.index < self.contents.len) {
            const c = self.currentChar();
            if (c == '0' or c == '1' or c == '_') {
                self.advance();
            } else {
                break;
            }
        }

        const number_value = self.getLiteral(start_pos, self.index);
        self.tokens.append(LexerToken.init(.NumberLiteral, start_pos, number_value)) catch unreachable;
    }

    fn lexOctalNumber(self: *LexerFile, start_pos: usize) void {
        self.advanceBy(2); // Skip '0o'

        while (self.index < self.contents.len) {
            const c = self.currentChar();
            if ((c >= '0' and c <= '7') or c == '_') {
                self.advance();
            } else {
                break;
            }
        }

        const number_value = self.getLiteral(start_pos, self.index);
        self.tokens.append(LexerToken.init(.NumberLiteral, start_pos, number_value)) catch unreachable;
    }

    fn lexDecimalNumber(self: *LexerFile, start_pos: usize) void {
        var is_float = false;

        // Consume integer part
        while (self.index < self.contents.len) {
            const c = self.currentChar();
            if (isDigit(c) or c == '_') {
                self.advance();
            } else {
                break;
            }
        }

        // Check for decimal point
        if (self.currentChar() == '.' and self.index + 1 < self.contents.len and isDigit(self.peek(1))) {
            is_float = true;
            self.advance(); // Skip '.'

            // Consume fractional part
            while (self.index < self.contents.len) {
                const c = self.currentChar();
                if (isDigit(c) or c == '_') {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Check for scientific notation
        const c = self.currentChar();
        if (c == 'e' or c == 'E') {
            is_float = true;
            self.advance();

            // Handle optional sign
            if (self.currentChar() == '+' or self.currentChar() == '-') {
                self.advance();
            }

            // Ensure there's at least one digit
            if (!isDigit(self.currentChar())) {
                self.hasErrorTokens = true;
                self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Invalid scientific notation: missing exponent digits")) catch unreachable;
                return;
            }

            // Consume exponent digits
            while (self.index < self.contents.len) {
                const exp_c = self.currentChar();
                if (isDigit(exp_c) or exp_c == '_') {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        const number_value = self.getLiteral(start_pos, self.index);
        const token_kind = if (is_float) TokenKinds.FloatLiteral else TokenKinds.NumberLiteral;
        self.tokens.append(LexerToken.init(token_kind, start_pos, number_value)) catch unreachable;
    }

    fn lexIdentifier(self: *LexerFile, start_pos: usize, lexer: Lexer) void {
        // Consume until not identifier char
        while (self.index < self.contents.len and isIdentifierChar(self.currentChar())) {
            self.advance();
        }

        const id_value = self.getLiteral(start_pos, self.index);
        const maybeKeyword = lexer.keyWordMap.get(id_value);

        if (maybeKeyword) |keyword| {
            self.tokens.append(LexerToken.init(keyword, start_pos, null)) catch unreachable;
        } else {
            self.tokens.append(LexerToken.init(TokenKinds.Identifier, start_pos, id_value)) catch unreachable;
        }
    }

    fn lexString(self: *LexerFile, start_pos: usize) void {
        self.advance(); // Skip opening quote
        const string_start = self.index;
        var string_terminated = false;

        while (self.index < self.contents.len) {
            const c = self.currentChar();

            if (c == '\\') {
                // Skip escape sequence (simplified - could be enhanced)
                self.advance();
                if (self.index < self.contents.len) {
                    self.advance();
                }
            } else if (c == '"') {
                string_terminated = true;
                break;
            } else if (c == '\n') {
                // Multiline strings not allowed (adjust if needed for your language)
                break;
            } else {
                self.advance();
            }
        }

        if (string_terminated) {
            const string_value = self.getLiteral(string_start, self.index);
            self.advance(); // Skip closing quote
            self.tokens.append(LexerToken.init(.StringLiteral, start_pos, string_value)) catch unreachable;
        } else {
            self.hasErrorTokens = true;
            self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Unterminated string literal")) catch unreachable;
        }
    }

    fn lexChar(self: *LexerFile, start_pos: usize) void {
        self.advance(); // Skip opening quote
        const char_start = self.index;
        var char_terminated = false;
        var char_count: usize = 0;

        while (self.index < self.contents.len) {
            const c = self.currentChar();

            if (c == '\\') {
                // Escape sequence
                self.advance();
                if (self.index < self.contents.len) {
                    self.advance();
                    char_count += 1;
                }
            } else if (c == '\'') {
                char_terminated = true;
                break;
            } else if (c == '\n') {
                break;
            } else {
                self.advance();
                char_count += 1;
            }
        }

        if (char_terminated and char_count == 1) {
            const char_value = self.getLiteral(char_start, self.index);
            self.advance(); // Skip closing quote
            self.tokens.append(LexerToken.init(.CharLiteral, start_pos, char_value)) catch unreachable;
        } else if (!char_terminated) {
            self.hasErrorTokens = true;
            self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Unterminated character literal")) catch unreachable;
        } else {
            self.hasErrorTokens = true;
            self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Invalid character literal: must contain exactly one character")) catch unreachable;
        }
    }

    fn lexBuiltin(self: *LexerFile, start_pos: usize) void {
        self.advance(); // Skip '@'

        // Handle @{} syntax
        if (self.currentChar() == '{') {
            self.tokens.append(LexerToken.init(.At, start_pos, null)) catch unreachable;
            return;
        }

        const id_start = self.index;

        // Consume identifier characters
        while (self.index < self.contents.len and isIdentifierChar(self.currentChar())) {
            self.advance();
        }

        if (id_start == self.index) {
            // No identifier after @
            self.hasErrorTokens = true;
            self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Expected identifier after '@'")) catch unreachable;
            return;
        }

        const id_value = self.getLiteral(id_start, self.index);
        // Map builtin function names to appropriate tokens
        // For now using .Import as placeholder - extend this mapping as needed
        self.tokens.append(LexerToken.init(.Import, start_pos, id_value)) catch unreachable;
    }
};

pub const Lexer = struct {
    const Self = @This();
    files: ArrayListLexerFiles,
    allocator: std.mem.Allocator,
    keyWordMap: std.StringHashMap(TokenKinds),

    pub fn init(allocator: std.mem.Allocator) Lexer {
        return Lexer{
            .files = ArrayListLexerFiles.init(allocator),
            .allocator = allocator,
            .keyWordMap = initKeywordHashMap(allocator) catch unreachable,
        };
    }
    pub fn deinit(self: *Lexer) void {
        for (self.files.items) |file| {
            file.deinit();
        }
        self.files.deinit();
        self.keyWordMap.deinit();
    }

    /// Add a file to the lexer
    /// The file name and contents are owned by the lexer
    pub fn addFile(self: *Lexer, name: []const u8, contents: []const u8) void {
        const file = LexerFile.init(name, contents, self.allocator);
        self.files.append(file) catch unreachable;
        const file_idx = self.files.items.len - 1;
        self.files.items[file_idx].lex(self.*);
    }

    pub fn lex(self: *Lexer) void {
        for (self.files.items) |*file| {
            file.lex(self.*); // Pass the lexer to each file
        }
    }

    fn initKeywordHashMap(allocator: std.mem.Allocator) !std.StringHashMap(TokenKinds) {
        var map = std.StringHashMap(TokenKinds).init(allocator);

        try map.put("true", .True);
        try map.put("false", .False);
        try map.put("var", .Var);
        try map.put("const", .Const);
        try map.put("pub", .Pub);
        try map.put("let", .Let);
        try map.put("comptime", .Comptime);
        try map.put("comp", .Comp);
        try map.put("error", .Error);
        try map.put("unreachable", .Unreachable);
        try map.put("undefined", .Undefined);
        try map.put("test", .Test);
        try map.put("return", .Return);
        try map.put("break", .Break);
        try map.put("continue", .Continue);
        try map.put("defer", .Defer);
        try map.put("errdefer", .ErrDefer);
        try map.put("try", .Try);
        try map.put("catch", .Catch);
        try map.put("fn", .Fn);
        try map.put("struct", .Struct);
        try map.put("enum", .Enum);
        try map.put("union", .Union);
        try map.put("Self", .Self);
        try map.put("while", .While);
        try map.put("for", .For);
        try map.put("if", .If);
        try map.put("else", .Else);
        try map.put("match", .Match);
        try map.put("and", .And);
        try map.put("or", .Or);
        try map.put("not", .Not);
        try map.put("async", .Async);
        try map.put("await", .Await);
        try map.put("spawn", .Spawn);
        try map.put("suspend", .Suspend);
        try map.put("mutex", .Mutex);
        try map.put("constmutex", .ConstMutex);
        try map.put("lock", .Lock);
        try map.put("unlock", .Unlock);
        try map.put("channel", .Channel);
        try map.put("u8", .U8);
        try map.put("u16", .U16);
        try map.put("u32", .U32);
        try map.put("u64", .U64);
        try map.put("u128", .U128);
        try map.put("usize", .USize);
        try map.put("i8", .I8);
        try map.put("i16", .I16);
        try map.put("i32", .I32);
        try map.put("i64", .I64);
        try map.put("i128", .I128);
        try map.put("isize", .ISize);
        try map.put("f32", .F32);
        try map.put("f64", .F64);
        try map.put("f80", .F80);
        try map.put("f128", .F128);
        try map.put("bool", .Bool);
        try map.put("str", .String);
        try map.put("strb", .StringBuilder);
        try map.put("None", .None);
        try map.put("Some", .Some);
        try map.put("noreturn", .NoReturn);
        try map.put("AnyType", .AnyType);
        try map.put("Type", .Type);
        try map.put("AnyError", .AnyError);
        try map.put("void", .Void);
        try map.put("bOr", .bOr);
        try map.put("bAnd", .bAnd);
        try map.put("bXor", .bXor);
        try map.put("bNot", .bNot);
        return map;
    }

    pub fn printAllFiles(self: Self) void {
        for (self.files.items) |file| {
            std.debug.print("\n------------------------------------------------------------------\n", .{});
            std.debug.print("File: {s}\n", .{file.name});
            file.printTokens();
        }
    }
};
