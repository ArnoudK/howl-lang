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

    c: u8 = 0,
    c_next: u8 = 0,
    c_next_next: u8 = 0,
    c_next_next_next: u8 = 0,

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

    fn moveNext(self: *LexerFile) void {
        self.index += 1;
        self.c = self.c_next;
        self.c_next = self.c_next_next;
        self.c_next_next = self.c_next_next_next;
        self.c_next_next_next = if (self.index + 3 < self.contents.len) self.contents[self.index + 3] else 0;
    }

    pub fn lex(self: *LexerFile, lexer: Lexer) void {
        const len = self.contents.len;
        if (self.index >= len) {
            return;
        }
        self.c = self.contents[self.index];
        self.c_next = if (self.index + 1 < len) self.contents[self.index + 1] else 0;
        self.c_next_next = if (self.index + 2 < len) self.contents[self.index + 2] else 0;
        self.c_next_next_next = if (self.index + 3 < len) self.contents[self.index + 3] else 0;

        while (self.index < self.contents.len) : (self.moveNext()) {
            const start_pos = self.index;

            switch (self.c) {
                // Whitespace
                ' ', '\t' => {
                    while (self.index + 1 < len and (self.c_next == ' ' or self.c_next == '\t')) {
                        self.moveNext();
                    }
                    self.tokens.append(LexerToken.init(.Whitespace, self.index, null)) catch unreachable;
                },
                '\n' => {
                    self.tokens.append(LexerToken.init(.Newline, self.index, null)) catch unreachable;
                },
                '\r' => {},

                // Comments
                '/' => {
                    if (self.c_next == '/') {
                        // Line comment
                        self.moveNext(); // Skip the second '/'
                        const comment_start = self.index + 1;
                        // Consume until end of line
                        while (self.index + 1 < len and self.contents[self.index + 1] != '\n') {
                            self.index += 1;
                        }
                        const comment_value = self.getLiteral(comment_start, self.index + 1);
                        self.tokens.append(LexerToken.init(.LineComment, start_pos, comment_value)) catch unreachable;
                    } else if (self.c_next == '*') {
                        // Block comment
                        self.moveNext(); // Skip the '*'
                        const comment_start = self.index + 1;
                        var nesting: usize = 1;

                        while (nesting > 0 and self.index + 2 < len) {
                            self.moveNext();
                            if (self.contents[self.index] == '/' and self.contents[self.index + 1] == '*') {
                                nesting += 1;
                                self.moveNext();
                            } else if (self.contents[self.index] == '*' and self.contents[self.index + 1] == '/') {
                                nesting -= 1;
                                self.moveNext();
                            }
                        }

                        if (nesting == 0) {
                            const comment_value = self.getLiteral(comment_start, self.index - 1);
                            self.tokens.append(LexerToken.init(.BlockComment, start_pos, comment_value)) catch unreachable;
                        } else {
                            // Unterminated block comment
                            self.hasErrorTokens = true;
                            self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Unterminated block comment")) catch unreachable;
                        }
                    } else {
                        self.tokens.append(LexerToken.init(.Slash, self.index, null)) catch unreachable;
                    }
                },

                // Numbers
                '0'...'9' => {
                    const number_start = self.index;
                    var is_float = false;

                    // Handle hex, binary, octal prefixes
                    if (self.c == '0' and self.index + 1 < len) {
                        const next = self.contents[self.index + 1];
                        if (next == 'x' or next == 'X' or next == 'b' or next == 'B' or next == 'o' or next == 'O') {
                            // Skip the prefix
                            self.moveNext();
                            // Consume all valid digits for this base
                            while (self.index + 1 < len) {
                                const peek = self.contents[self.index + 1];
                                if (isDigit(peek) or peek == '_' or
                                    ((self.c == 'x' or self.c == 'X') and
                                        ((peek >= 'a' and peek <= 'f') or (peek >= 'A' and peek <= 'F'))))
                                {
                                    self.moveNext();
                                } else {
                                    break;
                                }
                            }
                        }
                    } else {
                        // Regular decimal number
                        // Consume digits and underscores
                        while (self.index + 1 < len) {
                            const peek = self.contents[self.index + 1];
                            if (isDigit(peek) or peek == '_') {
                                self.moveNext();
                            } else if (peek == '.' and self.index + 2 < len and isDigit(self.contents[self.index + 2])) {
                                // Handle decimal point for floating point numbers
                                is_float = true;
                                self.moveNext();

                                // Consume fractional part
                                while (self.index + 1 < len) {
                                    const frac_peek = self.contents[self.index + 1];
                                    if (isDigit(frac_peek) or frac_peek == '_') {
                                        self.moveNext();
                                    } else {
                                        break;
                                    }
                                }
                            } else if ((peek == 'e' or peek == 'E')) {
                                // Handle scientific notation (for both integers and floats)
                                is_float = true; // e-notation always produces a float
                                self.moveNext();

                                // Handle optional sign
                                var has_sign = false;
                                if (self.index + 1 < len and (self.contents[self.index + 1] == '+' or self.contents[self.index + 1] == '-')) {
                                    self.moveNext();
                                    has_sign = true;
                                }

                                // Ensure there's at least one digit after e/E and optional sign
                                if (self.index + 1 < len and isDigit(self.contents[self.index + 1])) {
                                    // Consume exponent digits
                                    while (self.index + 1 < len) {
                                        const exp_peek = self.contents[self.index + 1];
                                        if (isDigit(exp_peek) or exp_peek == '_') {
                                            self.moveNext();
                                        } else {
                                            break;
                                        }
                                    }
                                } else {
                                    // Invalid e-notation (no digits after e/E or sign)
                                    self.hasErrorTokens = true;
                                    const error_msg = if (has_sign)
                                        "Invalid scientific notation: missing digits after sign"
                                    else
                                        "Invalid scientific notation: missing digits after 'e'";
                                    self.tokens.append(LexerToken.init(.ErrorToken, number_start, error_msg)) catch unreachable;
                                    return;
                                }
                            } else {
                                break;
                            }
                        }
                    }

                    const number_value = self.getLiteral(number_start, self.index + 1);
                    const token_kind = if (is_float) TokenKinds.FloatLiteral else TokenKinds.NumberLiteral;
                    self.tokens.append(LexerToken.init(token_kind, number_start, number_value)) catch unreachable;
                },

                // Identifiers and Keywords
                'a'...'z', 'A'...'Z', '_' => {
                    const id_start = self.index;

                    // Consume until not identifier char
                    while (self.index + 1 < len and isIdentifierChar(self.contents[self.index + 1])) {
                        self.moveNext();
                    }

                    const id_value = self.getLiteral(id_start, self.index + 1);
                    const maybeKeyword = lexer.keyWordMap.get(id_value); // check if keyword or identifier
                    if (maybeKeyword) |keyword| {
                        self.tokens.append(LexerToken.init(keyword, id_start, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(TokenKinds.Identifier, start_pos, id_value)) catch unreachable;
                    }
                },

                // String literals
                '"' => {
                    const string_start = self.index + 1;
                    var string_terminated = false;

                    while (self.index + 1 < len) {
                        self.moveNext();

                        if (self.c == '\\' and self.c_next == '"') {
                            // Skip escaped quote
                            self.moveNext();
                        } else if (self.c == '"') {
                            string_terminated = true;
                            break;
                        }
                    }

                    if (string_terminated) {
                        const string_value = self.getLiteral(string_start, self.index);
                        self.tokens.append(LexerToken.init(.StringLiteral, start_pos, string_value)) catch unreachable;
                    } else {
                        // Unterminated string
                        self.hasErrorTokens = true;
                        self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Unterminated string!")) catch unreachable;
                    }
                },

                // Character literals
                '\'' => {
                    const char_start = self.index + 1;
                    var char_terminated = false;

                    if (self.index + 1 < len) {
                        self.moveNext();
                        if (self.c == '\\' and self.index + 1 < len) {
                            // Escape sequence
                            self.moveNext();
                        }

                        if (self.index + 1 < len and self.contents[self.index + 1] == '\'') {
                            self.index += 1;
                            char_terminated = true;
                        }
                    }

                    if (char_terminated) {
                        const char_value = self.getLiteral(char_start, self.index);
                        self.tokens.append(LexerToken.init(.CharLiteral, start_pos, char_value)) catch unreachable;
                    } else {
                        // Invalid character literal
                        self.hasErrorTokens = true;
                        self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Unterminated char literal")) catch unreachable;
                    }
                },

                // Operators and punctuation
                '+' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.PlusEquals, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Plus, start_pos, null)) catch unreachable;
                    }
                },
                '-' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.MinusEquals, start_pos, null)) catch unreachable;
                    } else if (self.c_next == '>') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.MinusGreaterThan, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Minus, start_pos, null)) catch unreachable;
                    }
                },
                '*' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.AsteriskEquals, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Asterisk, start_pos, null)) catch unreachable;
                    }
                },
                '%' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.PercentEquals, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Percent, start_pos, null)) catch unreachable;
                    }
                },
                '=' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.DoubleEquals, start_pos, null)) catch unreachable;
                    } else if (self.c_next == '>') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.EqualsGreaterThan, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Equals, start_pos, null)) catch unreachable;
                    }
                },
                '&' => {
                    self.tokens.append(LexerToken.init(.Ampersand, start_pos, null)) catch unreachable;
                },
                '|' => {
                    if (self.c_next == '>') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.PipeGreaterThan, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Pipe, start_pos, null)) catch unreachable;
                    }
                },
                '^' => {
                    self.tokens.append(LexerToken.init(.Caret, start_pos, null)) catch unreachable;
                },
                '!' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.ExclamationEquals, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Exclamation, start_pos, null)) catch unreachable;
                    }
                },
                '<' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.LessThanEquals, start_pos, null)) catch unreachable;
                    } else if (self.c_next == '<') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.DoubleLessThan, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.LessThan, start_pos, null)) catch unreachable;
                    }
                },
                '>' => {
                    if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.GreaterThanEquals, start_pos, null)) catch unreachable;
                    } else if (self.c_next == '>') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.DoubleGreaterThan, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.GreaterThan, start_pos, null)) catch unreachable;
                    }
                },
                '[' => {
                    self.tokens.append(LexerToken.init(.BracketOpen, start_pos, null)) catch unreachable;
                },
                ']' => {
                    self.tokens.append(LexerToken.init(.BracketClose, start_pos, null)) catch unreachable;
                },
                '(' => {
                    self.tokens.append(LexerToken.init(.ParenOpen, start_pos, null)) catch unreachable;
                },
                ')' => {
                    self.tokens.append(LexerToken.init(.ParenClose, start_pos, null)) catch unreachable;
                },
                '{' => {
                    self.tokens.append(LexerToken.init(.CurlyOpen, start_pos, null)) catch unreachable;
                },
                '}' => {
                    self.tokens.append(LexerToken.init(.CurlyClose, start_pos, null)) catch unreachable;
                },
                ',' => {
                    self.tokens.append(LexerToken.init(.Comma, start_pos, null)) catch unreachable;
                },
                '.' => {
                    if (self.c_next == '.') {
                        if (self.c_next_next == '.') {
                            self.moveNext();
                            self.moveNext();
                            self.tokens.append(LexerToken.init(.DotDotDot, start_pos, null)) catch unreachable;
                        } else if (self.c_next_next == '=') {
                            self.moveNext();
                            self.moveNext();
                            self.tokens.append(LexerToken.init(.DotDotEquals, start_pos, null)) catch unreachable;
                        } else if (self.c_next_next == '<') {
                            self.moveNext();
                            self.moveNext();
                            self.tokens.append(LexerToken.init(.DotDotLessThan, start_pos, null)) catch unreachable;
                        } else {
                            self.moveNext();
                            self.tokens.append(LexerToken.init(.DotDot, start_pos, null)) catch unreachable;
                        }
                    } else {
                        self.tokens.append(LexerToken.init(.Dot, start_pos, null)) catch unreachable;
                    }
                },
                '$' => {
                    self.tokens.append(LexerToken.init(.Dollar, start_pos, null)) catch unreachable;
                },
                '#' => {
                    self.tokens.append(LexerToken.init(.Hash, start_pos, null)) catch unreachable;
                },
                '~' => {
                    self.tokens.append(LexerToken.init(.Tilde, start_pos, null)) catch unreachable;
                },
                ':' => {
                    if (self.c_next == ':') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.ColonColon, start_pos, null)) catch unreachable;
                    } else if (self.c_next == '=') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.ColonEquals, start_pos, null)) catch unreachable;
                    } else {
                        self.tokens.append(LexerToken.init(.Colon, start_pos, null)) catch unreachable;
                    }
                },
                '@' => {
                    // Handle builtin function identifiers
                    if (self.c_next == '{') {
                        self.moveNext();
                        self.tokens.append(LexerToken.init(.At, start_pos, null)) catch unreachable;
                        continue;
                    }

                    const id_start = self.index + 1;

                    // Consume until not identifier char
                    while (self.index + 1 < len and isIdentifierChar(self.contents[self.index + 1])) {
                        self.moveNext();
                    }

                    const id_value = self.getLiteral(id_start, self.index + 1);
                    // Here you would map builtin function names to their corresponding TokenKinds
                    // For now, just using .Import as a placeholder
                    self.tokens.append(LexerToken.init(.Import, start_pos, id_value)) catch unreachable;
                },

                // Handle any other characters
                else => {
                    self.tokens.append(LexerToken.init(.ErrorToken, start_pos, "Wat")) catch unreachable;
                },
            }
        }

        // Add EOF token
        self.tokens.append(LexerToken.init(.EOF, self.index, null)) catch unreachable;
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
