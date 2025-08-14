pub const BaseToken = struct {
    pos: usize,
};

pub const UnknownToken = struct {
    pos: usize,
    unknownValue: []const u8, // memory owned by the lexer / open file
};

pub const IdentifierToken = struct {
    pos: usize,
    value: []const u8, // memory owned by the lexer / open file
};

pub const StringLiteralToken = struct {
    pos: usize,
    value: []const u8, // memory owned by the lexer / open file
};

pub const IntegerLiteralToken = struct {
    pos: usize,
    value: i256, // int value of the literal
};
pub const FloatLiteralToken = struct {
    pos: usize,
    value: f128, // float value of the literal
};
pub const CharLiteralToken = struct {
    pos: usize,
    char: u8,
};

pub const LexerErrorToken = struct {
    pos: usize,
    message: []const u8, // memory owned by the lexer
};

pub const CommentToken = struct {
    pos: usize,
    value: []const u8, // memory owned by the lexer
};

pub const Token = union(enum) {
    Unknown: UnknownToken,

    Newline: BaseToken, // \n
    Whitespace: BaseToken, // space, tab, etc.

    Identifier: IdentifierToken,
    StringLiteral: StringLiteralToken, // "string"
    IntegerLiteral: IntegerLiteralToken, // 1245
    FloatLiteral: FloatLiteralToken, // 123.2
    CharLiteral: CharLiteralToken, // 'c'
    Comment: CommentToken, // // comment
    DocComment: CommentToken, // /// doc comment
    // Keywords
    For: BaseToken, // for
    If: BaseToken, // if
    Else: BaseToken, // else
    Match: BaseToken, // match
    Fn: BaseToken, // fn (function)
    // Builtin types
    I8: BaseToken, // i8
    I16: BaseToken, // i16
    I32: BaseToken, // i32
    I64: BaseToken, // i64
    I128: BaseToken, // i128
    U8: BaseToken, // u8
    U16: BaseToken, // u16
    U32: BaseToken, // u32
    U64: BaseToken, // u64
    U128: BaseToken, // u128
    F8: BaseToken, // f8
    F16: BaseToken, // f16
    F32: BaseToken, // f32
    F64: BaseToken, // f64
    Str: BaseToken, // Str
    StrB: BaseToken, // Strb
    Bool: BaseToken, // bool
    Pub: BaseToken, // pub (public visibility)
    Void: BaseToken, // void
    Some: BaseToken, // Some
    None: BaseToken, // None
    Type: BaseToken, // Type
    Error: BaseToken, // error
    Struct: BaseToken, // struct
    Tag: BaseToken, // tag
    Enum: BaseToken, // enum
    True: BaseToken, // true
    False: BaseToken, // false
    Let: BaseToken, // let
    Mut: BaseToken, // mut
    While: BaseToken, // while
    Return: BaseToken, // return
    Import: BaseToken, // @import

    // Operators and Punctuation
    Plus: BaseToken, // +
    PlusAssign: BaseToken, // += (add and assign)
    PlusPercent: BaseToken, // +%  (add with overflow)
    Minus: BaseToken, // -
    MinusAssign: BaseToken, // -= (subtract and assign)
    MinusPercent: BaseToken, // -% (subtract with underflow/overflow)
    Asterisk: BaseToken, // *
    AsteriskPercent: BaseToken, // *% (multiply with overflow)
    Slash: BaseToken, // /
    SlashPercent: BaseToken, // /% (divide with overflow or by zero (div by zero is zero) )
    Modulo: BaseToken, // mod
    Remainder: BaseToken, //  rem
    AsteriskAsterisk: BaseToken, // ** (exponentiation)
    And: BaseToken, // and
    Or: BaseToken, // or
    Not: BaseToken, // not
    EqualEqual: BaseToken, // == (to match lexer.zig)
    DoubleEquals: BaseToken, // == (alternative name)
    NotEqual: BaseToken, // != (to match lexer.zig)  
    ExclamationEquals: BaseToken, // != (alternative name)
    Assignment: BaseToken, // = (to match lexer.zig)
    Equals: BaseToken, // = (alternative name)
    Percent: BaseToken, // %
    LessThan: BaseToken, // <
    GreaterThan: BaseToken, // >
    LessThanEqual: BaseToken, // <= (to match lexer.zig)
    LessThanEquals: BaseToken, // <= (alternative name)
    GreaterThanEqual: BaseToken, // >= (to match lexer.zig)
    GreaterThanEquals: BaseToken, // >= (alternative name)
    BitwiseAnd: BaseToken, // bAnd
    BitwiseOr: BaseToken, // bOr
    BitwiseXor: BaseToken, // bXor
    BitwiseNot: BaseToken, // bNot
    BitwiseToggle: BaseToken, // bToggle
    BitwiseShiftLeft: BaseToken, // bShiftLeft
    BitwiseShiftRight: BaseToken, // bShiftRight
    BitwiseRotateLeft: BaseToken, // bRotLeft
    BitwiseRotateRight: BaseToken, // bRotRight
    LeftParen: BaseToken, // (
    RightParen: BaseToken, // )
    ParenthesisStart: BaseToken, // ( (alternative name)
    ParenthesisEnd: BaseToken, // ) (alternative name)
    LeftBracket: BaseToken, // [
    RightBracket: BaseToken, // ]
    BracketStart: BaseToken, // [ (alternative name)
    BracketEnd: BaseToken, // ] (alternative name)
    LeftBrace: BaseToken, // {
    RightBrace: BaseToken, // }
    BraceStart: BaseToken, // { (alternative name)
    BraceEnd: BaseToken, // } (alternative name)
    Semicolon: BaseToken, // ;
    Colon: BaseToken, // :
    ColonEquals: BaseToken, // :=
    DoubleColon: BaseToken, // ::
    Arrow: BaseToken, // ->
    DoubleArrow: BaseToken, // =>
    Ampersand: BaseToken, // &
    Tilde: BaseToken, // ~
    Exclamation: BaseToken, // !
    QuestionMark: BaseToken, // ?
    Backtick: BaseToken, // `
    Backslash: BaseToken, // \
    Dollar: BaseToken, // $
    Hash: BaseToken, // #
    At: BaseToken, // @
    Caret: BaseToken, // ^
    Dot: BaseToken, // .
    DotDot: BaseToken, // ..
    DotDotDot: BaseToken, // ...
    DotEquals: BaseToken, // .=
    DotDotEquals: BaseToken, // ..=
    DotDotLessThan: BaseToken, // ..<

    Underscore: BaseToken, // _
    Comma: BaseToken, // ,
    Pipe: BaseToken, // |
    PipeGreaterThan: BaseToken, // |>
    // Special tokens
    LexerError: LexerErrorToken, // Lexer error token, used to report errors during lexing and error recovery
    StartOfFile: BaseToken, // Start of file token, used to indicate the start
    EndOfFile: BaseToken, // End of file token, used to indicate the end (to match lexer.zig)
    EOF: BaseToken, // End of file token, used to indicate the end

    pub fn getPos(self: Token) usize {
        // Use inline switch to handle all token types efficiently
        return switch (self) {
            inline else => |token_data| token_data.pos,
        };
    }
    
    pub fn getEndPos(self: Token) usize {
        const start_pos = self.getPos();
        return switch (self) {
            .Identifier => |token| start_pos + token.value.len,
            .StringLiteral => |token| start_pos + token.value.len + 2, // +2 for quotes
            .IntegerLiteral => start_pos + 1, // Simple approximation
            .FloatLiteral => start_pos + 1, // Simple approximation
            .CharLiteral => start_pos + 3, // 'c' = 3 chars
            .Comment => |token| start_pos + token.value.len,
            .DocComment => |token| start_pos + token.value.len,
            .DoubleColon => start_pos + 2, // ::
            .ColonEquals => start_pos + 2, // :=
            .DoubleEquals => start_pos + 2, // ==
            .ExclamationEquals => start_pos + 2, // !=
            .LessThanEquals => start_pos + 2, // <=
            .GreaterThanEquals => start_pos + 2, // >=
            .Arrow => start_pos + 2, // ->
            .DoubleArrow => start_pos + 2, // =>
            .DotDot => start_pos + 2, // ..
            .DotDotDot => start_pos + 3, // ...
            .DotEquals => start_pos + 2, // .=
            .DotDotEquals => start_pos + 3, // ..=
            .DotDotLessThan => start_pos + 3, // ..<
            // Single character tokens
            else => start_pos + 1,
        };
    }
};
