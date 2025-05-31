pub const TokenKinds = enum {
    String,
    StringBuilder,
    U8,
    U16,
    U32,
    U64,
    U128,
    ComptimeInt,
    USize,
    ISize,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    F80,
    F128,
    ComptimeFloat,
    Bool,
    True,
    False,
    None, // null
    Void, // lack of type
    Some,
    NoReturn,
    AnyType,
    Var,
    Const,
    Pub,
    Let,
    Comptime,
    Comp,
    Error,
    AnyError,
    Unreachable,
    Undefined,
    Test,
    Return,
    Break,
    Continue,
    Defer,
    ErrDefer,
    Try,
    Catch,
    Fn,
    Struct,
    Enum,
    Union,
    Self,
    While,
    For,
    If,
    Else,
    Match,
    And,
    Or,
    Not,
    bOr,
    bAnd,
    bXor,
    bNot,
    Async,
    Await,
    Spawn,
    Suspend,
    Mutex,
    ConstMutex,
    Lock,
    Unlock,
    Channel,
    // builtin functions
    // detect with @ start
    Import,
    Vector,
    Type,
    FieldType,
    TypeOf,
    AddSec,
    SubSec,
    MulSec,
    DivSec,
    ModSec,
    Mod,
    Rem,
    RemSec,
    Panic,
    CompileError,
    CastUp,
    Truncate,
    CastDown,

    // operators
    Plus, // +
    Minus, // -
    Asterisk, // *
    Slash, // /
    Percent, // %
    PlusEquals, // +=
    MinusEquals, // -=
    AsteriskEquals, // *=
    SlashEquals, // /=
    PercentEquals, // %=
    Equals, // =
    Ampersand, // &
    Pipe, // |
    Caret, // ^
    Exclamation, // !
    ExclamationEquals, // !=
    DoubleEquals, // ==
    LessThan, // <
    GreaterThan, // >
    LessThanEquals, // <=
    GreaterThanEquals, // >=
    DoubleLessThan, // <<
    DoubleGreaterThan, // >>
    PipeGreaterThan, // |>
    BracketOpen, // [
    BracketClose, // ]
    ParenOpen, // (
    ParenClose, // )
    CurlyOpen, // {
    CurlyClose, // }
    Comma, // ,
    Dot, // .
    DotDot, // ..
    DotDotDot, // ...
    DotDotEquals, // ..=
    DotDotLessThan, // ..<
    Dollar, // $
    Hash, // #
    Tilde, // ~
    Colon, // :
    ColonColon, // ::
    ColonEquals, // :=
    Semicolon, // ;
    Question, // ?
    At, // @

    MinusGreaterThan, // ->
    EqualsGreaterThan, // =>

    // user defined
    Identifier,
    StringLiteral,
    NumberLiteral,
    FloatLiteral,
    CharLiteral,
    LineComment, // `//` comment
    BlockComment, // `/* */` comment
    Newline,
    Whitespace,
    EOF,
    Unknown,
    ErrorToken, // error token is used to represent a token that was not parsed correctly
    // this is used to recover from errors and give the user feedback about what went wrong

};
