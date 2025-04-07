const Lexer = @import("lexer.zig").Lexer;
const std = @import("std");
const testing = std.testing;
const TokenKinds = @import("tokenkinds.zig").TokenKinds;
const LexerToken = @import("lextoken.zig").LexerToken;

const testSrc1 =
    \\ const std = @import("std")
    \\ pub fn main () !void {
    \\     std.debug.print("Hello, World!\n", .{})
    \\     var x: i32 = 5
    \\     x += 0b1010
    \\   x += 0o12
    \\   x += 0x10
    \\   x += 10_000
    \\   let y = 5.0
    \\   let z = 5.0e-10
    \\   
    \\ return
    \\ }
;
test "Lex a 'file'" {
    const source = try testing.allocator.alloc(u8, testSrc1.len);
    @memcpy(source, testSrc1);

    const file_name = try testing.allocator.alloc(u8, "test.zig".len);
    @memcpy(file_name, "test.zig");
    var lexer = Lexer.init(testing.allocator);
    defer lexer.deinit();
    lexer.addFile(file_name, source);
    //lexer.lex();
    lexer.printAllFiles();
}
