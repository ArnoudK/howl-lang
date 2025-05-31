const std = @import("std");
const testing = std.testing;
const Lexer = @import("howl_lang_lib").Lexing.Lexer;
const Ast = @import("howl_lang_lib").Ast;

const testSrc1 =
    \\std :: @import("std")
    \\pub kek :: 5
    \\pub lol :: 10 + kek
    \\ 
    \\add :: fn(a: i32, b: i32) i32 {
    \\  a + b
    \\}
    \\ 
    \\pub main :: fn () {
    \\  x :: add(kek, lol)
    \\  std.debug.print("x = {}\n", .{x})
    \\}
    \\
;

test "lex into ast" {
    {
        const source = try testing.allocator.alloc(u8, testSrc1.len);
        @memcpy(source, testSrc1);

        const file_name = try testing.allocator.alloc(u8, "test.zig".len);
        @memcpy(file_name, "test.zig");
        var lexer = Lexer.init(testing.allocator);
        defer lexer.deinit();
        lexer.addFile(file_name, source);
        //    lexer.printAllFiles();

        {
            var astParser = Ast.Parser.init(lexer.files.items[0].tokens.items, std.heap.page_allocator);
            defer astParser.deinit();
            _ = try astParser.parse();
            astParser.print_ast();
        }
    }
    std.debug.print("\n", .{});
}
