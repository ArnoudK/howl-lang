const std = @import("std");
const testing = std.testing;
const Lexer = @import("lexer.zig").Lexer;
const Ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analysis.zig").SemanticAnalyzer;

const testSrc1 =
    //\\ const std = @import("std")
    \\ pub const kek = 5
    \\ pub const lol = 10 + kek
    \\ 
    \\ fn add(a: i32, b: i32) i32 {
    \\   return a + b    
    \\ }
    \\ 
    \\ pub fn main() {
    \\     let x = add(kek, lol)
    //\\    std.debug.print("x = {}\n", .{x})
    \\ }
    \\
;

test "lex into ast into semantic analysis" {
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
            std.debug.print("\n\nSementic Analysis:\n", .{});

            var semanticAnalyzer = try testing.allocator.create(SemanticAnalyzer);
            defer testing.allocator.destroy(semanticAnalyzer);
            semanticAnalyzer.* = try SemanticAnalyzer.init(
                std.heap.page_allocator,
                astParser.root.?,
            );
            defer semanticAnalyzer.deinit();
            _ = try semanticAnalyzer.analyze();

            semanticAnalyzer.print();
        }
    }
    std.debug.print("\n", .{});
}
