const std = @import("std");
const testing = std.testing;
const Lexer = @import("lexer.zig").Lexer;
const Ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analysis.zig").SemanticAnalyzer;
const Codegen = @import("codegen_js.zig").CodeGenerator;

const codegentest =
    \\ 
    \\ 
    \\ var x = 5
    \\ x = x + 10
    \\ let y = 10
    \\ fn add(a: i32, b: i32) i32 {
    \\     return a + b
    \\ }
    \\
    \\ let d = add(x, y)
    \\ 
;

test "codegen" {
    const source = try testing.allocator.alloc(u8, codegentest.len);
    @memcpy(source, codegentest);

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
        std.debug.print("\n\nCodegen:\n", .{});

        var semanticAnalyzer = try testing.allocator.create(SemanticAnalyzer);
        defer testing.allocator.destroy(semanticAnalyzer);
        semanticAnalyzer.* = try SemanticAnalyzer.init(
            std.heap.page_allocator,
            astParser.root.?,
        );
        defer semanticAnalyzer.deinit();
        _ = try semanticAnalyzer.analyze();
        semanticAnalyzer.print();
        var codegen = try Codegen.init(testing.allocator, semanticAnalyzer);
        defer codegen.deinit();

        std.debug.print("\n\nCodegen:\n", .{});

        try codegen.generate();
        const output = codegen.getResult();
        std.debug.print("Generated JS code:\n{s}\n", .{output});
    }
}
