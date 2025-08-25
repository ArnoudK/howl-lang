//! Howl Language Compiler Library
//! This is the root module that exports all compiler functionality.

const std = @import("std");
const testing = std.testing;

// Export legacy functionality for compatibility
pub export fn add(a: i32, b: i32) i32 {
    return a + b;
}

// Export the modern compiler modules
pub const ast = @import("lib/ast.zig");
pub const ErrorSystem = @import("lib/error_system.zig");
pub const ErrorFormatter = @import("lib/error_formatter.zig");
pub const Token = @import("lib/token.zig");
pub const Parser = @import("lib/parser.zig");
pub const SemanticAnalyzer = @import("lib/semantic_analyzer.zig");
pub const CompileProcess = @import("lib/compile_process.zig");
pub const Lsp = @import("lib/lsp.zig");
pub const LspServer = @import("lib/lsp_server.zig");

// Export Sea-of-Nodes IR modules
pub const SeaOfNodes = @import("lib/sea_of_nodes_ir.zig");
pub const AstToIr = @import("lib/ast_to_ir.zig");
pub const IrOptimizer = @import("lib/ir_optimizer.zig");
pub const JsIrCodegen = @import("lib/codegen_js_ir.zig");
pub const CIrCodegen = @import("lib/codegen_c_ir.zig");

// Export legacy modules for backward compatibility
pub const LexerEnhanced = @import("lib/lexer_enhanced.zig");
pub const SemanticErrors = @import("lib/semantic_errors.zig");

// Export commonly used types
pub const Compiler = CompileProcess.Compiler;
pub const CompileOptions = CompileProcess.CompileOptions;
pub const CompileTarget = CompileProcess.CompileTarget;
pub const CompileResult = CompileProcess.CompileResult;
pub const AstArena = ast.AstArena;
pub const NodeId = ast.NodeId;
pub const SourceLoc = ast.SourceLoc;

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}

test "modern AST system" {
    var arena = ast.AstArena.init(testing.allocator);
    defer arena.deinit();

    // Test creating a literal
    const literal = ast.Literal{ .integer = .{ .value = 42 } };
    const source_loc = ast.SourceLoc.invalid();
    const node_id = try ast.createLiteralExpr(&arena, source_loc, literal);

    // Verify node was created correctly
    const node = arena.getNode(node_id).?;
    try testing.expect(node.data == .literal);
    try testing.expect(node.data.literal.integer.value == 42);
}

test "error system integration" {
    var collector = ErrorSystem.ErrorCollector.init(testing.allocator);
    defer collector.deinit();

    const source_span = ErrorSystem.SourceSpan.single("test.howl", 10, 1, 5);

    // Create an error
    _ = try collector.createAndAddError(
        .type_mismatch,
        .semantic,
        .error_,
        "Test error message",
        source_span,
    );

    // Verify error was collected
    try testing.expect(collector.hasErrors());
    try testing.expect(collector.errorCount() == 1);
}
