const std = @import("std");
const ast = @import("ast.zig");
const CompileError = @import("CompileError.zig").CompileError;
const ErrorSystem = @import("error_system.zig");
const ErrorFormatter = @import("error_formatter.zig");
const Lexer = @import("lexer_enhanced.zig");
const ParserModule = @import("parser.zig");
const Token = @import("token.zig").Token;
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;
const JSCodegen = @import("codegen_js.zig");
const CCodegen = @import("codegen_c.zig");

// Sea-of-nodes IR imports
const SeaOfNodes = @import("sea_of_nodes_ir.zig").SeaOfNodes;
const AstToIr = @import("ast_to_ir.zig");
const IrOptimizer = @import("ir_optimizer.zig");
const JsIrCodegen = @import("codegen_js_ir.zig");
const CIrCodegen = @import("codegen_c_ir.zig");

// ============================================================================
// Compilation Process Manager
// ============================================================================

pub const CompilePhase = enum {
    lexing,
    parsing,
    semantic_analysis,
    ir_construction,
    ir_optimization,
    codegen,
};

pub const CompileTarget = enum {
    c,
    javascript,
};

pub const CompileResult = struct {
    success: bool,
    generated_code: ?[]const u8,
    errors: ErrorSystem.ErrorCollector,
    warnings: usize,
    phase_completed: CompilePhase,
    target: CompileTarget,
    ir: ?SeaOfNodes, // Sea-of-nodes IR for debugging/introspection

    pub fn deinit(self: *CompileResult, allocator: std.mem.Allocator) void {
        // Clean up error collector
        self.errors.deinit();

        if (self.generated_code) |code| {
            allocator.free(code);
        }

        if (self.ir) |*ir| {
            ir.deinit();
        }
    }
};

pub const CompileOptions = struct {
    file_path: []const u8,
    source_content: []const u8,
    stop_on_first_error: bool = false,
    max_errors: usize = 50,
    enable_warnings: bool = true,
    output_format: OutputFormat = .colored_text,
    target: CompileTarget = .c,

    pub const OutputFormat = enum {
        colored_text,
        plain_text,
        json,
        summary_only,
    };
};

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    options: CompileOptions,
    arena: ast.AstArena,
    source_map: ErrorSystem.SourceMap,
    source_maps: std.StringHashMap(ErrorSystem.SourceMap),

    pub fn init(allocator: std.mem.Allocator, options: CompileOptions) CompileError!Compiler {
        var source_maps = std.StringHashMap(ErrorSystem.SourceMap).init(allocator);
        const source_map = try ErrorSystem.SourceMap.init(allocator, options.source_content);
        try source_maps.put(options.file_path, source_map);

        return Compiler{
            .allocator = allocator,
            .options = options,
            .arena = ast.AstArena.init(allocator),
            .source_map = source_map,
            .source_maps = source_maps,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.arena.deinit();

        // Deinit all source maps
        var iterator = self.source_maps.iterator();
        while (iterator.next()) |entry| {
            var source_map = entry.value_ptr;
            source_map.deinit();
        }
        self.source_maps.deinit();
    }

    // ============================================================================
    // Compilation Pipeline
    // ============================================================================

    pub fn compile(self: *Compiler) CompileError!CompileResult {
        // Use main allocator for error messages so they persist after compilation
        var errors = ErrorSystem.ErrorCollector.init(self.allocator);
        errors.max_errors = self.options.max_errors;

        var result = CompileResult{
            .success = false,
            .generated_code = null,
            .errors = errors,
            .warnings = 0,
            .phase_completed = .lexing,
            .target = self.options.target,
            .ir = null,
        };

        // Phase 1: Lexing
        const LexerEnhanced = @import("./lexer_enhanced.zig");
        var lexer = LexerEnhanced.Lexer.init(self.allocator);
        defer lexer.deinit();

        try lexer.addFile(self.options.file_path, self.options.source_content);
        try lexer.tokenizeAll();

        // Check for lexer errors and transfer them
        if (lexer.hasErrors()) {
            // Transfer lexer errors to our error collector (move ownership)
            for (lexer.error_collector.errors.items) |err| {
                try result.errors.errors.append(err);
            }
            // Clear the lexer's error list without deinitializing the errors
            lexer.error_collector.errors.clearRetainingCapacity();
        }

        // Get tokens from the lexer
        const lexer_file = lexer.files.get(self.options.file_path) orelse {
            _ = try result.errors.createAndAddError(.invalid_expression, .lexer, .error_, "Failed to get lexer file", ErrorSystem.SourceSpan.single(self.options.file_path, 0, 1, 1));
            return result;
        };
        const tokens = lexer_file.tokens.items;

        // Check for errors after lexing - stop compilation if any errors exist
        if (result.errors.hasErrors()) {
            return result;
        }

        std.debug.print("Lexing completed: {d} tokens\n", .{tokens.len});

        // Phase 2: Parsing
        result.phase_completed = .parsing;
        const ast_root = try self.parseTokens(tokens, &result.errors);

        // Check for errors after parsing - stop compilation if any errors exist
        if (result.errors.hasErrors()) {
            return result;
        }

        std.debug.print("Parsing completed: {d} AST nodes\n", .{ast_root.?});

        // Phase 3: Semantic Analysis
        result.phase_completed = .semantic_analysis;
        std.debug.print("Starting semantic analysis...\n", .{});
        var semantic_analyzer: ?SemanticAnalyzer = null;
        if (ast_root) |root| {
            semantic_analyzer = try self.performSemanticAnalysis(root, &result.errors);
        }

        // Check for errors after semantic analysis - stop compilation if any errors exist
        if (result.errors.hasErrors()) {
            std.debug.print("Semantic analysis failed with errors\n", .{});
            if (semantic_analyzer) |*analyzer| {
                analyzer.deinit();
            }
            return result;
        }
        std.debug.print("Semantic analysis completed successfully\n", .{});

        // Phase 3.5: Transform AST to Sea-of-Nodes IR
        result.phase_completed = .ir_construction;
        std.debug.print("Starting IR construction...\n", .{});
        var sea_of_nodes_ir: ?SeaOfNodes = null;
        if (ast_root) |root| {
            if (semantic_analyzer) |*analyzer| {
                sea_of_nodes_ir = AstToIr.transformAstToIr(
                    self.allocator,
                    root,
                    analyzer,
                    &self.arena,
                    &result.errors,
                ) catch |err| switch (err) {
                    error.OutOfMemory => return err,
                    else => {
                        std.debug.print("IR construction failed with error: {}\n", .{err});
                        // IR construction errors are already reported
                        analyzer.deinit();
                        return result;
                    },
                };
            }
        }

        // Check for errors after IR construction
        if (result.errors.hasErrors()) {
            std.debug.print("IR construction failed with errors\n", .{});
            if (semantic_analyzer) |*analyzer| {
                analyzer.deinit();
            }
            if (sea_of_nodes_ir) |*ir| {
                ir.deinit();
            }
            return result;
        }
        std.debug.print("IR construction completed successfully\n", .{});

        // Phase 3.6: IR Optimization (temporarily disabled for debugging)
        result.phase_completed = .ir_optimization;
        if (sea_of_nodes_ir) |*ir| {
            _ = ir; // TODO: Re-enable optimizations after fixing IR construction issues
        }

        // Phase 4: Code Generation from IR
        result.phase_completed = .codegen;
        if (sea_of_nodes_ir) |*ir| {
            if (semantic_analyzer) |*analyzer| {
                defer analyzer.deinit();

                // Store IR for debugging/introspection
                result.ir = ir.*;

                switch (self.options.target) {
                    .c => {
                        // Use the C backend with IR
                        const executable_result = try self.generateCExecutableFromIr(ir, analyzer, &result.errors);
                        result.generated_code = executable_result;
                    },
                    .javascript => {
                        // Use JavaScript backend with IR
                        const js_result = try self.generateJavaScriptFromIr(ir, analyzer, &result.errors);
                        result.generated_code = js_result;
                    },
                }
            }
        }

        // Set final result
        result.success = !result.errors.hasErrors();
        result.warnings = result.errors.warningCount();

        return result;
    }

    fn parseTokens(self: *Compiler, tokens: []const Token, errors: *ErrorSystem.ErrorCollector) CompileError!?ast.NodeId {
        var parser = ParserModule.Parser{
            .allocator = self.allocator,
            .tokens = tokens,
            .current = 0,
            .arena = &self.arena,
            .errors = errors,
            .source_map = &self.source_map, // Pass the source map instead of null
            .file_path = self.options.file_path,
            .panic_mode = false,
            .in_recovery = false,
            .recovery_tokens = &.{},
            .debug_mode = false,
            .recursion_depth = 0,
            .max_recursion_depth = 1000,
        };

        const ast_root = parser.parseProgram() catch {
            // Parser errors are already reported, just return null
            return null;
        };

        return ast_root;
    }

    fn performSemanticAnalysis(self: *Compiler, root_node: ast.NodeId, errors: *ErrorSystem.ErrorCollector) CompileError!SemanticAnalyzer {
        var analyzer = try SemanticAnalyzer.init(
            self.allocator,
            &self.arena,
            errors,
            self.options.file_path,
        );

        analyzer.analyzeProgram(root_node) catch |err| {
            switch (err) {
                error.OutOfMemory => return err,
                else => {
                    // Semantic errors are already reported
                },
            }
        };

        return analyzer;
    }

    fn generateJavaScriptFromIr(self: *Compiler, ir: *SeaOfNodes, analyzer: *const SemanticAnalyzer, errors: *ErrorSystem.ErrorCollector) CompileError![]const u8 {
        _ = errors; // TODO: Use this for codegen error reporting

        std.debug.print("Generating JavaScript from Sea-of-Nodes IR...\n", .{});

        // Use the new IR-based JavaScript code generator
        const js_code = JsIrCodegen.generateJavaScriptFromIr(
            self.allocator,
            ir,
            analyzer,
        ) catch |err| switch (err) {
            error.OutOfMemory => return err,
        };

        return js_code;
    }

    fn generateCExecutableFromIr(self: *Compiler, ir: *SeaOfNodes, analyzer: *const SemanticAnalyzer, errors: *ErrorSystem.ErrorCollector) CompileError![]const u8 {
        _ = errors; // TODO: Use this for codegen error reporting

        std.debug.print("Generating C code from Sea-of-Nodes IR...\n", .{});

        // Use the new IR-based C code generator with compilation
        const c_code = CIrCodegen.generateAndCompileCFromIr(
            self.allocator,
            ir,
            analyzer,
        ) catch |err| {
            switch (err) {
                error.OutOfMemory => return err,
                error.CCompilationFailed => {
                    // C compilation failed, but still return the generated C code
                    return CIrCodegen.generateCFromIr(self.allocator, ir, analyzer) catch {
                        return try self.allocator.dupe(u8, "/* C IR code generation failed */");
                    };
                },
                else => {
                    // TODO: Report codegen errors properly
                    return try self.allocator.dupe(u8, "/* C IR code generation failed */");
                },
            }
        };

        return c_code;
    }

    // ============================================================================
    // Error Reporting and Output
    // ============================================================================

    pub fn formatErrors(self: *Compiler, result: *const CompileResult, writer: anytype) CompileError!void {
        const formatter = ErrorFormatter.ErrorFormatter.init(self.allocator);

        switch (self.options.output_format) {
            .colored_text => {
                try formatter.formatErrors(&result.errors, &self.source_maps, writer);
            },
            .plain_text => {
                var plain_formatter = formatter;
                plain_formatter.use_colors = false;
                try plain_formatter.formatErrors(&result.errors, &self.source_maps, writer);
            },
            .json => {
                try formatter.formatErrorsJson(&result.errors, writer);
            },
            .summary_only => {
                try formatter.formatErrorsSummary(&result.errors, writer);
            },
        }
    }

    pub fn printCompilationSummary(self: *Compiler, result: *const CompileResult, writer: anytype) CompileError!void {
        const error_count = result.errors.errorCount();
        const warning_count = result.errors.warningCount();

        if (result.success) {
            try writer.print("✓ Compilation successful", .{});
            if (warning_count > 0) {
                try writer.print(" ({d} warning{s})", .{ warning_count, if (warning_count == 1) "" else "s" });
            }
            try writer.print("\n", .{});
        } else {
            try writer.print("✗ Compilation failed in phase: {s}\n", .{@tagName(result.phase_completed)});
            try writer.print("  {d} error{s}", .{ error_count, if (error_count == 1) "" else "s" });
            if (warning_count > 0) {
                try writer.print(", {d} warning{s}", .{ warning_count, if (warning_count == 1) "" else "s" });
            }
            try writer.print("\n", .{});
        }

        _ = self;
    }

    // ============================================================================
    // Diagnostics and Introspection
    // ============================================================================

    pub fn dumpIR(self: *Compiler, result: *const CompileResult, writer: anytype) CompileError!void {
        _ = self;
        if (result.ir) |*ir| {
            try writer.print("=== Sea-of-Nodes IR Dump ===\n", .{});
            try ir.dumpGraph(writer);
        } else {
            try writer.print("No IR available for dumping.\n", .{});
        }
    }

    pub fn dumpAST(self: *Compiler, root_node: ast.NodeId, writer: anytype, indent: usize) CompileError!void {
        const node = self.arena.getNode(root_node);
        if (node == null) {
            const spacing = try self.allocator.alloc(u8, indent);
            defer self.allocator.free(spacing);
            @memset(spacing, ' ');
            try writer.print("{s}[INVALID NODE]\n", .{spacing});

            return;
        }
        const spacing_buf = try self.allocator.alloc(u8, indent);
        defer self.allocator.free(spacing_buf);
        @memset(spacing_buf, ' ');
        const spacing = spacing_buf;
        const n = node.?;

        try writer.print("{s}{s} @ {s}:{d}:{d}", .{
            spacing,
            @tagName(n.data),
            n.source_loc.file_path,
            n.source_loc.line,
            n.source_loc.column,
        });

        if (n.type_info) |type_info| {
            const type_str = try type_info.toString(self.allocator);
            defer self.allocator.free(type_str);
            try writer.print(" : {s}", .{type_str});
        }

        try writer.print("\n", .{});

        // Print children
        switch (n.data) {
            .binary_expr => |binary| {
                try writer.print("{s}  op: {s}\n", .{ spacing, binary.op.toString() });
                try writer.print("{s}  left:\n", .{spacing});
                try self.dumpAST(binary.left, writer, indent + 4);
                try writer.print("{s}  right:\n", .{spacing});
                try self.dumpAST(binary.right, writer, indent + 4);
            },
            .unary_expr => |unary| {
                try writer.print("{s}  op: {s}\n", .{ spacing, unary.op.toString() });
                try writer.print("{s}  operand:\n", .{spacing});
                try self.dumpAST(unary.operand, writer, indent + 4);
            },
            .literal => |literal| {
                const literal_str = try literal.toString(self.allocator);
                defer self.allocator.free(literal_str);
                try writer.print("{s}  value: {s}\n", .{ spacing, literal_str });
            },
            .identifier => |ident| {
                try writer.print("{s}  name: {s}\n", .{ spacing, ident.name });
            },
            .block => |block| {
                try writer.print("{s}  statements ({d}):\n", .{ spacing, block.statements.items.len });
                for (block.statements.items) |stmt| {
                    try self.dumpAST(stmt, writer, indent + 4);
                }
            },
            .var_decl => |var_decl| {
                try writer.print("{s}  name: {s}\n", .{ spacing, var_decl.name });
                try writer.print("{s}  mutable: {}\n", .{ spacing, var_decl.is_mutable });
                if (var_decl.type_annotation) |type_node| {
                    try writer.print("{s}  type:\n", .{spacing});
                    try self.dumpAST(type_node, writer, indent + 4);
                }
                if (var_decl.initializer) |initializer| {
                    try writer.print("{s}  initializer:\n", .{spacing});
                    try self.dumpAST(initializer, writer, indent + 4);
                }
            },
            else => {
                // For other node types, just show basic info
            },
        }
    }

    pub fn getCompilationStats(self: *Compiler, result: *const CompileResult) CompilationStats {
        var ir_nodes: usize = 0;
        if (result.ir) |*ir| {
            ir_nodes = ir.nodes.items.len;
        }

        return CompilationStats{
            .total_ast_nodes = self.arena.nodes.items.len,
            .total_ir_nodes = ir_nodes,
            .total_errors = result.errors.errorCount(),
            .total_warnings = result.errors.warningCount(),
            .phase_completed = result.phase_completed,
            .success = result.success,
        };
    }
};

pub const CompilationStats = struct {
    total_ast_nodes: usize,
    total_ir_nodes: usize,
    total_errors: usize,
    total_warnings: usize,
    phase_completed: CompilePhase,
    success: bool,
};

/// Legacy compatibility - represents the compilation process, which includes lexing and parsing.
/// This struct holds the lexer and is responsible for managing the compilation process.
const Compilation = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    // ast: Ast, // Removed to avoid conflicts with new AST system
};
