const std = @import("std");
const ast = @import("ast.zig");
const ErrorSystem = @import("error_system.zig");
const ErrorFormatter = @import("error_formatter.zig");
const Lexer = @import("lexer_enhanced.zig");
const ParserModule = @import("parser.zig");
const Token = @import("token.zig").Token;
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;
const JSCodegen = @import("codegen_js.zig");
const NativeCodegen = @import("codegen_native.zig");
const CCodegen = @import("codegen_c.zig");

// ============================================================================
// Compilation Process Manager
// ============================================================================

pub const CompilePhase = enum {
    lexing,
    parsing,
    semantic_analysis,
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

    pub fn deinit(self: *CompileResult, allocator: std.mem.Allocator) void {
        // Clean up error collector
        self.errors.deinit();

        if (self.generated_code) |code| {
            allocator.free(code);
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

    pub fn init(allocator: std.mem.Allocator, options: CompileOptions) !Compiler {
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

    pub fn compile(self: *Compiler) !CompileResult {
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

        if (result.errors.hasErrors() and self.options.stop_on_first_error) {
            return result;
        }

        std.debug.print("Lexing completed: {d} tokens\n", .{tokens.len});

        // Phase 2: Parsing
        result.phase_completed = .parsing;
        const ast_root = try self.parseTokens(tokens, &result.errors);

        if (result.errors.hasErrors() and self.options.stop_on_first_error) {
            return result;
        }

        std.debug.print("Parsing completed: {d} AST nodes\n", .{ast_root.?});

        // Phase 3: Semantic Analysis
        result.phase_completed = .semantic_analysis;
        var semantic_analyzer: ?SemanticAnalyzer = null;
        if (ast_root) |root| {
            semantic_analyzer = try self.performSemanticAnalysis(root, &result.errors);
        }

        if (result.errors.hasErrors() and self.options.stop_on_first_error) {
            if (semantic_analyzer) |*analyzer| {
                analyzer.deinit();
            }
            return result;
        }

        // Phase 4: Code Generation
        result.phase_completed = .codegen;
        if (ast_root) |root| {
            if (semantic_analyzer) |*analyzer| {
                defer analyzer.deinit();

                switch (self.options.target) {
                    .c => {
                        // Use the C backend
                        const executable_result = try self.generateCExecutable(root, analyzer, &result.errors);
                        result.generated_code = executable_result;
                    },
                    .javascript => {
                        // Use JavaScript backend if available
                        const js_result = try self.generateJavaScript(root, analyzer, &result.errors);
                        result.generated_code = js_result;
                    },
                }
            }
        }

        // Set final result
        // For native targets, success is determined by whether the binary was created
        if (self.options.target == .c or self.options.target == .javascript) {
            // Check if the binary was successfully created
            std.fs.cwd().access("howl_output", .{}) catch {
                result.success = false;
                return result;
            };
            result.success = true;
        } else {
            result.success = !result.errors.hasErrors();
        }
        result.warnings = result.errors.warningCount();

        return result;
    }

    fn parseTokens(self: *Compiler, tokens: []const Token, errors: *ErrorSystem.ErrorCollector) !?ast.NodeId {
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

    fn performSemanticAnalysis(self: *Compiler, root_node: ast.NodeId, errors: *ErrorSystem.ErrorCollector) !SemanticAnalyzer {
        var analyzer = SemanticAnalyzer.init(
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

    fn generateJavaScript(self: *Compiler, root_node: ast.NodeId, analyzer: *const SemanticAnalyzer, errors: *ErrorSystem.ErrorCollector) ![]const u8 {
        _ = errors; // TODO: Use this for codegen error reporting

        const js_code = JSCodegen.generateJS(
            self.allocator,
            &self.arena,
            analyzer,
            root_node,
        ) catch |err| {
            switch (err) {
                error.OutOfMemory => return err,
                else => {
                    // TODO: Report codegen errors properly
                    return try self.allocator.dupe(u8, "/* JavaScript code generation failed */");
                },
            }
        };

        return js_code;
    }

    fn generateCode(self: *Compiler, root_node: ast.NodeId, analyzer: *const SemanticAnalyzer, errors: *ErrorSystem.ErrorCollector) ![]const u8 {
        // Legacy method - now delegates to generateJavaScript
        return self.generateJavaScript(root_node, analyzer, errors);
    }

    // fn generateRealZir(self: *Compiler, root_node: ast.NodeId, analyzer: *const SemanticAnalyzer, errors: *ErrorSystem.ErrorCollector) !std.zig.Zir {
    //     _ = errors; // TODO: Use this for codegen error reporting
    //
    //     var zir_generator = ZirGenerator.ZirGenerator.init(self.allocator, &self.arena, analyzer);
    //     defer zir_generator.deinit();
    //
    //     const zir = zir_generator.generate(root_node) catch |err| {
    //         switch (err) {
    //             error.OutOfMemory => return err,
    //             else => {
    //                 // TODO: Report codegen errors properly
    //                 // For now return an empty ZIR structure
    //                 var empty_instructions = std.MultiArrayList(std.zig.Zir.Inst){};
    //                 var empty_string_bytes = [_]u8{0};
    //                 var empty_extra = [_]u32{0, 0};
    //                 return std.zig.Zir{
    //                     .instructions = empty_instructions.toOwnedSlice(),
    //                     .string_bytes = empty_string_bytes[0..],
    //                     .extra = empty_extra[0..],
    //                 };
    //             },
    //         }
    //     };

    //     return zir;
    // }

    fn generateNativeExecutable(self: *Compiler, root_node: ast.NodeId, analyzer: *const SemanticAnalyzer, errors: *ErrorSystem.ErrorCollector) ![]const u8 {
        _ = errors; // TODO: Use this for codegen error reporting

        var native_codegen = NativeCodegen.NativeCodegen.init(self.allocator, &self.arena, analyzer);
        defer native_codegen.deinit();

        const result = native_codegen.generate(root_node) catch |err| {
            switch (err) {
                error.OutOfMemory => return err,
                else => {
                    // TODO: Report codegen errors properly
                    return try self.allocator.dupe(u8, "/* Native executable generation failed */");
                },
            }
        };

        return result;
    }

    fn generateCExecutable(self: *Compiler, root_node: ast.NodeId, analyzer: *const SemanticAnalyzer, errors: *ErrorSystem.ErrorCollector) ![]const u8 {
        _ = errors; // TODO: Use this for codegen error reporting

        var c_codegen = CCodegen.CCodegen.init(self.allocator, &self.arena, analyzer);
        defer c_codegen.deinit();

        const result = c_codegen.generate(root_node) catch |err| {
            return err;
        };

        return result;
    }

    // ============================================================================
    // Error Reporting and Output
    // ============================================================================

    pub fn formatErrors(self: *Compiler, result: *const CompileResult, writer: anytype) !void {
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

    pub fn printCompilationSummary(self: *Compiler, result: *const CompileResult, writer: anytype) !void {
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

    pub fn dumpAST(self: *Compiler, root_node: ast.NodeId, writer: anytype, indent: usize) !void {
        const node = self.arena.getNodeConst(root_node);
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
        return CompilationStats{
            .total_nodes = self.arena.nodes.items.len,
            .total_errors = result.errors.errorCount(),
            .total_warnings = result.errors.warningCount(),
            .phase_completed = result.phase_completed,
            .success = result.success,
        };
    }
};

pub const CompilationStats = struct {
    total_nodes: usize,
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
