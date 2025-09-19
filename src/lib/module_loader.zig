const std = @import("std");
const ast = @import("ast.zig");
const ErrorSystem = @import("error_system.zig");
const ModuleRegistry = @import("module_registry.zig");
const LexerEnhanced = @import("lexer_enhanced.zig");
const ParserModule = @import("parser.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;
const CompileError = @import("CompileError.zig").CompileError;

/// Module loader responsible for loading and parsing external Howl files
pub const ModuleLoader = struct {
    allocator: std.mem.Allocator,
    registry: *ModuleRegistry.ModuleRegistry,
    search_paths: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, registry: *ModuleRegistry.ModuleRegistry) ModuleLoader {
        return ModuleLoader{
            .allocator = allocator,
            .registry = registry,
            .search_paths = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *ModuleLoader) void {
        self.search_paths.deinit();
    }

    /// Add a search path for module resolution
    pub fn addSearchPath(self: *ModuleLoader, path: []const u8) !void {
        const duped = try self.allocator.dupe(u8, path);
        try self.search_paths.append(duped);
    }

    /// Load a module from a file path (without caching)
    pub fn loadModule(self: *ModuleLoader, module_path: []const u8) !*ModuleRegistry.Module {
        // std.debug.print("DEBUG: loadModule called with path '{s}'\n", .{module_path});
        // Resolve the full path
        const resolved_path = try self.resolveModulePath(module_path);
        defer self.allocator.free(resolved_path);
        // std.debug.print("DEBUG: resolved path '{s}'\n", .{resolved_path});

        // Read the file content
        const source_content = try std.fs.cwd().readFileAlloc(self.allocator, resolved_path, std.math.maxInt(usize));
        defer self.allocator.free(source_content);

        // Extract module name from path
        const module_name = try self.extractModuleName(resolved_path);
        defer self.allocator.free(module_name);

        // Create the arena
        var arena = ast.AstArena.init(self.allocator);
        errdefer arena.deinit();

        // Parse the module
        const ast_root = try self.parseModule(resolved_path, source_content, &arena, resolved_path);

        // Create the module (transfer ownership of arena)
        const module = try ModuleRegistry.Module.init(self.allocator, resolved_path, module_name, ast_root, arena);

        return module;
    }

    /// Load a module with caching via the registry
    pub fn getOrLoadModule(self: *ModuleLoader, module_path: []const u8) !*ModuleRegistry.Module {
        return self.registry.getOrLoadModule(module_path, self);
    }

    /// Resolve a module path to an absolute file path
    fn resolveModulePath(self: *ModuleLoader, module_path: []const u8) ![]const u8 {
        // std.debug.print("DEBUG: resolveModulePath called with '{s}'\n", .{module_path});
        // Handle special std module
        if (std.mem.eql(u8, module_path, "std")) {
            return try self.allocator.dupe(u8, "src/std.howl");
        }

        // Check if it's already an absolute path
        if (std.fs.path.isAbsolute(module_path)) {
            return try self.allocator.dupe(u8, module_path);
        }

        // Try different extensions and search paths
        const extensions = [_][]const u8{ ".howl", "" };

        for (self.search_paths.items) |search_path| {
            for (extensions) |ext| {
                const full_path = try std.fs.path.join(self.allocator, &[_][]const u8{ search_path, module_path });
                defer self.allocator.free(full_path);

                var test_path = full_path;
                if (ext.len > 0) {
                    test_path = try std.mem.concat(self.allocator, u8, &[_][]const u8{ full_path, ext });
                    defer self.allocator.free(test_path);
                }

                // Check if file exists
                if (std.fs.cwd().access(test_path, .{})) |_| {
                    return try self.allocator.dupe(u8, test_path);
                } else |_| {
                    // File doesn't exist, continue
                }
            }
        }

        // Try relative to current working directory
        for (extensions) |ext| {
            var test_path = module_path;
            if (ext.len > 0) {
                test_path = try std.mem.concat(self.allocator, u8, &[_][]const u8{ module_path, ext });
                defer self.allocator.free(test_path);
            }

            if (std.fs.cwd().access(test_path, .{})) |_| {
                return try self.allocator.dupe(u8, test_path);
            } else |_| {
                // Continue
            }
        }

        return error.ModuleNotFound;
    }

    /// Extract module name from file path
    fn extractModuleName(self: *ModuleLoader, path: []const u8) ![]const u8 {
        const basename = std.fs.path.basename(path);
        const ext = std.fs.path.extension(basename);

        if (std.mem.eql(u8, ext, ".howl")) {
            return try self.allocator.dupe(u8, basename[0 .. basename.len - ext.len]);
        }

        return try self.allocator.dupe(u8, basename);
    }

    /// Parse a module's source code
    fn parseModule(self: *ModuleLoader, file_path: []const u8, source_content: []const u8, arena: *ast.AstArena, resolved_path: []const u8) !ast.NodeId {
        // Initialize lexer
        var lexer = LexerEnhanced.Lexer.init(self.allocator);
        defer lexer.deinit();

        try lexer.addFile(file_path, source_content);

        // Tokenize
        try lexer.tokenizeAll();

        // Check for lexer errors
        if (lexer.hasErrors()) {
            // Convert lexer errors to our error format
            return error.LexerError;
        }

        // Get tokens
        const file = lexer.files.get(file_path) orelse return error.FileNotFound;
        const tokens = file.tokens.items;

        // Create error collector for parsing
        var parse_errors = ErrorSystem.ErrorCollector.init(self.allocator);
        defer parse_errors.deinit();

        // Create source map
        const source_map = try ErrorSystem.SourceMap.init(self.allocator, source_content);

        // Parse
        var parser = ParserModule.Parser.init(
            self.allocator,
            tokens,
            arena,
            &parse_errors,
            resolved_path,
            &source_map,
        );

        const ast_root = try parser.parseProgram();

        // Check for parse errors
        if (parse_errors.hasErrors()) {
            return error.ParseError;
        }

        return ast_root;
    }

    /// Analyze a module semantically
    pub fn analyzeModule(self: *ModuleLoader, module: *ModuleRegistry.Module, error_collector: anytype) !void {
        var analyzer = try SemanticAnalyzer.init(self.allocator, &module.arena, error_collector, module.path);
        defer analyzer.deinit();

        // Set the module context
        analyzer.current_module = module;

        // Analyze the module
        _ = try analyzer.analyzeProgram(module.ast_root);

        // Extract exported symbols
        try self.extractExportedSymbols(module, &analyzer);
    }

    /// Extract exported symbols from a module
    fn extractExportedSymbols(self: *ModuleLoader, module: *ModuleRegistry.Module, analyzer: *SemanticAnalyzer) !void {
        // Walk the AST and find all top-level declarations
        const root_node = module.arena.getNode(module.ast_root) orelse return;

        switch (root_node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    const stmt_node = module.arena.getNode(stmt_id) orelse continue;

                    switch (stmt_node.data) {
                        .var_decl => |var_decl| {
                            // Check if it's a public declaration
                            const is_public = self.isPublicDeclaration(var_decl, &module.arena);
                            if (is_public) {
                                // Get the variable type from the analyzer
                                if (analyzer.current_scope.lookup(var_decl.name)) |symbol| {
                                    const var_type = symbol.declared_type orelse symbol.inferred_type orelse continue;
                                    try module.addExportedSymbol(var_decl.name, var_type, true);
                                }
                            }
                        },
                        .function_decl => |func_decl| {
                            // Check if it's a public declaration
                            const is_public = self.isPublicDeclaration(func_decl, &module.arena);
                            if (is_public) {
                                // Create function type
                                const func_type = try self.createFunctionType(func_decl, analyzer, stmt_node.source_loc);
                                try module.addExportedSymbol(func_decl.name, func_type, true);
                            }
                        },
                        else => {
                            // Skip other declarations for now
                        },
                    }
                }
            },
            else => {
                // Root should be a block - report error but don't fail
                // This allows the module to still be loaded with no exported symbols
                return;
            },
        }
    }

    /// Check if a declaration is public by looking for 'pub' keyword
    fn isPublicDeclaration(_: *ModuleLoader, decl: anytype, _: *ast.AstArena) bool {
        // Try to find the AST node that contains this declaration
        // For now, we'll use a simple heuristic: check if the name starts with a capital letter
        // This is a temporary solution until the parser properly handles 'pub' keywords

        if (@hasField(@TypeOf(decl), "name")) {
            const name = decl.name;
            if (name.len > 0 and std.ascii.isUpper(name[0])) {
                return true;
            }
        }

        // For a more robust implementation, we would need to:
        // 1. Find the source location of the declaration
        // 2. Look backwards in the source code for a 'pub' keyword
        // 3. Check if it's properly positioned before the declaration

        // For now, since the parser doesn't store pub information,
        // we'll assume all declarations in imported modules are public
        // unless they start with an underscore (private convention)
        if (@hasField(@TypeOf(decl), "name")) {
            const name = decl.name;
            if (name.len > 0 and name[0] == '_') {
                return false; // Names starting with _ are private
            }
        }

        return true; // Default to public
    }

    /// Create a function type from a function declaration
    fn createFunctionType(self: *ModuleLoader, func_decl: anytype, analyzer: *SemanticAnalyzer, source_loc: ast.SourceLoc) !ast.Type {
        // Extract parameter types
        var param_types = std.ArrayList(ast.Type).init(self.allocator);
        defer param_types.deinit();

        for (func_decl.params.items) |param| {
            if (param.type_annotation) |type_node_id| {
                // Use the analyzer to infer the type from the type annotation
                if (try analyzer.inferType(type_node_id)) |param_type| {
                    try param_types.append(param_type);
                } else {
                    // If we can't infer the type, default to i32
                    try param_types.append(ast.Type.initPrimitive(.{ .i32 = {} }, source_loc));
                }
            } else {
                // If no type annotation, default to i32
                try param_types.append(ast.Type.initPrimitive(.{ .i32 = {} }, source_loc));
            }
        }

        // Extract return type
        var return_type: ast.Type = undefined;
        if (func_decl.return_type) |return_type_node_id| {
            if (try analyzer.inferType(return_type_node_id)) |inferred_return_type| {
                return_type = inferred_return_type;
            } else {
                // Default to i32 if we can't infer
                return_type = ast.Type.initPrimitive(.{ .i32 = {} }, source_loc);
            }
        } else {
            // Default to void if no return type specified
            return_type = ast.Type.initPrimitive(.{ .void = {} }, source_loc);
        }

        // Create the function type
        const func_type = try self.allocator.create(ast.Type);
        const param_types_slice = try self.allocator.dupe(ast.Type, param_types.items);
        const return_type_ptr = try self.allocator.create(ast.Type);
        return_type_ptr.* = return_type;

        func_type.* = ast.Type{
            .data = .{
                .function = .{
                    .param_types = param_types_slice,
                    .return_type = return_type_ptr,
                },
            },
            .source_loc = source_loc,
        };

        return func_type.*;
    }
};
