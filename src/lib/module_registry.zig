const std = @import("std");
const ast = @import("ast.zig");
const ErrorSystem = @import("error_system.zig");

/// Information about a symbol exported by a module
pub const SymbolInfo = struct {
    name: []const u8,
    type_info: ast.Type,
    is_public: bool,
    defined_in: *Module,
};

/// Represents a loaded Howl module
pub const Module = struct {
    allocator: std.mem.Allocator,
    path: []const u8, // Full path to the module file
    name: []const u8, // Module name (derived from path)
    ast_root: ast.NodeId, // Root AST node of the module
    arena: ast.AstArena, // AST arena for this module (owned)
    exported_symbols: std.StringHashMap(SymbolInfo),
    dependencies: std.ArrayList(*Module),
    is_std_module: bool, // True for std library modules

    pub fn init(allocator: std.mem.Allocator, path: []const u8, name: []const u8, ast_root: ast.NodeId, arena: ast.AstArena) !*Module {
        const module = try allocator.create(Module);
        module.* = Module{
            .allocator = allocator,
            .path = try allocator.dupe(u8, path),
            .name = try allocator.dupe(u8, name),
            .ast_root = ast_root,
            .arena = arena,
            .exported_symbols = std.StringHashMap(SymbolInfo).init(allocator),
            .dependencies = std.ArrayList(*Module).init(allocator),
            .is_std_module = std.mem.startsWith(u8, path, "src/std/"),
        };
        return module;
    }

    pub fn deinit(self: *Module) void {
        self.arena.deinit();
        self.allocator.free(self.path);
        self.allocator.free(self.name);
        self.exported_symbols.deinit();
        self.dependencies.deinit();
        self.allocator.destroy(self);
    }

    /// Add a symbol to the module's exported symbols
    pub fn addExportedSymbol(self: *Module, name: []const u8, type_info: ast.Type, is_public: bool) !void {
        const symbol_info = SymbolInfo{
            .name = try self.allocator.dupe(u8, name),
            .type_info = type_info,
            .is_public = is_public,
            .defined_in = self,
        };
        try self.exported_symbols.put(name, symbol_info);
    }

    /// Get a symbol by name
    pub fn getSymbol(self: *const Module, name: []const u8) ?SymbolInfo {
        return self.exported_symbols.get(name);
    }

    /// Add a dependency on another module
    pub fn addDependency(self: *Module, dependency: *Module) !void {
        // Check for circular dependency
        if (self.hasCircularDependency(dependency)) {
            return error.CircularDependency;
        }
        try self.dependencies.append(dependency);
    }

    /// Check if adding this dependency would create a circular dependency
    fn hasCircularDependency(self: *const Module, potential_dep: *const Module) bool {
        // Check if potential_dep depends on self (directly or indirectly)
        for (potential_dep.dependencies.items) |dep| {
            if (dep == self) return true;
            if (self.hasCircularDependency(dep)) return true;
        }
        return false;
    }
};

/// Registry for managing loaded modules and their dependencies
pub const ModuleRegistry = struct {
    allocator: std.mem.Allocator,
    modules: std.StringHashMap(*Module),
    module_stack: std.ArrayList(*Module), // For circular dependency detection

    pub fn init(allocator: std.mem.Allocator) ModuleRegistry {
        return ModuleRegistry{
            .allocator = allocator,
            .modules = std.StringHashMap(*Module).init(allocator),
            .module_stack = std.ArrayList(*Module).init(allocator),
        };
    }

    pub fn deinit(self: *ModuleRegistry) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.modules.deinit();
        self.module_stack.deinit();
    }

    /// Load or get a cached module by path
    pub fn getOrLoadModule(self: *ModuleRegistry, path: []const u8, loader: anytype) !*Module {
        // Check cache first
        if (self.modules.get(path)) |cached| {
            std.debug.print("DEBUG: Module {s} found in cache\n", .{path});
            return cached;
        }
        std.debug.print("DEBUG: Module {s} not in cache, loading...\n", .{path});

        // Check for circular dependency
        for (self.module_stack.items) |stack_module| {
            if (std.mem.eql(u8, stack_module.path, path)) {
                return error.CircularDependency;
            }
        }

        // Load the module
        const module = try loader.loadModule(path);

        // Analyze the module to extract exported symbols
        // Create a temporary error collector for module analysis
        var temp_errors = ErrorSystem.ErrorCollector.init(loader.allocator);
        defer temp_errors.deinit();

        std.debug.print("DEBUG: REGISTRY: About to analyze module {s}\n", .{path});
        loader.analyzeModule(module, &temp_errors) catch |err| {
            std.debug.print("DEBUG: REGISTRY: Module analysis failed with error: {}\n", .{err});
            // Don't fail the module loading, just continue without exported symbols
        };
        std.debug.print("DEBUG: REGISTRY: Module analysis completed for {s}\n", .{path});

        // Check for analysis errors
        if (temp_errors.hasErrors()) {
            std.debug.print("DEBUG: REGISTRY: Module analysis had {d} errors\n", .{temp_errors.errors.items.len});
            // Don't fail, just log the errors
        }

        std.debug.print("DEBUG: REGISTRY: Module {s} has {d} exported symbols\n", .{ module.name, module.exported_symbols.count() });

        try self.modules.put(path, module);

        return module;
    }

    /// Get a module by path (returns null if not loaded)
    pub fn getModule(self: *const ModuleRegistry, path: []const u8) ?*Module {
        return self.modules.get(path);
    }

    /// Get all loaded modules
    pub fn getAllModules(self: *const ModuleRegistry) []const *Module {
        return self.modules.values();
    }

    /// Get modules in dependency order (topological sort)
    pub fn getModulesInDependencyOrder(self: *ModuleRegistry, allocator: std.mem.Allocator) ![]*Module {
        var result = std.ArrayList(*Module).init(allocator);
        var visited = std.StringHashMap(void).init(allocator);
        defer visited.deinit();

        var visiting = std.StringHashMap(void).init(allocator);
        defer visiting.deinit();

        // Visit all modules
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            try self.visitModule(entry.value_ptr.*, &result, &visited, &visiting);
        }

        return result.toOwnedSlice();
    }

    fn visitModule(self: *ModuleRegistry, module: *Module, result: *std.ArrayList(*Module), visited: *std.StringHashMap(void), visiting: *std.StringHashMap(void)) !void {
        // Skip if already visited
        if (visited.contains(module.path)) return;

        // Check for circular dependency
        if (visiting.contains(module.path)) {
            return error.CircularDependency;
        }

        try visiting.put(module.path, {});

        // Visit dependencies first
        for (module.dependencies.items) |dep| {
            try self.visitModule(dep, result, visited, visiting);
        }

        _ = visiting.remove(module.path);
        try visited.put(module.path, {});
        try result.append(module);
    }
};
