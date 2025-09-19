const std = @import("std");
const ast = @import("ast.zig");
const ModuleRegistry = @import("module_registry.zig");
const CIrCodegen = @import("codegen_c_ir.zig");
const SeaOfNodes = @import("sea_of_nodes_ir.zig").SeaOfNodes;
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;
const CompileError = @import("CompileError.zig").CompileError;

/// Multi-file C code generator that creates separate header and implementation files
pub const MultiFileCCodegen = struct {
    allocator: std.mem.Allocator,
    output_dir: []const u8,
    modules: std.ArrayList(*ModuleRegistry.Module),

    pub fn init(allocator: std.mem.Allocator, output_dir: []const u8) MultiFileCCodegen {
        return MultiFileCCodegen{
            .allocator = allocator,
            .output_dir = output_dir,
            .modules = std.ArrayList(*ModuleRegistry.Module).init(allocator),
        };
    }

    pub fn deinit(self: *MultiFileCCodegen) void {
        self.modules.deinit();
    }

    /// Generate C code for all modules in dependency order
    pub fn generateMultiFileC(
        self: *MultiFileCCodegen,
        registry: *ModuleRegistry.ModuleRegistry,
        main_ir: *SeaOfNodes,
        main_analyzer: *SemanticAnalyzer,
    ) CompileError!void {
        // Get modules in dependency order
        const ordered_modules = try registry.getModulesInDependencyOrder(self.allocator);
        defer self.allocator.free(ordered_modules);

        // Store modules for later use
        try self.modules.appendSlice(ordered_modules);

        // Ensure output directory exists
        std.fs.cwd().makeDir(self.output_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {}, // Directory exists, that's fine
            else => return error.FileSystemError,
        };

        // Generate code for each module
        for (ordered_modules) |module| {
            try self.generateModuleFiles(module);
        }

        // Generate main program files
        try self.generateMainFiles(main_ir, main_analyzer);
    }

    /// Generate header and implementation files for a module
    fn generateModuleFiles(self: *MultiFileCCodegen, module: *ModuleRegistry.Module) CompileError!void {
        const header_path = try self.getModuleHeaderPath(module);
        defer self.allocator.free(header_path);

        const impl_path = try self.getModuleImplPath(module);
        defer self.allocator.free(impl_path);

        // Generate header file
        try self.generateModuleHeader(module, header_path);

        // Generate implementation file
        try self.generateModuleImplementation(module, impl_path, header_path);
    }

    /// Generate main program files
    fn generateMainFiles(self: *MultiFileCCodegen, ir: *SeaOfNodes, analyzer: *SemanticAnalyzer) CompileError!void {
        const header_path = try std.fs.path.join(self.allocator, &[_][]const u8{ self.output_dir, "main.h" });
        defer self.allocator.free(header_path);

        const impl_path = try std.fs.path.join(self.allocator, &[_][]const u8{ self.output_dir, "main.c" });
        defer self.allocator.free(impl_path);

        // Generate main header
        try self.generateMainHeader(header_path);

        // Generate main implementation (using existing IR-based codegen)
        const c_code = try CIrCodegen.generateCFromIr(self.allocator, ir, analyzer);
        defer self.allocator.free(c_code);

        // Write main implementation with includes
        const main_with_includes = try self.addIncludesToMain(c_code);
        defer self.allocator.free(main_with_includes);

        try std.fs.cwd().writeFile(.{ .sub_path = impl_path, .data = main_with_includes });
    }

    /// Generate module header file
    fn generateModuleHeader(self: *MultiFileCCodegen, module: *ModuleRegistry.Module, header_path: []const u8) CompileError!void {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        const writer = buffer.writer();

        // Include guard
        const guard_name = try self.getIncludeGuardName(module);
        defer self.allocator.free(guard_name);

        try writer.print("#ifndef {s}\n", .{guard_name});
        try writer.print("#define {s}\n\n", .{guard_name});

        // Standard includes
        try writer.writeAll("#include <stdint.h>\n");
        try writer.writeAll("#include <stdbool.h>\n\n");

        // Include dependencies
        for (module.dependencies.items) |dep| {
            const dep_header = try self.getModuleHeaderName(dep);
            defer self.allocator.free(dep_header);
            try writer.print("#include \"{s}\"\n", .{dep_header});
        }

        if (module.dependencies.items.len > 0) {
            try writer.writeAll("\n");
        }

        // Type definitions
        try self.generateModuleTypes(writer, module);

        // Function declarations
        try self.generateModuleFunctionDeclarations(writer, module);

        // Close include guard
        try writer.print("\n#endif // {s}\n", .{guard_name});

        // Write to file
        try std.fs.cwd().writeFile(.{ .sub_path = header_path, .data = buffer.items });
    }

    /// Generate module implementation file
    fn generateModuleImplementation(_: *MultiFileCCodegen, _: *ModuleRegistry.Module, impl_path: []const u8, header_path: []const u8) CompileError!void {
        var buffer = std.ArrayList(u8).init(std.heap.page_allocator);
        defer buffer.deinit();

        const writer = buffer.writer();

        // Include header
        const header_name = try std.fs.path.basename(header_path);
        try writer.print("#include \"{s}\"\n\n", .{header_name});

        // TODO: Use module parameter for actual implementation

        // Function implementations (placeholder for now)
        try writer.writeAll("// Function implementations will be generated here\n");

        // Write to file
        try std.fs.cwd().writeFile(.{ .sub_path = impl_path, .data = buffer.items });
    }

    /// Generate main header file
    fn generateMainHeader(self: *MultiFileCCodegen, header_path: []const u8) CompileError!void {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        const writer = buffer.writer();

        // Include guard
        try writer.writeAll("#ifndef MAIN_H\n");
        try writer.writeAll("#define MAIN_H\n\n");

        // Include all module headers
        for (self.modules.items) |module| {
            const header_name = try self.getModuleHeaderName(module);
            defer self.allocator.free(header_name);
            try writer.print("#include \"{s}\"\n", .{header_name});
        }

        try writer.writeAll("\n#endif // MAIN_H\n");

        // Write to file
        try std.fs.cwd().writeFile(.{ .sub_path = header_path, .data = buffer.items });
    }

    /// Add includes to main C code
    fn addIncludesToMain(self: *MultiFileCCodegen, original_code: []const u8) CompileError![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        const writer = buffer.writer();

        // Add includes at the top
        try writer.writeAll("#include \"main.h\"\n\n");

        // Add the original code
        try writer.writeAll(original_code);

        return self.allocator.dupe(u8, buffer.items);
    }

    /// Generate type definitions for a module
    fn generateModuleTypes(_: *MultiFileCCodegen, writer: anytype, module: *ModuleRegistry.Module) CompileError!void {
        // Placeholder - will be implemented when we have proper type information
        _ = writer;
        _ = module;
        // TODO: Generate struct/enum/union definitions from module symbols
    }

    /// Generate function declarations for a module
    fn generateModuleFunctionDeclarations(_: *MultiFileCCodegen, writer: anytype, module: *ModuleRegistry.Module) CompileError!void {
        // Placeholder - will be implemented when we have proper function information
        _ = writer;
        _ = module;
        // TODO: Generate function prototypes from module symbols
    }

    /// Get header file path for a module
    fn getModuleHeaderPath(self: *MultiFileCCodegen, module: *ModuleRegistry.Module) ![]const u8 {
        const header_name = try std.mem.concat(self.allocator, u8, &[_][]const u8{ module.name, ".h" });
        defer self.allocator.free(header_name);

        return std.fs.path.join(self.allocator, &[_][]const u8{ self.output_dir, header_name });
    }

    /// Get implementation file path for a module
    fn getModuleImplPath(self: *MultiFileCCodegen, module: *ModuleRegistry.Module) ![]const u8 {
        const impl_name = try std.mem.concat(self.allocator, u8, &[_][]const u8{ module.name, ".c" });
        defer self.allocator.free(impl_name);

        return std.fs.path.join(self.allocator, &[_][]const u8{ self.output_dir, impl_name });
    }

    /// Get header file name for a module
    fn getModuleHeaderName(self: *MultiFileCCodegen, module: *ModuleRegistry.Module) ![]const u8 {
        return std.mem.concat(self.allocator, u8, &[_][]const u8{ module.name, ".h" });
    }

    /// Get include guard name for a module
    fn getIncludeGuardName(self: *MultiFileCCodegen, module: *ModuleRegistry.Module) ![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        // Convert module name to uppercase and add _H suffix
        for (module.name) |char| {
            if (char == '-' or char == '.') {
                try buffer.append('_');
            } else {
                try buffer.append(std.ascii.toUpper(char));
            }
        }
        try buffer.appendSlice("_H");

        return self.allocator.dupe(u8, buffer.items);
    }
};
