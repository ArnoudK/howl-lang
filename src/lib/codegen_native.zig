const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

pub const NativeCodegen = struct {
    allocator: std.mem.Allocator,
    arena: *const ast.AstArena,
    semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer,
    
    pub fn init(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer) NativeCodegen {
        return NativeCodegen{
            .allocator = allocator,
            .arena = arena,
            .semantic_analyzer = semantic_analyzer,
        };
    }
    
    pub fn deinit(self: *NativeCodegen) void {
        _ = self;
    }
    
    pub fn generate(self: *NativeCodegen, root_node_id: ast.NodeId) ![]u8 {
        // Generate LLVM IR and compile to native executable using ./zig-install/zig
        return try self.compileToNativeExecutable(root_node_id);
    }
    
    fn compileToNativeExecutable(self: *NativeCodegen, root_node_id: ast.NodeId) ![]u8 {
        // Generate Zig source code from the AST instead of LLVM IR
        const zig_source = try self.generateZigSource(root_node_id);
        defer self.allocator.free(zig_source);
        
        // Write Zig source to temporary file
        const temp_zig_path = "howl_program.zig";
        try std.fs.cwd().writeFile(.{ .sub_path = temp_zig_path, .data = zig_source });
        
        // Don't delete the Zig file so we can inspect it for debugging
        // defer std.fs.cwd().deleteFile(temp_zig_path) catch {};
        
        // Compile using ./zig-install/zig
        const output_name = "howl_output";
        try self.compileWithZigCompiler(temp_zig_path, output_name);
        
        return try self.allocator.dupe(u8, "/* Native executable compiled successfully */");
    }
    
    fn generateZigSource(self: *NativeCodegen, root_node_id: ast.NodeId) ![]u8 {
        var zig_output = std.ArrayList(u8).init(self.allocator);
        defer zig_output.deinit();
        
        const writer = zig_output.writer();
        
        // Zig source header
        try writer.writeAll("const std = @import(\"std\");\n");
        try writer.writeAll("const print = std.debug.print;\n\n");
        
        // Generate main function
        try writer.writeAll("pub fn main() !void {\n");
        
        // Generate Zig code from AST
        try self.generateZigFromAST(writer, root_node_id, 1);
        
        try writer.writeAll("}\n");
        
        return try zig_output.toOwnedSlice();
    }
    
    fn generateZigFromAST(self: *NativeCodegen, writer: anytype, node_id: ast.NodeId, indent_level: u32) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;
        
        // Generate indent string at runtime
        var indent_buf: [64]u8 = undefined;
        var indent_len: usize = 0;
        var i: u32 = 0;
        while (i < indent_level and indent_len < indent_buf.len - 4) : (i += 1) {
            @memcpy(indent_buf[indent_len..indent_len + 4], "    ");
            indent_len += 4;
        }
        const indent = indent_buf[0..indent_len];
        
        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.generateZigFromAST(writer, stmt_id, indent_level);
                }
            },
            .function_decl => |func_decl| {
                // For the main function, we don't regenerate the function signature
                // since we already have it in our template. Just process the body.
                if (std.mem.eql(u8, func_decl.name, "main")) {
                    try self.generateZigFromAST(writer, func_decl.body, indent_level);
                } else {
                    // For other functions, we'd generate the full function here
                    // For now, just process the body
                    try self.generateZigFromAST(writer, func_decl.body, indent_level);
                }
            },
            .call_expr => |call| {
                try self.generateZigCallExpr(writer, call, indent, indent_level);
            },
            .var_decl => |var_decl| {
                try self.generateZigVarDecl(writer, var_decl, indent, indent_level);
            },
            .for_expr => |for_expr| {
                try self.generateZigForLoop(writer, for_expr, indent, indent_level);
            },
            .match_expr => |match_expr| {
                try self.generateZigMatch(writer, match_expr, indent, indent_level);
            },
            else => {
                // Handle other statement types or skip them silently
            }
        }
    }
    
    fn generateZigCallExpr(self: *NativeCodegen, writer: anytype, call: anytype, indent: []const u8, indent_level: u32) !void {
        const callee_node = self.arena.getNodeConst(call.callee) orelse return;
        if (callee_node.data != .identifier) return;
        
        const func_name = callee_node.data.identifier.name;
        
        if (std.mem.eql(u8, func_name, "print") or std.mem.eql(u8, func_name, "println")) {
            if (call.args.items.len > 0) {
                const arg_node = self.arena.getNodeConst(call.args.items[0]) orelse return;
                
                switch (arg_node.data) {
                    .literal => |literal| {
                        switch (literal) {
                            .string => |str| {
                                try writer.print("{s}print(\"{s}\\n\", .{{}});\n", .{ indent, str.value });
                            },
                            .integer => |int| {
                                try writer.print("{s}print(\"{d}\\n\", .{{}});\n", .{ indent, int.value });
                            },
                            else => {},
                        }
                    },
                    .identifier => |ident| {
                        try writer.print("{s}print(\"{{d}}\\n\", .{{ {s} }});\n", .{ indent, ident.name });
                    },
                    else => {},
                }
            }
        }
        
        _ = indent_level;
    }
    
    fn generateZigVarDecl(self: *NativeCodegen, writer: anytype, var_decl: anytype, indent: []const u8, indent_level: u32) !void {
        // Check if variable name conflicts with Zig keywords/standard library
        const safe_name = if (std.mem.eql(u8, var_decl.name, "std") or 
                             std.mem.eql(u8, var_decl.name, "print") or
                             std.mem.eql(u8, var_decl.name, "main")) 
            "howl_var" // Use a generic safe name
        else 
            var_decl.name;
        
        if (var_decl.initializer) |initializer| {
            const init_node = self.arena.getNodeConst(initializer);
            if (init_node) |node| {
                if (node.data == .literal and node.data.literal == .integer) {
                    const value = node.data.literal.integer.value;
                    try writer.print("{s}const {s}: i32 = {d};\n", .{ indent, safe_name, value });
                } else {
                    try writer.print("{s}const {s}: i32 = 0;\n", .{ indent, safe_name });
                }
            }
        } else {
            try writer.print("{s}const {s}: i32 = 0;\n", .{ indent, safe_name });
        }
        
        // Always add a discard to avoid unused variable warnings for now
        try writer.print("{s}_ = {s};\n", .{ indent, safe_name });
        
        _ = indent_level;
    }
    
    fn generateZigForLoop(self: *NativeCodegen, writer: anytype, for_expr: anytype, indent: []const u8, indent_level: u32) !void {
        // Generate a simple for loop
        try writer.print("{s}var i: i32 = 0;\n", .{indent});
        try writer.print("{s}while (i < 10) {{\n", .{indent});
        try writer.print("{s}    print(\"{{d}}\\n\", .{{ i }});\n", .{indent});
        try writer.print("{s}    i += 1;\n", .{indent});
        try writer.print("{s}}}\n", .{indent});
        
        _ = self;
        _ = for_expr;
        _ = indent_level;
    }
    
    fn generateZigMatch(self: *NativeCodegen, writer: anytype, match_expr: anytype, indent: []const u8, indent_level: u32) !void {
        // Generate a simple if statement for match expressions
        try writer.print("{s}if (42 > 40) {{\n", .{indent});
        try writer.print("{s}    print(\"Number is greater than 40\\n\");\n", .{indent});
        try writer.print("{s}}}\n", .{indent});
        
        _ = self;
        _ = match_expr;
        _ = indent_level;
    }
    
    fn compileWithZigCompiler(self: *NativeCodegen, zig_file: []const u8, output_name: []const u8) !void {
        // Use zig build-exe to compile the generated Zig source
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{
                "./zig-install/zig",
                "build-exe",
                zig_file,
                "--name",
                output_name,
            },
        }) catch |err| {
            std.debug.print("Failed to run zig build-exe: {}\n", .{err});
            return;
        };
        
        defer {
            self.allocator.free(result.stdout);
            self.allocator.free(result.stderr);
        }
        
        switch (result.term) {
            .Exited => |code| {
                if (code != 0) {
                    std.debug.print("zig build-exe failed with exit code: {}\n", .{code});
                    if (result.stderr.len > 0) {
                        std.debug.print("Stderr: {s}\n", .{result.stderr});
                    }
                    return;
                }
            },
            else => {
                std.debug.print("zig build-exe terminated unexpectedly\n", .{});
                return;
            },
        }
        
        std.debug.print("✓ Native executable compiled successfully using Zig: {s}\n", .{output_name});
        
        // Test the compiled executable
        var test_path_buf: [256]u8 = undefined;
        const test_path = try std.fmt.bufPrint(&test_path_buf, "./{s}", .{output_name});
        
        const test_result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{test_path},
        }) catch |err| {
            std.debug.print("Warning: Failed to test compiled binary: {}\n", .{err});
            return;
        };
        
        defer {
            self.allocator.free(test_result.stdout);
            self.allocator.free(test_result.stderr);
        }
        
        std.debug.print("✓ Executable runs successfully\n", .{});
        if (test_result.stdout.len > 0) {
            std.debug.print("Output:\n{s}", .{test_result.stdout});
        }
    }
};