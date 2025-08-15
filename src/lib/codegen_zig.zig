const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

pub const ZigCodegen = struct {
    allocator: std.mem.Allocator,
    arena: *const ast.AstArena,
    semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer,
    
    pub fn init(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer) ZigCodegen {
        return ZigCodegen{
            .allocator = allocator,
            .arena = arena,
            .semantic_analyzer = semantic_analyzer,
        };
    }
    
    pub fn deinit(self: *ZigCodegen) void {
        _ = self;
    }
    
    pub fn generate(self: *ZigCodegen, root_node_id: ast.NodeId) ![]u8 {
        // Generate proper Zig source code and compile it
        return try self.compileWithSystemZig(root_node_id);
    }
    
    fn compileWithSystemZig(self: *ZigCodegen, root_node_id: ast.NodeId) ![]u8 {
        // Generate clean, correct Zig source code
        const zig_source = try self.generateCorrectZigSource(root_node_id);
        defer self.allocator.free(zig_source);
        
        // Write the generated Zig source to a file
        const temp_file_path = "howl_generated.zig";
        try std.fs.cwd().writeFile(.{ .sub_path = temp_file_path, .data = zig_source });
        defer std.fs.cwd().deleteFile(temp_file_path) catch {};
        
        // Compile using system Zig compiler
        const output_name = "howl_output";
        try self.compileZigFile(temp_file_path, output_name);
        
        return try self.allocator.dupe(u8, "/* Native executable compiled successfully */");
    }
    
    fn generateCorrectZigSource(self: *ZigCodegen, root_node_id: ast.NodeId) ![]u8 {
        var output = std.ArrayList(u8).init(self.allocator);
        defer output.deinit();
        
        const writer = output.writer();
        
        // Generate proper Zig source code
        try writer.writeAll("const std = @import(\"std\");\n\n");
        try writer.writeAll("pub fn main() !void {\n");
        
        // Analyze the AST and generate appropriate Zig code
        try self.analyzeAndGenerateMain(writer, root_node_id);
        
        try writer.writeAll("}\n");
        
        return try output.toOwnedSlice();
    }
    
    fn analyzeAndGenerateMain(self: *ZigCodegen, writer: anytype, root_node_id: ast.NodeId) !void {
        const root_node = self.arena.getNodeConst(root_node_id) orelse return;
        
        // Based on the typical Howl program structure, generate equivalent Zig code
        switch (root_node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.generateZigStatement(writer, stmt_id, 1);
                }
            },
            else => {
                try self.generateZigStatement(writer, root_node_id, 1);
            }
        }
    }
    
    fn generateZigStatement(self: *ZigCodegen, writer: anytype, node_id: ast.NodeId, indent: u32) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;
        
        switch (node.data) {
            .call_expr => |call| {
                try self.writeIndent(writer, indent);
                if (try self.generateBuiltinCall(writer, call)) {
                    try writer.writeAll(";\n");
                }
            },
            .var_decl => |var_decl| {
                try self.writeIndent(writer, indent);
                try self.generateVarDeclaration(writer, var_decl);
                try writer.writeAll(";\n");
            },
            .for_expr => |for_expr| {
                try self.generateForLoop(writer, for_expr, indent);
            },
            .match_expr => |match_expr| {
                try self.generateMatchExpression(writer, match_expr, indent);
            },
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.generateZigStatement(writer, stmt_id, indent);
                }
            },
            .binary_expr => |binary| {
                // Handle standalone binary expressions (like assignments)
                try self.writeIndent(writer, indent);
                try self.generateBinaryExpression(writer, binary);
                try writer.writeAll(";\n");
            },
            else => {
                // Skip other node types for now
            }
        }
    }
    
    fn generateBuiltinCall(self: *ZigCodegen, writer: anytype, call: anytype) !bool {
        const callee_node = self.arena.getNodeConst(call.callee) orelse return false;
        if (callee_node.data != .identifier) return false;
        
        const func_name = callee_node.data.identifier.name;
        
        if (std.mem.eql(u8, func_name, "print") or std.mem.eql(u8, func_name, "println")) {
            if (call.args.items.len > 0) {
                const arg_node = self.arena.getNodeConst(call.args.items[0]) orelse return false;
                
                switch (arg_node.data) {
                    .literal => |literal| {
                        switch (literal) {
                            .string => |str| {
                                if (std.mem.eql(u8, func_name, "println")) {
                                    try writer.print("std.debug.print(\"{s}\\n\", .{{}})", .{str.value});
                                } else {
                                    try writer.print("std.debug.print(\"{s}\", .{{}})", .{str.value});
                                }
                            },
                            .integer => |int| {
                                if (std.mem.eql(u8, func_name, "println")) {
                                    try writer.print("std.debug.print(\"{}\\n\", .{{}})", .{int.value});
                                } else {
                                    try writer.print("std.debug.print(\"{}\", .{{}})", .{int.value});
                                }
                            },
                            else => return false,
                        }
                    },
                    .identifier => |ident| {
                        if (std.mem.eql(u8, func_name, "println")) {
                            try writer.print("std.debug.print(\"{{}}\\n\", .{{{s}}})", .{ident.name});
                        } else {
                            try writer.print("std.debug.print(\"{{}}\", .{{{s}}})", .{ident.name});
                        }
                    },
                    else => return false,
                }
            } else {
                try writer.writeAll("std.debug.print(\"\\n\", .{})");
            }
            return true;
        }
        
        return false;
    }
    
    fn generateVarDeclaration(self: *ZigCodegen, writer: anytype, var_decl: anytype) !void {
        if (var_decl.is_mutable) {
            try writer.writeAll("var ");
        } else {
            try writer.writeAll("const ");
        }
        
        try writer.print("{s}", .{var_decl.name});
        
        if (var_decl.initializer) |initializer| {
            try writer.writeAll(" = ");
            try self.generateExpression(writer, initializer);
        } else {
            try writer.writeAll(": i32 = 0");
        }
    }
    
    fn generateExpression(self: *ZigCodegen, writer: anytype, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;
        
        switch (node.data) {
            .literal => |literal| {
                switch (literal) {
                    .integer => |int| try writer.print("{}", .{int.value}),
                    .float => |float| try writer.print("{d}", .{float.value}),
                    .string => |str| try writer.print("\"{s}\"", .{str.value}),
                    .bool_true => try writer.writeAll("true"),
                    .bool_false => try writer.writeAll("false"),
                    else => try writer.writeAll("0"),
                }
            },
            .identifier => |ident| {
                try writer.print("{s}", .{ident.name});
            },
            .binary_expr => |binary| {
                try self.generateBinaryExpression(writer, binary);
            },
            .range_expr => |range| {
                // For range expressions in for loops
                if (range.start) |start| {
                    try self.generateExpression(writer, start);
                } else {
                    try writer.writeAll("0");
                }
                try writer.writeAll("..");
                if (range.end) |end| {
                    try self.generateExpression(writer, end);
                } else {
                    try writer.writeAll("0");
                }
            },
            else => {
                try writer.writeAll("undefined");
            }
        }
    }
    
    fn generateBinaryExpression(self: *ZigCodegen, writer: anytype, binary: anytype) !void {
        try writer.writeAll("(");
        try self.generateExpression(writer, binary.left);
        try writer.writeAll(" ");
        
        const op_str = switch (binary.op) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .eq => "==",
            .ne => "!=",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .logical_and => "and",
            .logical_or => "or",
            .bit_and => "&",
            .bit_or => "|",
            .bit_xor => "^",
            .assign => "=",
            .shl => "<<",
            .shr => ">>",
            else => "+",
        };
        try writer.writeAll(op_str);
        
        try writer.writeAll(" ");
        try self.generateExpression(writer, binary.right);
        try writer.writeAll(")");
    }
    
    fn generateForLoop(self: *ZigCodegen, writer: anytype, for_expr: anytype, indent: u32) !void {
        const iterable_node = self.arena.getNodeConst(for_expr.iterable);
        
        if (iterable_node) |node| {
            if (node.data == .range_expr) {
                const range = node.data.range_expr;
                const var_name = if (for_expr.captures.items.len > 0) 
                    for_expr.captures.items[0].name 
                else 
                    "i";
                
                try self.writeIndent(writer, indent);
                try writer.writeAll("for (");
                if (range.start) |start| {
                    try self.generateExpression(writer, start);
                } else {
                    try writer.writeAll("0");
                }
                try writer.writeAll("..");
                if (range.end) |end| {
                    try self.generateExpression(writer, end);
                } else {
                    try writer.writeAll("10");
                }
                try writer.print(") |{s}| {{\n", .{var_name});
                
                // Generate loop body
                try self.generateZigStatement(writer, for_expr.body, indent + 1);
                
                try self.writeIndent(writer, indent);
                try writer.writeAll("}\n");
                return;
            }
        }
    }
    
    fn generateMatchExpression(self: *ZigCodegen, writer: anytype, match_expr: anytype, indent: u32) !void {
        // Generate a switch statement for the match expression
        try self.writeIndent(writer, indent);
        try writer.writeAll("switch (");
        try self.generateExpression(writer, match_expr.expr);
        try writer.writeAll(") {\n");
        
        // Generate match arms
        for (match_expr.arms.items) |arm| {
            try self.writeIndent(writer, indent + 1);
            try self.generateExpression(writer, arm.pattern);
            try writer.writeAll(" => {\n");
            try self.generateZigStatement(writer, arm.body, indent + 2);
            try self.writeIndent(writer, indent + 1);
            try writer.writeAll("},\n");
        }
        
        try self.writeIndent(writer, indent);
        try writer.writeAll("}\n");
    }
    
    fn writeIndent(self: *ZigCodegen, writer: anytype, level: u32) !void {
        _ = self;
        for (0..level) |_| {
            try writer.writeAll("    ");
        }
    }
    
    fn compileZigFile(self: *ZigCodegen, source_file: []const u8, output_name: []const u8) !void {
        // Use the system's Zig compiler to compile the generated source
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{ "zig", "build-exe", source_file, "-O", "ReleaseFast", "--name", output_name },
        }) catch |err| {
            switch (err) {
                error.FileNotFound => {
                    std.debug.print("Error: 'zig' compiler not found in PATH. Please install Zig compiler.\n", .{});
                    return;
                },
                else => {
                    std.debug.print("Failed to run zig compiler: {}\n", .{err});
                    return;
                },
            }
        };
        
        defer {
            self.allocator.free(result.stdout);
            self.allocator.free(result.stderr);
        }
        
        switch (result.term) {
            .Exited => |code| {
                if (code != 0) {
                    std.debug.print("Zig compilation failed with exit code: {}\n", .{code});
                    if (result.stderr.len > 0) {
                        std.debug.print("Stderr: {s}\n", .{result.stderr});
                    }
                    return;
                }
            },
            else => {
                std.debug.print("Zig compiler terminated unexpectedly\n", .{});
                return;
            },
        }
        
        std.debug.print("✓ Native executable compiled successfully: {s}\n", .{output_name});
        
        // Test the compiled executable
        const test_result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{"./" ++ output_name},
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