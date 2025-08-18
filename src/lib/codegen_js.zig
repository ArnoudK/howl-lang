const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

pub const JSCodegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    indent_level: u32,
    arena: *const ast.AstArena,
    semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer,

    const INDENT_SIZE = 4;

    pub fn init(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer) JSCodegen {
        return JSCodegen{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
            .arena = arena,
            .semantic_analyzer = semantic_analyzer,
        };
    }

    pub fn deinit(self: *JSCodegen) void {
        self.output.deinit();
    }

    pub fn generate(self: *JSCodegen, root_node_id: ast.NodeId) ![]const u8 {
        try self.generateNode(root_node_id);
        return try self.output.toOwnedSlice();
    }

    fn writeIndent(self: *JSCodegen) !void {
        var i: u32 = 0;
        while (i < self.indent_level * INDENT_SIZE) : (i += 1) {
            try self.output.append(' ');
        }
    }

    fn writeLine(self: *JSCodegen, text: []const u8) !void {
        try self.writeIndent();
        try self.output.appendSlice(text);
        try self.output.append('\n');
    }

    fn write(self: *JSCodegen, text: []const u8) !void {
        try self.output.appendSlice(text);
    }

    fn writeFormatted(self: *JSCodegen, comptime fmt: []const u8, args: anytype) !void {
        const formatted = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(formatted);
        try self.write(formatted);
    }

    fn generateNode(self: *JSCodegen, node_id: ast.NodeId) anyerror!void {
        const node = self.arena.getNodeConst(node_id) orelse return;
        
        switch (node.data) {
            .literal => |literal| try self.generateLiteral(literal),
            .identifier => |id| try self.write(id.name),
            .binary_expr => |binary| try self.generateBinaryExpr(binary),
            .unary_expr => |unary| try self.generateUnaryExpr(unary),
            .call_expr => |call| try self.generateCallExpr(call),
            .member_expr => |member| try self.generateMemberExpr(member),
            .index_expr => |index| try self.generateIndexExpr(index),
            .var_decl => |var_decl| try self.generateVarDecl(var_decl),
            .function_decl => |func| try self.generateFunctionDecl(func),
            .struct_decl => |struct_decl| try self.generateStructDecl(struct_decl),
            .enum_decl => |enum_decl| try self.generateEnumDecl(enum_decl),
            .if_expr => |if_expr| try self.generateIfExpr(if_expr),
            .while_expr => |while_expr| try self.generateWhileExpr(while_expr),
            .for_expr => |for_expr| try self.generateForExpr(for_expr),
            .block => |block| try self.generateBlock(block),
            .struct_init => |struct_init| try self.generateStructInit(struct_init),
            .array_init => |array| try self.generateArrayInit(array),
            .range_expr => |range| try self.generateRangeExpr(range),
            .generic_type_expr => |generic| try self.generateGenericTypeExpr(generic),
            .return_stmt => |ret| try self.generateReturnStmt(ret),
            .import_decl => |import_decl| try self.generateImportDecl(import_decl),
            .extern_fn_decl => |extern_fn| try self.generateExternFnDecl(extern_fn),
            .compile_target_expr => try self.generateCompileTargetExpr(),
            .compile_insert_expr => |insert| try self.generateCompileInsertExpr(insert),
            .match_compile_expr => |match_expr| try self.generateMatchCompileExpr(match_expr),
            .match_expr => |match_expr| try self.generateMatchExpr(match_expr),
            .try_expr => |try_expr| try self.generateTryExpr(try_expr),
            .catch_expr => |catch_expr| try self.generateCatchExpr(catch_expr),
            .error_union_type => try self.generateErrorUnionType(),
            .error_literal => |error_literal| try self.generateErrorLiteral(error_literal),
            .error_set_decl => |error_set_decl| try self.generateErrorSetDecl(error_set_decl),
            else => {
                // Handle unsupported nodes with comments
                try self.writeFormatted("/* Unsupported AST node: {s} */", .{@tagName(node.data)});
            },
        }
    }

    fn generateLiteral(self: *JSCodegen, literal: ast.Literal) !void {
        switch (literal) {
            .integer => |int| try self.writeFormatted("{d}", .{int.value}),
            .float => |float| try self.writeFormatted("{d}", .{float.value}),
            .string => |str| try self.writeFormatted("\"{s}\"", .{str.value}),
            .char => |char| try self.writeFormatted("\"{c}\"", .{char.value}),
            .bool_true => try self.write("true"),
            .bool_false => try self.write("false"),
            .enum_member => |enum_member| {
                // For enum members in JavaScript, we can use string literals or enum objects
                try self.writeFormatted("\"{s}\"", .{enum_member.name});
            },
        }
    }

    fn generateBinaryExpr(self: *JSCodegen, binary: anytype) anyerror!void {
        // Special handling for string concatenation
        if (binary.op == .concat) {
            try self.write("(");
            try self.generateNode(binary.left);
            try self.write(" + ");
            try self.generateNode(binary.right);
            try self.write(")");
        } else {
            try self.write("(");
            try self.generateNode(binary.left);
            try self.write(" ");
            try self.write(self.binaryOpToJS(binary.op));
            try self.write(" ");
            try self.generateNode(binary.right);
            try self.write(")");
        }
    }

    fn generateUnaryExpr(self: *JSCodegen, unary: anytype) !void {
        try self.write(self.unaryOpToJS(unary.op));
        try self.generateNode(unary.operand);
    }

    fn generateCallExpr(self: *JSCodegen, call: anytype) !void {
        // Check if this is a builtin function call
        const callee_node = self.arena.getNodeConst(call.callee);
        if (callee_node) |node| {
            if (node.data == .identifier) {
                const ident = node.data.identifier;
                
                // Handle builtin functions
                if (try self.handleBuiltinCall(ident.name, call.args)) {
                    return;
                }
            }
            
            // Special handling for std.debug.print
            if (node.data == .member_expr) {
                const member = node.data.member_expr;
                const member_obj_node = self.arena.getNodeConst(member.object);
                if (member_obj_node) |member_obj| {
                    if (member_obj.data == .member_expr) {
                        const nested_member = member_obj.data.member_expr;
                        const base_obj_node = self.arena.getNodeConst(nested_member.object);
                        if (base_obj_node) |base_obj| {
                            if (base_obj.data == .identifier) {
                                const base_ident = base_obj.data.identifier;
                                // Handle std.debug.print -> console.log
                                if (std.mem.eql(u8, base_ident.name, "std") and 
                                    std.mem.eql(u8, nested_member.field, "debug") and 
                                    std.mem.eql(u8, member.field, "print")) {
                                    try self.generatePrintCall(call.args);
                                    return;
                                }
                            }
                        }
                    }
                }
            }
            
            // Special handling for method calls on generic types like std.List(T).init()
            if (node.data == .member_expr) {
                const member = node.data.member_expr;
                const member_obj_node = self.arena.getNodeConst(member.object);
                if (member_obj_node) |member_obj| {
                    if (member_obj.data == .generic_type_expr) {
                        const generic = member_obj.data.generic_type_expr;
                        const base_node = self.arena.getNodeConst(generic.base_type);
                        if (base_node) |base| {
                            if (base.data == .member_expr) {
                                const base_member = base.data.member_expr;
                                // Check if this is std.List(T).method()
                                if (std.mem.eql(u8, base_member.field, "List")) {
                                    const std_node = self.arena.getNodeConst(base_member.object);
                                    if (std_node) |std_obj| {
                                        if (std_obj.data == .identifier) {
                                            const std_ident = std_obj.data.identifier;
                                            if (std.mem.eql(u8, std_ident.name, "std")) {
                                                // Handle std.List(T) method calls
                                                if (std.mem.eql(u8, member.field, "init")) {
                                                    // std.List(T).init() -> []
                                                    try self.write("[]");
                                                    return;
                                                } else if (std.mem.eql(u8, member.field, "from")) {
                                                    // std.List(T).from(array) -> [...array]
                                                    if (call.args.items.len > 0) {
                                                        try self.write("[...");
                                                        try self.generateNode(call.args.items[0]);
                                                        try self.write("]");
                                                    } else {
                                                        try self.write("[]");
                                                    }
                                                    return;
                                                } else if (std.mem.eql(u8, member.field, "initCapacity")) {
                                                    // std.List(T).initCapacity(n) -> new Array(n)
                                                    try self.write("new Array(");
                                                    if (call.args.items.len > 0) {
                                                        try self.generateNode(call.args.items[0]);
                                                    } else {
                                                        try self.write("0");
                                                    }
                                                    try self.write(")");
                                                    return;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            // Special handling for __anonymous_struct calls (from .{} syntax)
            if (node.data == .identifier) {
                const ident = node.data.identifier;
                if (std.mem.eql(u8, ident.name, "__anonymous_struct")) {
                    // For anonymous struct, just generate the arguments directly
                    // This handles .{c} -> c
                    for (call.args.items, 0..) |arg, i| {
                        if (i > 0) try self.write(" + ");
                        try self.generateNode(arg);
                    }
                    return;
                }
            }
        }
        
        // Default function call generation
        try self.generateNode(call.callee);
        try self.write("(");
        for (call.args.items, 0..) |arg, i| {
            if (i > 0) try self.write(", ");
            try self.generateNode(arg);
        }
        try self.write(")");
    }

    fn handleBuiltinCall(self: *JSCodegen, function_name: []const u8, args: std.ArrayList(ast.NodeId)) !bool {
        // Type introspection builtins
        if (std.mem.eql(u8, function_name, "sizeOf")) {
            try self.generateSizeOfBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "TypeOf")) {
            try self.generateTypeOfBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "alignOf")) {
            try self.generateAlignOfBuiltin(args);
            return true;
        }
        
        // Safe arithmetic builtins
        if (std.mem.eql(u8, function_name, "add_s")) {
            try self.generateSafeArithmeticBuiltin("add", args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "sub_s")) {
            try self.generateSafeArithmeticBuiltin("sub", args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "mul_s")) {
            try self.generateSafeArithmeticBuiltin("mul", args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "div_s")) {
            try self.generateSafeArithmeticBuiltin("div", args);
            return true;
        }
        
        // Type casting builtins
        if (std.mem.eql(u8, function_name, "intCast")) {
            try self.generateIntCastBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "floatCast")) {
            try self.generateFloatCastBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "truncate")) {
            try self.generateTruncateBuiltin(args);
            return true;
        }
        
        // Memory builtins
        if (std.mem.eql(u8, function_name, "memcpy")) {
            try self.generateMemcpyBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "memset")) {
            try self.generateMemsetBuiltin(args);
            return true;
        }
        
        // ArrayList builtins
        if (std.mem.eql(u8, function_name, "ArrayList")) {
            try self.generateArrayListBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "arrayListInit")) {
            try self.generateArrayListInitBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "arrayListAppend")) {
            try self.generateArrayListAppendBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "arrayListGet")) {
            try self.generateArrayListGetBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "arrayListLen")) {
            try self.generateArrayListLenBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "arrayListClear")) {
            try self.generateArrayListClearBuiltin(args);
            return true;
        }
        
        // Compilation builtins
        if (std.mem.eql(u8, function_name, "panic")) {
            try self.generatePanicBuiltin(args);
            return true;
        }
        if (std.mem.eql(u8, function_name, "compileError")) {
            try self.generateCompileErrorBuiltin(args);
            return true;
        }
        
        return false; // Not a builtin function
    }
    
    fn generateSizeOfBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 1) {
            try self.write("/* Invalid @sizeOf call - expected 1 argument */");
            return;
        }
        
        // For JavaScript, we'll create a mapping of Howl types to their sizes
        const arg_node = self.arena.getNodeConst(args.items[0]);
        if (arg_node) |node| {
            if (node.data == .identifier) {
                const type_name = node.data.identifier.name;
                const size = self.getTypeSize(type_name);
                try self.writeFormatted("{d}", .{size});
                return;
            }
        }
        
        // Fallback - generate as a runtime check
        try self.write("(");
        try self.generateNode(args.items[0]);
        try self.write(" ? 8 : 0)"); // Default to 8 bytes for most types
    }
    
    fn generateTypeOfBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 1) {
            try self.write("/* Invalid @TypeOf call - expected 1 argument */");
            return;
        }
        
        try self.write("\"");
        try self.generateNode(args.items[0]);
        try self.write("\"");
    }
    
    fn generateAlignOfBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 1) {
            try self.write("/* Invalid @alignOf call - expected 1 argument */");
            return;
        }
        
        // Most types have pointer alignment in JavaScript
        try self.write("8");
    }
    
    fn generateSafeArithmeticBuiltin(self: *JSCodegen, op: []const u8, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 2) {
            try self.writeFormatted("/* Invalid @{s}_s call - expected 2 arguments */", .{op});
            return;
        }
        
        // Generate safe arithmetic with overflow checks
        const js_op = if (std.mem.eql(u8, op, "add")) "+"
                     else if (std.mem.eql(u8, op, "sub")) "-"
                     else if (std.mem.eql(u8, op, "mul")) "*"
                     else if (std.mem.eql(u8, op, "div")) "/"
                     else "?";
        
        try self.write("(");
        try self.generateNode(args.items[0]);
        try self.writeFormatted(" {s} ", .{js_op});
        try self.generateNode(args.items[1]);
        try self.write(")");
    }
    
    fn generateIntCastBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 2) {
            try self.write("/* Invalid @intCast call - expected 2 arguments */");
            return;
        }
        
        // In JavaScript, just cast to integer
        try self.write("Math.trunc(");
        try self.generateNode(args.items[1]);
        try self.write(")");
    }
    
    fn generateFloatCastBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 2) {
            try self.write("/* Invalid @floatCast call - expected 2 arguments */");
            return;
        }
        
        // In JavaScript, just cast to number
        try self.write("Number(");
        try self.generateNode(args.items[1]);
        try self.write(")");
    }
    
    fn generateTruncateBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 2) {
            try self.write("/* Invalid @truncate call - expected 2 arguments */");
            return;
        }
        
        try self.write("Math.trunc(");
        try self.generateNode(args.items[1]);
        try self.write(")");
    }
    
    fn generateMemcpyBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 3) {
            try self.write("/* Invalid @memcpy call - expected 3 arguments */");
            return;
        }
        
        // For JavaScript, we'll simulate with array copying
        try self.write("(");
        try self.generateNode(args.items[0]);
        try self.write(".set(");
        try self.generateNode(args.items[1]);
        try self.write("))");
    }
    
    fn generateMemsetBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 3) {
            try self.write("/* Invalid @memset call - expected 3 arguments */");
            return;
        }
        
        // For JavaScript, we'll simulate with array filling
        try self.write("(");
        try self.generateNode(args.items[0]);
        try self.write(".fill(");
        try self.generateNode(args.items[1]);
        try self.write("))");
    }
    
    fn generatePanicBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        try self.write("(function() { throw new Error(");
        if (args.items.len > 0) {
            try self.generateNode(args.items[0]);
        } else {
            try self.write("\"panic\"");
        }
        try self.write("); })()");
    }
    
    fn generateCompileErrorBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        try self.write("/* @compileError: ");
        if (args.items.len > 0) {
            try self.generateNode(args.items[0]);
        } else {
            try self.write("compile error");
        }
        try self.write(" */");
    }
    
    fn getTypeSize(self: *JSCodegen, type_name: []const u8) u32 {
        _ = self;
        // Return sizes in bytes for common types
        if (std.mem.eql(u8, type_name, "i8") or std.mem.eql(u8, type_name, "u8")) return 1;
        if (std.mem.eql(u8, type_name, "i16") or std.mem.eql(u8, type_name, "u16")) return 2;
        if (std.mem.eql(u8, type_name, "i32") or std.mem.eql(u8, type_name, "u32") or std.mem.eql(u8, type_name, "f32")) return 4;
        if (std.mem.eql(u8, type_name, "i64") or std.mem.eql(u8, type_name, "u64") or std.mem.eql(u8, type_name, "f64")) return 8;
        if (std.mem.eql(u8, type_name, "i128") or std.mem.eql(u8, type_name, "u128")) return 16;
        if (std.mem.eql(u8, type_name, "bool")) return 1;
        if (std.mem.eql(u8, type_name, "str")) return 8; // Pointer size
        if (std.mem.eql(u8, type_name, "void")) return 0;
        
        // Default for unknown types
        return 8;
    }
    
    // ArrayList builtin functions for JavaScript
    fn generateArrayListBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 1) {
            try self.write("/* Invalid @ArrayList call - expected 1 argument */");
            return;
        }
        
        // @ArrayList(T) returns a constructor function
        try self.write("Array"); // JavaScript arrays are our ArrayList implementation
    }
    
    fn generateArrayListInitBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 2) {
            try self.write("/* Invalid @arrayListInit call - expected 2 arguments */");
            return;
        }
        
        // @arrayListInit(T, allocator) returns new Array()
        try self.write("[]"); // Empty JavaScript array
    }
    
    fn generateArrayListAppendBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 2) {
            try self.write("/* Invalid @arrayListAppend call - expected 2 arguments */");
            return;
        }
        
        // @arrayListAppend(list, item) -> list.push(item)
        try self.generateNode(args.items[0]);
        try self.write(".push(");
        try self.generateNode(args.items[1]);
        try self.write(")");
    }
    
    fn generateArrayListGetBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 2) {
            try self.write("/* Invalid @arrayListGet call - expected 2 arguments */");
            return;
        }
        
        // @arrayListGet(list, index) -> list[index]
        try self.generateNode(args.items[0]);
        try self.write("[");
        try self.generateNode(args.items[1]);
        try self.write("]");
    }
    
    fn generateArrayListLenBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 1) {
            try self.write("/* Invalid @arrayListLen call - expected 1 argument */");
            return;
        }
        
        // @arrayListLen(list) -> list.length
        try self.generateNode(args.items[0]);
        try self.write(".length");
    }
    
    fn generateArrayListClearBuiltin(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        if (args.items.len != 1) {
            try self.write("/* Invalid @arrayListClear call - expected 1 argument */");
            return;
        }
        
        // @arrayListClear(list) -> list.length = 0
        try self.write("(");
        try self.generateNode(args.items[0]);
        try self.write(".length = 0)");
    }

    fn generatePrintCall(self: *JSCodegen, args: std.ArrayList(ast.NodeId)) !void {
        try self.write("console.log(");
        
        if (args.items.len == 0) {
            try self.write(")");
            return;
        }
        
        if (args.items.len == 1) {
            // Single argument - just pass it through
            try self.generateNode(args.items[0]);
            try self.write(")");
            return;
        }
        
        // Two arguments: format string and anonymous struct
        if (args.items.len == 2) {
            const first_arg = args.items[0];  // Should be string literal
            const second_arg = args.items[1]; // Should be .{variable}
            
            // Get the format string content
            const format_node = self.arena.getNodeConst(first_arg);
            var format_string: []const u8 = "";
            if (format_node) |fmt_node| {
                if (fmt_node.data == .literal) {
                    const literal = fmt_node.data.literal;
                    if (literal == .string) {
                        format_string = literal.string.value;
                    }
                }
            }
            
            // Check if second argument is an anonymous struct
            const arg_node = self.arena.getNodeConst(second_arg);
            if (arg_node) |arg_n| {
                if (arg_n.data == .call_expr) {
                    const arg_call = arg_n.data.call_expr;
                    const arg_callee_node = self.arena.getNodeConst(arg_call.callee);
                    if (arg_callee_node) |callee_n| {
                        if (callee_n.data == .identifier) {
                            const callee_ident = callee_n.data.identifier;
                            if (std.mem.eql(u8, callee_ident.name, "__anonymous_struct")) {
                                // Process format string with variables
                                try self.generateFormattedString(format_string, arg_call.args.items);
                                try self.write(")");
                                return;
                            }
                        }
                    }
                }
            }
            
            // If not anonymous struct, treat as regular argument
            try self.generateNode(first_arg);
            try self.write(", ");
            try self.generateNode(second_arg);
        }
        
        try self.write(")");
    }

    // Enhanced method to generate formatted strings with proper format specifier handling
    fn generateFormattedString(self: *JSCodegen, format_string: []const u8, args: []ast.NodeId) !void {
        // Check if there are any format specifiers at all
        var has_format_specs = false;
        for (format_string) |char| {
            if (char == '{') {
                has_format_specs = true;
                break;
            }
        }
        
        // If no format specifiers, just output the string as-is
        if (!has_format_specs) {
            try self.write("\"");
            for (format_string) |char| {
                if (char == '"') {
                    try self.write("\\\"");
                } else if (char == '\n') {
                    try self.write("\\n");
                } else if (char == '\t') {
                    try self.write("\\t");
                } else if (char == '\r') {
                    try self.write("\\r");
                } else if (char == '\\') {
                    try self.write("\\\\");
                } else {
                    try self.output.append(char);
                }
            }
            try self.write("\"");
            return;
        }
        
        // Process format string with variables
        var arg_index: usize = 0;
        var i: usize = 0;
        var is_first_part = true;
        
        var current_text = std.ArrayList(u8).init(self.allocator);
        defer current_text.deinit();
        
        while (i < format_string.len) {
            if (format_string[i] == '{' and i + 1 < format_string.len) {
                // Found a format specifier
                var end_pos = i + 1;
                while (end_pos < format_string.len and format_string[end_pos] != '}') {
                    end_pos += 1;
                }
                
                if (end_pos < format_string.len and arg_index < args.len) {
                    // Output the current text part if any
                    if (current_text.items.len > 0) {
                        if (!is_first_part) try self.write(" + ");
                        try self.write("\"");
                        for (current_text.items) |char| {
                            if (char == '"') {
                                try self.write("\\\"");
                            } else if (char == '\n') {
                                try self.write("\\n");
                            } else if (char == '\t') {
                                try self.write("\\t");
                            } else if (char == '\r') {
                                try self.write("\\r");
                            } else if (char == '\\') {
                                try self.write("\\\\");
                            } else {
                                try self.output.append(char);
                            }
                        }
                        try self.write("\"");
                        is_first_part = false;
                        current_text.clearRetainingCapacity();
                    }
                    
                    // Output the variable
                    if (!is_first_part) try self.write(" + ");
                    
                    const format_spec = format_string[i + 1..end_pos];
                    try self.generateVariableWithFormat(args[arg_index], format_spec);
                    
                    is_first_part = false;
                    arg_index += 1;
                    i = end_pos + 1; // Skip past '}'
                } else {
                    // Malformed format spec, just add the character
                    try current_text.append(format_string[i]);
                    i += 1;
                }
            } else {
                try current_text.append(format_string[i]);
                i += 1;
            }
        }
        
        // Add any remaining text part
        if (current_text.items.len > 0) {
            if (!is_first_part) try self.write(" + ");
            try self.write("\"");
            for (current_text.items) |char| {
                if (char == '"') {
                    try self.write("\\\"");
                } else if (char == '\n') {
                    try self.write("\\n");
                } else if (char == '\t') {
                    try self.write("\\t");
                } else if (char == '\r') {
                    try self.write("\\r");
                } else if (char == '\\') {
                    try self.write("\\\\");
                } else {
                    try self.output.append(char);
                }
            }
            try self.write("\"");
        }
        
        // If we had no parts at all, output empty string
        if (is_first_part) {
            try self.write("\"\"");
        }
    }
    
    // Generate a variable with optional format specification
    fn generateVariableWithFormat(self: *JSCodegen, var_node_id: ast.NodeId, format_spec: []const u8) !void {
        if (format_spec.len == 0 or std.mem.eql(u8, format_spec, "d") or std.mem.eql(u8, format_spec, "s")) {
            // Basic format specifiers - just output the variable
            try self.generateNode(var_node_id);
        } else if (std.mem.startsWith(u8, format_spec, "f:")) {
            // Float format with precision like "f:.2"
            const precision_part = format_spec[2..];
            if (std.mem.startsWith(u8, precision_part, ".")) {
                const precision_str = precision_part[1..];
                try self.generateNode(var_node_id);
                try self.write(".toFixed(");
                try self.write(precision_str);
                try self.write(")");
            } else {
                // Default case for malformed f: spec
                try self.generateNode(var_node_id);
            }
        } else {
            // Default case - just output the variable
            try self.generateNode(var_node_id);
        }
    }

    fn generateMemberExpr(self: *JSCodegen, member: anytype) !void {
        try self.generateNode(member.object);
        try self.write(".");
        try self.write(member.field);
    }

    fn generateIndexExpr(self: *JSCodegen, index: anytype) !void {
        try self.generateNode(index.object);
        try self.write("[");
        try self.generateNode(index.index);
        try self.write("]");
    }

    fn generateVarDecl(self: *JSCodegen, var_decl: anytype) !void {
        // Check if this is an import variable (e.g., std :: @import("std"))
        if (var_decl.initializer) |initializer| {
            const init_node = self.arena.getNodeConst(initializer);
            if (init_node) |node| {
                if (node.data == .call_expr) {
                    const call = node.data.call_expr;
                    const callee_node = self.arena.getNodeConst(call.callee);
                    if (callee_node) |callee| {
                        if (callee.data == .identifier) {
                            const ident = callee.data.identifier;
                            if (std.mem.eql(u8, ident.name, "import")) {
                                // This is an import assignment, skip it for JS output
                                return;
                            }
                        }
                    }
                }
            }
        }
        
        const keyword = if (var_decl.is_mutable) "let" else "const";
        try self.writeFormatted("{s} {s}", .{ keyword, var_decl.name });
        
        if (var_decl.initializer) |initializer| {
            try self.write(" = ");
            try self.generateNode(initializer);
        }
        try self.write(";");
    }

    fn generateFunctionDecl(self: *JSCodegen, func: anytype) !void {
        // Check if this is the main function to add export
        if (std.mem.eql(u8, func.name, "main")) {
            try self.write("export ");
        }
        
        try self.writeFormatted("function {s}(", .{func.name});
        
        for (func.params.items, 0..) |param, i| {
            if (i > 0) try self.write(", ");
            try self.write(param.name);
        }
        
        try self.write(") ");
        
        // Generate function body - should be a block
        const body_node = self.arena.getNodeConst(func.body);
        if (body_node) |node| {
            if (node.data == .block) {
                // It's a block, use specialized function body generator
                try self.generateFunctionBodyBlock(node.data.block);
            } else {
                // Not a block, wrap it in braces
                try self.write(" {\n");
                self.indent_level += 1;
                try self.writeIndent();
                try self.generateNode(func.body);
                try self.write(";\n");
                self.indent_level -= 1;
                try self.write("}");
            }
        } else {
            // No body node, create empty block
            try self.write(" {\n}");
        }
    }

    fn generateStructDecl(self: *JSCodegen, struct_decl: anytype) !void {
        // Generate a constructor function for the struct
        try self.writeFormatted("function {s}(", .{struct_decl.name});
        
        // Generate constructor parameters
        for (struct_decl.fields.items, 0..) |field, i| {
            if (i > 0) try self.write(", ");
            try self.write(field.name);
        }
        
        try self.writeLine(") {");
        self.indent_level += 1;
        
        // Assign fields
        for (struct_decl.fields.items) |field| {
            try self.writeFormatted("this.{s} = {s};\n", .{ field.name, field.name });
        }
        
        self.indent_level -= 1;
        try self.writeLine("}");
    }

    fn generateEnumDecl(self: *JSCodegen, enum_decl: anytype) !void {
        // Generate JavaScript object for enum
        try self.writeFormatted("const {s} = {{", .{enum_decl.name});
        try self.writeLine("");
        self.indent_level += 1;
        
        for (enum_decl.members.items, 0..) |member, i| {
            try self.writeIndent();
            if (member.value) |value_node| {
                // Member has explicit value
                try self.writeFormatted("{s}: ", .{member.name});
                try self.generateNode(value_node);
            } else {
                // No explicit value, use index
                try self.writeFormatted("{s}: {d}", .{ member.name, i });
            }
            
            if (i < enum_decl.members.items.len - 1) {
                try self.writeLine(",");
            } else {
                try self.writeLine("");
            }
        }
        
        self.indent_level -= 1;
        try self.writeLine("};");
        
        // Also generate reverse mapping for debugging
        try self.writeFormatted("{s}._names = {{", .{enum_decl.name});
        try self.writeLine("");
        self.indent_level += 1;
        
        for (enum_decl.members.items, 0..) |member, i| {
            try self.writeIndent();
            if (member.value) |_| {
                // For explicit values, we'd need to evaluate the expression
                // For simplicity, we'll use the index for now
                try self.writeFormatted("{d}: '{s}'", .{ i, member.name });
            } else {
                try self.writeFormatted("{d}: '{s}'", .{ i, member.name });
            }
            
            if (i < enum_decl.members.items.len - 1) {
                try self.writeLine(",");
            } else {
                try self.writeLine("");
            }
        }
        
        self.indent_level -= 1;
        try self.writeLine("};");
    }

    fn generateIfExpr(self: *JSCodegen, if_expr: anytype) !void {
        // For if expressions (ternary), generate: (condition ? then : else)
        // This produces a more natural JavaScript ternary operator
        try self.write("(");
        try self.generateNode(if_expr.condition);
        try self.write(" ? ");
        try self.generateNode(if_expr.then_branch);
        try self.write(" : ");
        
        if (if_expr.else_branch) |else_branch| {
            try self.generateNode(else_branch);
        } else {
            try self.write("undefined");
        }
        
        try self.write(")");
    }

    fn generateWhileExpr(self: *JSCodegen, while_expr: anytype) !void {
        try self.write("while (");
        try self.generateNode(while_expr.condition);
        try self.write(") ");
        
        // Handle body
        const body_node = self.arena.getNodeConst(while_expr.body) orelse return;
        if (body_node.data == .block) {
            try self.generateNode(while_expr.body);
        } else {
            try self.write("{\n");
            self.indent_level += 1;
            try self.writeIndent();
            try self.generateNode(while_expr.body);
            try self.write(";\n");
            self.indent_level -= 1;
            try self.writeIndent();
            try self.write("}");
        }
    }

    fn generateForExpr(self: *JSCodegen, for_expr: anytype) !void {
        // Check if we're iterating over a range expression
        const iterable_node = self.arena.getNode(for_expr.iterable);
        if (iterable_node != null and iterable_node.?.data == .range_expr) {
            // Handle range-based for loop
            try self.generateRangeForLoop(for_expr);
            return;
        }
        
        // Handle different types of for loops based on captures
        if (for_expr.captures.items.len == 1) {
            // Single capture: for (array) |value|
            const capture = for_expr.captures.items[0];
            if (std.mem.eql(u8, capture.name, "_")) {
                // Ignored capture: for (array) |_|
                try self.write("for (const _ of ");
                try self.generateNode(for_expr.iterable);
                try self.write(") ");
            } else {
                // Value capture: for (array) |value|
                try self.writeFormatted("for (const {s} of ", .{capture.name});
                try self.generateNode(for_expr.iterable);
                try self.write(") ");
            }
        } else if (for_expr.captures.items.len == 2) {
            // Two captures: for (array, 0..) |value, index|
            const value_capture = for_expr.captures.items[0];
            const index_capture = for_expr.captures.items[1];
            
            // Generate: for (const [index, value] of array.entries())
            try self.write("for (const [");
            if (std.mem.eql(u8, index_capture.name, "_")) {
                try self.write("_");
            } else {
                try self.write(index_capture.name);
            }
            try self.write(", ");
            if (std.mem.eql(u8, value_capture.name, "_")) {
                try self.write("_");
            } else {
                try self.write(value_capture.name);
            }
            try self.write("] of ");
            try self.generateNode(for_expr.iterable);
            try self.write(".entries()) ");
        } else {
            // Fallback for unexpected capture count
            try self.write("/* Unsupported for loop capture pattern */ for (const item of ");
            try self.generateNode(for_expr.iterable);
            try self.write(") ");
        }
        
        // Generate the body with proper block handling
        const body_node = self.arena.getNode(for_expr.body);
        if (body_node != null and body_node.?.data == .block) {
            try self.generateFunctionBodyBlock(body_node.?.data.block);
        } else {
            try self.write("{\n");
            self.indent_level += 1;
            try self.writeIndent();
            try self.generateNode(for_expr.body);
            try self.write("\n");
            self.indent_level -= 1;
            try self.write("}");
        }
    }
    
    /// Generate a range-based for loop
    fn generateRangeForLoop(self: *JSCodegen, for_expr: anytype) !void {
        const iterable_node = self.arena.getNode(for_expr.iterable).?;
        const range_expr = iterable_node.data.range_expr;
        
        if (for_expr.captures.items.len == 1) {
            // for (0..=10) |i| becomes for (let i = 0; i <= 10; i++)
            const capture = for_expr.captures.items[0];
            const capture_name = if (std.mem.eql(u8, capture.name, "_")) "i" else capture.name;
            
            try self.writeFormatted("for (let {s} = ", .{capture_name});
            
            // Start value
            if (range_expr.start) |start_id| {
                try self.generateNode(start_id);
            } else {
                try self.write("0"); // Default start for ..<end
            }
            
            try self.writeFormatted("; {s} ", .{capture_name});
            
            // Condition
            if (range_expr.inclusive) {
                try self.write("<= ");
            } else {
                try self.write("< ");
            }
            
            // End value
            if (range_expr.end) |end_id| {
                try self.generateNode(end_id);
            } else {
                try self.write("Infinity"); // Unbounded range
            }
            
            try self.writeFormatted("; {s}++) ", .{capture_name});
        } else {
            // Multiple captures not supported for ranges, fallback
            try self.write("/* Range for loop with multiple captures not supported */ ");
        }
        
        // Generate the body with proper block handling
        const body_node = self.arena.getNode(for_expr.body);
        if (body_node != null and body_node.?.data == .block) {
            try self.generateFunctionBodyBlock(body_node.?.data.block);
        } else {
            try self.write("{\n");
            self.indent_level += 1;
            try self.writeIndent();
            try self.generateNode(for_expr.body);
            try self.write("\n");
            self.indent_level -= 1;
            try self.write("}");
        }
    }
    
    /// Generate a range expression (when used standalone)
    fn generateRangeExpr(self: *JSCodegen, range_expr: anytype) !void {
        // Generate a JavaScript array for the range
        try self.write("Array.from({length: ");
        
        // Calculate length
        if (range_expr.start != null and range_expr.end != null) {
            try self.write("(");
            try self.generateNode(range_expr.end.?);
            try self.write(" - ");
            try self.generateNode(range_expr.start.?);
            if (range_expr.inclusive) {
                try self.write(" + 1");
            }
            try self.write(")");
        } else if (range_expr.end != null) {
            // ..<end or ..=end
            try self.generateNode(range_expr.end.?);
            if (range_expr.inclusive) {
                try self.write(" + 1");
            }
        } else {
            try self.write("0"); // Fallback
        }
        
        try self.write("}, (_, i) => ");
        
        // Calculate actual value
        if (range_expr.start != null) {
            try self.write("(");
            try self.generateNode(range_expr.start.?);
            try self.write(" + i)");
        } else {
            try self.write("i");
        }
        
        try self.write(")");
    }

    fn generateBlock(self: *JSCodegen, block: anytype) !void {
        const is_root_level = (self.indent_level == 0);
        
        if (!is_root_level) {
            try self.write("{\n");
            self.indent_level += 1;
        }
        
        // Check if this block contains a main function (for root-level blocks)
        var has_main_function = false;
        if (is_root_level) { // Only check at root level
            for (block.statements.items) |stmt_id| {
                const stmt_node = self.arena.getNodeConst(stmt_id) orelse continue;
                if (stmt_node.data == .function_decl) {
                    const func = stmt_node.data.function_decl;
                    if (std.mem.eql(u8, func.name, "main")) {
                        has_main_function = true;
                        break;
                    }
                }
            }
        }
        
        for (block.statements.items) |stmt_id| {
            const stmt_node = self.arena.getNodeConst(stmt_id) orelse continue;
            
            // Skip dummy literal expressions at root level (like literal 0)
            if (is_root_level and stmt_node.data == .literal) {
                const literal = stmt_node.data.literal;
                if (literal == .integer and literal.integer.value == 0) {
                    continue;
                }
            }
            
            // Check if this is a statement that needs a newline
            const needs_newline = switch (stmt_node.data) {
                .var_decl, .function_decl, .struct_decl, .enum_decl, .return_stmt => true,
                .if_expr, .while_expr, .for_expr => true,
                else => false,
            };
            
            if (needs_newline) {
                if (!is_root_level) try self.writeIndent();
                try self.generateNode(stmt_id);
                try self.write("\n");
            } else {
                // For expression statements, add semicolon
                if (!is_root_level) try self.writeIndent();
                try self.generateNode(stmt_id);
                try self.write(";\n");
            }
        }
        
        if (!is_root_level) {
            self.indent_level -= 1;
            try self.write("}");
        }
        
        // Add main function call after the root block for root level
        if (is_root_level and has_main_function) {
            try self.write("\n\nmain();\n");
        }
    }

    fn generateFunctionBodyBlock(self: *JSCodegen, block: anytype) !void {
        try self.write("{\n");
        self.indent_level += 1;
        
        for (block.statements.items) |stmt_id| {
            const stmt_node = self.arena.getNodeConst(stmt_id) orelse continue;
            
            // Check if this is a statement that needs a newline
            const needs_newline = switch (stmt_node.data) {
                .var_decl, .function_decl, .struct_decl, .enum_decl, .return_stmt => true,
                .if_expr, .while_expr, .for_expr => true,
                else => false,
            };
            
            if (needs_newline) {
                try self.writeIndent();
                try self.generateNode(stmt_id);
                try self.write("\n");
            } else {
                // For expression statements, add semicolon
                try self.writeIndent();
                try self.generateNode(stmt_id);
                try self.write(";\n");
            }
        }
        
        self.indent_level -= 1;
        try self.write("}");
    }

    fn generateStructInit(self: *JSCodegen, struct_init: anytype) !void {
        if (struct_init.type_name) |type_name| {
            // Constructor call - generate as new TypeName(args)
            try self.writeFormatted("new {s}(", .{type_name});
            for (struct_init.fields.items, 0..) |field, i| {
                if (i > 0) try self.write(", ");
                try self.generateNode(field.value);
            }
            try self.write(")");
        } else {
            // Object literal - generate as { field: value, ... }
            try self.write("{ ");
            for (struct_init.fields.items, 0..) |field, i| {
                if (i > 0) try self.write(", ");
                
                // Check if field name needs quotes
                if (isValidJSIdentifier(field.name)) {
                    try self.writeFormatted("{s}: ", .{field.name});
                } else {
                    try self.writeFormatted("\"{s}\": ", .{field.name});
                }
                try self.generateNode(field.value);
            }
            try self.write(" }");
        }
    }

    fn generateArrayInit(self: *JSCodegen, array: anytype) !void {
        try self.write("[");
        for (array.elements.items, 0..) |element, i| {
            if (i > 0) try self.write(", ");
            try self.generateNode(element);
        }
        try self.write("]");
    }

    fn generateGenericTypeExpr(self: *JSCodegen, generic: anytype) !void {
        // Handle generic type expressions like std.List(T)
        const base_node = self.arena.getNodeConst(generic.base_type);
        if (base_node) |base| {
            if (base.data == .member_expr) {
                const member = base.data.member_expr;
                // Check if this is std.List
                if (std.mem.eql(u8, member.field, "List")) {
                    const obj_node = self.arena.getNodeConst(member.object);
                    if (obj_node) |obj| {
                        if (obj.data == .identifier) {
                            const ident = obj.data.identifier;
                            if (std.mem.eql(u8, ident.name, "std")) {
                                // For std.List(T), we just use the JavaScript Array constructor
                                // The type parameter is not needed at runtime
                                try self.write("Array");
                                return;
                            }
                        }
                    }
                }
            }
        }
        
        // Default: generate the base type
        try self.generateNode(generic.base_type);
    }

    fn generateReturnStmt(self: *JSCodegen, ret: anytype) !void {
        try self.write("return");
        if (ret.value) |value| {
            try self.write(" ");
            try self.generateNode(value);
        }
        try self.write(";");
    }

    fn generateImportDecl(self: *JSCodegen, import_decl: anytype) !void {
        // For JavaScript output, we don't generate import statements
        // since std functions are handled specially (e.g., std.debug.print -> console.log)
        _ = self;
        _ = import_decl;
        // Generate nothing
    }

    fn binaryOpToJS(self: *JSCodegen, op: ast.BinaryOp) []const u8 {
        _ = self;
        return switch (op) {
            .assign => "=",
            .logical_or => "||",
            .logical_and => "&&",
            .eq => "===",
            .ne => "!==",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .power => "**",
            .concat => "+", // String concatenation uses + in JS
            .bit_and => "&",
            .bit_or => "|",
            .bit_xor => "^",
            .shl => "<<",
            .shr => ">>",
            .range => "/* range operator not supported */",
            .pipe => "/* pipe operator not supported */",
        };
    }

    fn unaryOpToJS(self: *JSCodegen, op: ast.UnaryOp) []const u8 {
        _ = self;
        return switch (op) {
            .negate => "-",
            .not => "!",
            .bit_not => "~",
            .deref => "/* deref not supported */",
            .address_of => "/* address_of not supported */",
        };
    }

    // Helper functions for better code generation
    fn isValidJSIdentifier(name: []const u8) bool {
        if (name.len == 0) return false;
        
        // Check if starts with letter or underscore
        const first_char = name[0];
        if (!((first_char >= 'a' and first_char <= 'z') or 
              (first_char >= 'A' and first_char <= 'Z') or 
              first_char == '_' or first_char == '$')) {
            return false;
        }
        
        // Check remaining characters
        for (name[1..]) |char| {
            if (!((char >= 'a' and char <= 'z') or 
                  (char >= 'A' and char <= 'Z') or 
                  (char >= '0' and char <= '9') or 
                  char == '_' or char == '$')) {
                return false;
            }
        }
        
        // Check if it's a reserved keyword
        const js_keywords = [_][]const u8{
            "break", "case", "catch", "class", "const", "continue", "debugger", "default", 
            "delete", "do", "else", "export", "extends", "finally", "for", "function", 
            "if", "import", "in", "instanceof", "let", "new", "return", "super", "switch", 
            "this", "throw", "try", "typeof", "var", "void", "while", "with", "yield",
            "enum", "implements", "interface", "package", "private", "protected", "public", "static"
        };
        
        for (js_keywords) |keyword| {
            if (std.mem.eql(u8, name, keyword)) {
                return false;
            }
        }
        
        return true;
    }
    
    fn sanitizeIdentifier(self: *JSCodegen, name: []const u8) ![]const u8 {
        if (isValidJSIdentifier(name)) {
            return name;
        }
        
        // If invalid, prefix with underscore and sanitize
        var sanitized = std.ArrayList(u8).init(self.allocator);
        defer sanitized.deinit();
        
        try sanitized.append('_');
        for (name) |char| {
            if ((char >= 'a' and char <= 'z') or 
                (char >= 'A' and char <= 'Z') or 
                (char >= '0' and char <= '9') or 
                char == '_') {
                try sanitized.append(char);
            } else {
                try sanitized.append('_');
            }
        }
        
        return try sanitized.toOwnedSlice();
    }
    pub fn howlTypeToJS(self: *JSCodegen, howl_type: ast.Type) ![]const u8 {
        _ = self;
        return switch (howl_type.data) {
            .primitive => |prim| switch (prim) {
                .bool => "boolean",
                .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64 => "number",
                .char, .string => "string",
                .void => "undefined",
                .type => "Function", // Type constructors become functions
            },
            .array => "Array",
            .@"struct" => |s| s.name,
            .custom_struct => |s| s.name,
            else => "any",
        };
    }

    // Generate type annotations for TypeScript compatibility
    pub fn generateTypeAnnotation(self: *JSCodegen, howl_type: ast.Type) !void {
        const js_type = try self.howlTypeToJS(howl_type);
        try self.writeFormatted(": {s}", .{js_type});
    }

    // ========== NEW COMPILE-TIME SYSTEM SUPPORT ==========
    
    fn generateExternFnDecl(self: *JSCodegen, extern_fn: anytype) !void {
        // Generate JavaScript implementation using compile-time evaluation
        const fn_name = extern_fn.name;
        
        // Generate the function signature
        try self.writeFormatted("function {s}(", .{fn_name});
        
        // Generate parameters
        for (extern_fn.params.items, 0..) |param, i| {
            if (i > 0) try self.write(", ");
            try self.write(param.name);
        }
        try self.write(") {\n");
        
        self.indent_level += 1;
        
        // Process the function body using compile-time evaluation
        try self.generateNode(extern_fn.compile_time_body);
        
        self.indent_level -= 1;
        try self.writeLine("}");
    }
    
    fn generateCompileTargetExpr(self: *JSCodegen) !void {
        try self.write("\"javascript\"");
    }
    
    fn generateCompileInsertExpr(self: *JSCodegen, insert: anytype) !void {
        // For JavaScript target, evaluate and insert the code directly
        // Generate raw JavaScript code insertion
        try self.writeFormatted("{s}", .{insert.code});
    }
    
    fn generateMatchCompileExpr(self: *JSCodegen, match_expr: anytype) !void {
        // Find the arm that matches JavaScript target
        for (match_expr.arms.items) |arm| {
            if (arm.target == .javascript) {
                // Generate the body for JavaScript target
                try self.generateNode(arm.body);
                break;
            }
        }
        
        // If no JavaScript case found, generate empty code or comment
        try self.write("/* No JavaScript implementation found */");
    }
    
    fn generateMatchExpr(self: *JSCodegen, match_expr: anytype) !void {
        // Generate match expression as a JavaScript function
        // that returns the appropriate value based on the pattern
        
        try self.write("(() => {\n");
        self.indent_level += 1;
        
        // Generate a temporary variable to hold the matched expression
        try self.writeIndent();
        try self.write("const __match_value = ");
        try self.generateNode(match_expr.expression);
        try self.write(";\n");
        
        // Generate each match arm as an if-else chain
        var first_arm = true;
        for (match_expr.arms.items) |arm| {
            try self.writeIndent();
            
            if (first_arm) {
                try self.write("if (");
                first_arm = false;
            } else {
                try self.write("} else if (");
            }
            
            // Generate the pattern matching condition
            try self.generateMatchPattern(arm.pattern);
            try self.write(") {\n");
            
            self.indent_level += 1;
            
            // Check if the body is a block - if so, handle it differently
            const arm_body_node = self.arena.getNodeConst(arm.body);
            if (arm_body_node) |body_node| {
                if (body_node.data == .block) {
                    // For blocks, generate a special match block
                    try self.generateMatchBlock(body_node.data.block);
                } else {
                    // For single expressions, use return statement
                    try self.writeIndent();
                    try self.write("return ");
                    try self.generateNode(arm.body);
                    try self.write(";\n");
                }
            } else {
                // Fallback for null body
                try self.writeIndent();
                try self.write("return ");
                try self.generateNode(arm.body);
                try self.write(";\n");
            }
            
            self.indent_level -= 1;
        }
        
        // Close the if-else chain and the function
        try self.writeIndent();
        try self.write("}\n");
        
        // Add a default case if no wildcard pattern was found
        // (This should be caught by semantic analysis, but just in case)
        try self.writeIndent();
        try self.write("throw new Error('Match expression did not handle all cases');\n");
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("})()");
    }
    
    fn generateMatchPattern(self: *JSCodegen, pattern: ast.MatchPattern) !void {
        switch (pattern) {
            .wildcard => {
                // Wildcard always matches
                try self.write("true");
            },
            .literal => |literal| {
                try self.write("__match_value === ");
                try self.generateLiteral(literal);
            },
            .identifier => |_| {
                // Variable binding - always matches, but in JS we just check true
                // The actual binding would need more complex handling
                try self.write("true");
            },
            .enum_member => |member_name| {
                // For enum member patterns, check against the string representation
                try self.writeFormatted("__match_value === \"{s}\"", .{member_name});
            },
            .comparison => |comp| {
                // Generate comparison: __match_value < value, __match_value > value, etc.
                try self.write("__match_value ");
                
                const op_str = switch (comp.operator) {
                    .LessThan => "< ",
                    .GreaterThan => "> ",
                    .LessThanEqual => "<= ",
                    .GreaterThanEqual => ">= ",
                    .EqualEqual => "=== ",
                    .NotEqual => "!== ",
                    else => "=== ", // fallback
                };
                
                try self.write(op_str);
                try self.generateNode(comp.value);
            },
            .range => {
                // TODO: Implement range pattern matching
                try self.write("/* Range pattern not implemented yet */");
            },
            .tuple => {
                // TODO: Implement tuple pattern matching  
                try self.write("/* Tuple pattern not implemented yet */");
            },
            .array => {
                // TODO: Implement array pattern matching
                try self.write("/* Array pattern not implemented yet */");
            },
            .guard => {
                // TODO: Implement guard pattern matching
                try self.write("/* Guard pattern not implemented yet */");
            },
        }
    }
    
    fn generateMatchBlock(self: *JSCodegen, block: anytype) !void {
        // Generate a block for use in match expressions
        // The last statement should be a return statement
        
        for (block.statements.items, 0..) |stmt_id, index| {
            const is_last = (index == block.statements.items.len - 1);
            
            try self.writeIndent();
            
            if (is_last) {
                // Last statement should be a return
                try self.write("return ");
                try self.generateNode(stmt_id);
                try self.write(";\n");
            } else {
                // Regular statements
                try self.generateNode(stmt_id);
                try self.write(";\n");
            }
        }
    }
    
    fn evaluateComptimeExpression(self: *JSCodegen, expr_id: ast.NodeId) anyerror!void {
        const node = self.arena.getNodeConst(expr_id) orelse return;
        
        switch (node.data) {
            .literal => |literal| {
                switch (literal) {
                    .string => |str_lit| {
                        // For string literals in @compile.insert, output the raw JavaScript code
                        try self.write(str_lit.value);
                    },
                    else => try self.generateLiteral(literal),
                }
            },
            .binary_expr => |binary| {
                // Handle string concatenation for building JavaScript code
                if (binary.op == .add) {
                    try self.evaluateComptimeExpression(binary.left);
                    try self.evaluateComptimeExpression(binary.right);
                } else {
                    try self.generateBinaryExpr(binary);
                }
            },
            else => try self.generateNode(expr_id),
        }
    }
    
    // Enhanced format generation function for advanced debug.print support in JavaScript
    fn generateJSFormatCode(self: *JSCodegen, format_string: []const u8, args_node_id: ast.NodeId) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();
        
        // Parse format string and convert to JavaScript template literals or formatted output
        const args_node = self.arena.getNodeConst(args_node_id);
        
        if (args_node) |node| {
            switch (node.data) {
                .struct_init => |struct_init| {
                    // Generate JavaScript template literal or formatted console.log
                    try result.appendSlice("console.log(");
                    
                    // Convert format string
                    const converted = try self.convertHowlFormatToJS(format_string, struct_init.fields.items.len);
                    defer self.allocator.free(converted);
                    
                    try result.appendSlice("'");
                    try result.appendSlice(converted);
                    try result.appendSlice("'");
                    
                    // Add arguments
                    for (struct_init.fields.items, 0..) |field_init, index| {
                        try result.appendSlice(", ");
                        
                        const value_node = self.arena.getNodeConst(field_init.value);
                        if (value_node) |val_node| {
                            switch (val_node.data) {
                                .literal => |lit| {
                                    switch (lit) {
                                        .string => |s| {
                                            try result.appendSlice("'");
                                            try result.appendSlice(s.value);
                                            try result.appendSlice("'");
                                        },
                                        .integer => |i| {
                                            try result.writer().print("{d}", .{i.value});
                                        },
                                        .float => |f| {
                                            try result.writer().print("{d}", .{f.value});
                                        },
                                        .boolean => |b| {
                                            try result.appendSlice(if (b.value) "true" else "false");
                                        },
                                        else => try result.appendSlice("undefined"),
                                    }
                                },
                                .identifier => |id| try result.appendSlice(id.name),
                                else => try result.appendSlice("unknownValue"),
                            }
                        } else {
                            try result.appendSlice("null");
                        }
                        _ = index; // suppress unused variable warning
                    }
                    
                    try result.appendSlice(")");
                },
                else => {
                    try result.appendSlice("console.log('Invalid arguments')");
                },
            }
        } else {
            try result.appendSlice("console.log('No arguments')");
        }
        
        return try self.allocator.dupe(u8, result.items);
    }
    
    // Convert Howl format string to JavaScript format
    fn convertHowlFormatToJS(self: *JSCodegen, howl_format: []const u8, arg_count: usize) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();
        
        var placeholder_index: usize = 0;
        var i: usize = 0;
        
        while (i < howl_format.len) {
            if (howl_format[i] == '{' and i + 1 < howl_format.len) {
                // Find the end of the format specifier
                var end_pos = i + 1;
                while (end_pos < howl_format.len and howl_format[end_pos] != '}') {
                    end_pos += 1;
                }
                
                if (end_pos < howl_format.len and placeholder_index < arg_count) {
                    const format_spec = howl_format[i + 1..end_pos];
                    
                    // Convert format specifier to JavaScript format
                    const js_format = try self.convertJSFormatSpec(format_spec, placeholder_index);
                    try result.appendSlice(js_format);
                    
                    placeholder_index += 1;
                    i = end_pos + 1; // Skip past '}'
                } else {
                    // Malformed or too many placeholders, just copy literally
                    try result.append(howl_format[i]);
                    i += 1;
                }
            } else if (howl_format[i] == '\\' and i + 1 < howl_format.len) {
                // Handle escape sequences
                const next_char = howl_format[i + 1];
                switch (next_char) {
                    'n' => try result.appendSlice("\\n"),
                    't' => try result.appendSlice("\\t"),
                    'r' => try result.appendSlice("\\r"),
                    '\\' => try result.appendSlice("\\\\"),
                    '\'' => try result.appendSlice("\\'"),
                    else => {
                        try result.append(howl_format[i]);
                        try result.append(next_char);
                    },
                }
                i += 2;
            } else if (howl_format[i] == '\'') {
                // Escape single quotes
                try result.appendSlice("\\'");
                i += 1;
            } else {
                try result.append(howl_format[i]);
                i += 1;
            }
        }
        
        return try self.allocator.dupe(u8, result.items);
    }
    
    // Convert individual format specification for JavaScript
    fn convertJSFormatSpec(self: *JSCodegen, spec: []const u8, arg_index: usize) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();
        
        if (spec.len == 0) {
            // Default format - just use template literal syntax
            try result.writer().print("${{}}", .{});
            return try self.allocator.dupe(u8, result.items);
        }
        
        // Split on ':' if present
        var type_part = spec;
        var format_part: []const u8 = "";
        
        if (std.mem.indexOf(u8, spec, ":")) |colon_pos| {
            type_part = spec[0..colon_pos];
            format_part = spec[colon_pos + 1..];
        }
        
        // Handle type specifiers
        if (std.mem.eql(u8, type_part, "d")) {
            // Integer format
            if (format_part.len == 0) {
                try result.writer().print("${{}}", .{});
            } else {
                // For complex integer formatting, use helper function
                try result.writer().print("${{formatInt(arguments[{}], '{s}')}}", .{ arg_index, format_part });
            }
        } else if (std.mem.eql(u8, type_part, "s")) {
            // String format
            if (format_part.len == 0) {
                try result.writer().print("${{}}", .{});
            } else {
                // For complex string formatting, use helper function
                try result.writer().print("${{formatString(arguments[{}], '{s}')}}", .{ arg_index, format_part });
            }
        } else if (std.mem.eql(u8, type_part, "f")) {
            // Float format
            if (format_part.len == 0) {
                try result.writer().print("${{}}", .{});
            } else if (std.mem.startsWith(u8, format_part, ".")) {
                // Handle precision specifier
                const precision_str = format_part[1..];
                if (std.mem.eql(u8, precision_str, "2")) {
                    try result.writer().print("${{arguments[{}].toFixed(2)}}", .{arg_index});
                } else if (std.mem.eql(u8, precision_str, "1")) {
                    try result.writer().print("${{arguments[{}].toFixed(1)}}", .{arg_index});
                } else if (std.mem.eql(u8, precision_str, "3")) {
                    try result.writer().print("${{arguments[{}].toFixed(3)}}", .{arg_index});
                } else {
                    try result.writer().print("${{arguments[{}].toFixed(2)}}", .{arg_index}); // Default precision
                }
            } else {
                try result.writer().print("${{}}", .{});
            }
        } else {
            // Default case
            try result.writer().print("${{}}", .{});
        }
        
        return try self.allocator.dupe(u8, result.items);
    }
    
    // ============================================================================
    // Error Handling Generation
    // ============================================================================
    
    fn generateTryExpr(self: *JSCodegen, try_expr: anytype) !void {
        // In JavaScript, try expressions are handled with try-catch blocks
        try self.write("(function() {\n");
        self.indent_level += 1;
        
        try self.writeIndent();
        try self.write("try {\n");
        self.indent_level += 1;
        
        try self.writeIndent();
        try self.write("return ");
        try self.generateNode(try_expr.expression);
        try self.write(";\n");
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("} catch (error) {\n");
        self.indent_level += 1;
        
        try self.writeIndent();
        try self.write("throw error; // Propagate error\n");
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("}\n");
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("})()");
    }
    
    fn generateCatchExpr(self: *JSCodegen, catch_expr: anytype) !void {
        // Generate a try-catch block in JavaScript for the expression being caught
        try self.write("(function() {\n");
        self.indent_level += 1;
        
        try self.writeIndent();
        try self.write("try {\n");
        self.indent_level += 1;
        
        try self.writeIndent();
        try self.write("return ");
        try self.generateNode(catch_expr.expression);
        try self.write(";\n");
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("} catch (");
        
        // Handle error capture variable
        if (catch_expr.error_capture) |error_var| {
            try self.write(error_var);
        } else {
            try self.write("error");
        }
        
        try self.write(") {\n");
        self.indent_level += 1;
        
        try self.writeIndent();
        try self.write("return ");
        
        // Generate catch body or fallback value
        if (catch_expr.catch_body) |body| {
            try self.generateNode(body);
        } else if (catch_expr.fallback_value) |fallback| {
            try self.generateNode(fallback);
        } else {
            try self.write("null");
        }
        
        try self.write(";\n");
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("}\n");
        
        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("})()");
    }
    
    fn generateErrorUnionType(self: *JSCodegen) !void {
        // Error union types don't generate specific code in JavaScript
        try self.write("/* error_union_type */");
    }
    
    fn generateErrorLiteral(self: *JSCodegen, error_literal: anytype) !void {
        // Error literals generate as Error objects in JavaScript
        try self.writeFormatted("new Error(\"{s}\")", .{error_literal.name});
    }
    
    fn generateErrorSetDecl(self: *JSCodegen, error_set_decl: anytype) !void {
        // Generate error set as a JavaScript object with error constants
        try self.writeFormatted("const {s} = {{", .{error_set_decl.name});
        
        for (error_set_decl.errors.items, 0..) |error_name, i| {
            if (i > 0) try self.write(", ");
            try self.writeFormatted("\n    {s}: \"{s}\"", .{ error_name, error_name });
        }
        
        try self.write("\n};");
    }
};

// Helper function to generate JavaScript from AST
pub fn generateJS(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer, root_node_id: ast.NodeId) ![]const u8 {
    var codegen = JSCodegen.init(allocator, arena, semantic_analyzer);
    defer codegen.deinit();
    
    return try codegen.generate(root_node_id);
    }
    
test "Basic JavaScript generation" {
    const testing = std.testing;
    var arena = ast.AstArena.init(testing.allocator);
    defer arena.deinit();

    // Create a simple literal expression: 42
    const literal = ast.Literal{ .integer = .{ .value = 42 } };
    const node_id = try ast.createLiteralExpr(&arena, ast.SourceLoc.invalid(), literal);
    
    // Mock semantic analyzer for testing
    var global_scope = SemanticAnalyzer.Scope.init(testing.allocator, null);
    defer global_scope.deinit();
    
    var mock_analyzer = SemanticAnalyzer.SemanticAnalyzer{
        .allocator = testing.allocator,
        .arena = &arena,
        .errors = undefined,
        .current_scope = &global_scope,
        .global_scope = global_scope,
        .file_path = "test",
        .current_function_return_type = null,
        .in_loop = false,
        .type_registry = std.StringHashMap(ast.Type).init(testing.allocator),
        .struct_definitions = std.StringHashMap(SemanticAnalyzer.ComptimeValue).init(testing.allocator),
        .comptime_values = std.StringHashMap(SemanticAnalyzer.ComptimeValue).init(testing.allocator),
    };
    defer mock_analyzer.deinit();

    var codegen = JSCodegen.init(testing.allocator, &arena, &mock_analyzer);
    defer codegen.deinit();

    const result = try codegen.generate(node_id);
    defer testing.allocator.free(result);

    try testing.expectEqualStrings("42", result);
}

test "Binary expression generation" {
    const testing = std.testing;
    var arena = ast.AstArena.init(testing.allocator);
    defer arena.deinit();

    // Create: 2 + 3
    const left_lit = ast.Literal{ .integer = .{ .value = 2 } };
    const right_lit = ast.Literal{ .integer = .{ .value = 3 } };
    const left_id = try ast.createLiteralExpr(&arena, ast.SourceLoc.invalid(), left_lit);
    const right_id = try ast.createLiteralExpr(&arena, ast.SourceLoc.invalid(), right_lit);
    const binary_id = try ast.createBinaryExpr(&arena, ast.SourceLoc.invalid(), .add, left_id, right_id);

    // Mock semantic analyzer for testing
    var global_scope = SemanticAnalyzer.Scope.init(testing.allocator, null);
    defer global_scope.deinit();
    
    var mock_analyzer = SemanticAnalyzer.SemanticAnalyzer{
        .allocator = testing.allocator,
        .arena = &arena,
        .errors = undefined,
        .current_scope = &global_scope,
        .global_scope = global_scope,
        .file_path = "test",
        .current_function_return_type = null,
        .in_loop = false,
        .type_registry = std.StringHashMap(ast.Type).init(testing.allocator),
        .struct_definitions = std.StringHashMap(SemanticAnalyzer.ComptimeValue).init(testing.allocator),
        .comptime_values = std.StringHashMap(SemanticAnalyzer.ComptimeValue).init(testing.allocator),
    };
    defer mock_analyzer.deinit();

    var codegen = JSCodegen.init(testing.allocator, &arena, &mock_analyzer);
    defer codegen.deinit();

    const result = try codegen.generate(binary_id);
    defer testing.allocator.free(result);

    try testing.expectEqualStrings("(2 + 3)", result);
}