const std = @import("std");
const semantic_analysis = @import("semantic_analysis.zig");
const SemanticAnalyzer = semantic_analysis.SemanticAnalyzer;

const CodeGenError = error{
    OutOfMemory,
    InvalidNode,
    UnsupportedOperation,
};

// convert Howl SementicNodes to JS
const CodeGeneratorJs = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    semantic_analyzer: *SemanticAnalyzer,
    outArray: std.ArrayList(u8),
    outWriter: std.ArrayList(u8).Writer,

    pub fn init(
        allocator: std.mem.Allocator,
        semantic_analyzer: *SemanticAnalyzer,
    ) CodeGenError!CodeGeneratorJs {
        var newCodeGen = Self{
            .allocator = allocator,
            .semantic_analyzer = semantic_analyzer,
            .outArray = std.ArrayList(u8).init(allocator),
            .outWriter = undefined,
        };
        newCodeGen.outWriter = newCodeGen.outArray.writer();
        return newCodeGen;
    }

    pub fn deinit(self: *Self) void {
        self.outArray.deinit();
    }

    pub fn generate(self: *Self) CodeGenError!void {
        try self.outArray.ensureTotalCapacity(4097);
        self.outWriter = self.outArray.writer();
        const root = self.semantic_analyzer.ir_root;
        if (root.* != .Program) {
            std.debug.print("Invalid Root Node at JS CodeGen", .{});
            return CodeGenError.InvalidNode;
        }
        for (root.Program.items) |item| {
            switch (item.*) {
                .Function => |func| {
                    try self.generateFunction(func);
                },
                .Variable => |v| {
                    try self.generateVariable(v, true);
                },
                .Constant => |c| {
                    try self.generateConstant(c, true);
                },
                else => {
                    try self.generateNode(item);
                },
            }
        }
    }

    pub fn getResult(self: *Self) []const u8 {
        return self.outArray.items;
    }

    fn writeIndent(self: *Self, indent_level: usize) !void {
        const indent = "  ";
        var i: usize = 0;
        while (i < indent_level) : (i += 1) {
            try self.outWriter.writeAll(indent);
        }
    }

    fn generateFunction(self: *Self, func: semantic_analysis.IrFunction) !void {
        // Generate function declaration
        if (func.is_public) {
            try self.outWriter.writeAll("export ");
        }

        try self.outWriter.writeAll("function ");
        try self.outWriter.writeAll(func.name);
        try self.outWriter.writeAll("(");

        // Generate parameters
        for (func.parameters.items, 0..) |param, i| {
            try self.outWriter.writeAll(param.name);
            if (i < func.parameters.items.len - 1) {
                try self.outWriter.writeAll(", ");
            }
        }

        try self.outWriter.writeAll(") ");

        // Generate function body
        try self.generateNode(func.body);
        try self.outWriter.writeAll("\n\n");
    }

    fn generateVariable(self: *Self, var_decl: semantic_analysis.IrVariable, top_level: bool) CodeGenError!void {
        if (top_level and var_decl.is_public) {
            try self.outWriter.writeAll("export ");
        }

        try self.outWriter.writeAll("let ");
        try self.outWriter.writeAll(var_decl.name);
        try self.outWriter.writeAll(" = ");
        try self.generateNode(var_decl.value);
        try self.outWriter.writeAll(";\n");
    }

    fn generateConstant(self: *Self, const_decl: semantic_analysis.IrVariable, top_level: bool) !void {
        try self.outWriter.writeByte('\n');
        if (top_level and const_decl.is_public) {
            try self.outWriter.writeAll("export ");
        }

        try self.outWriter.writeAll("const ");
        try self.outWriter.writeAll(const_decl.name);
        try self.outWriter.writeAll(" = ");
        try self.generateNode(const_decl.value);
        try self.outWriter.writeAll(";\n");
    }

    fn generateNode(self: *Self, node: *semantic_analysis.IrNode) CodeGenError!void {
        switch (node.*) {
            .Block => |block| try self.generateBlock(block),
            .IntLiteral => |int_lit| try self.generateIntLiteral(int_lit),
            .BoolLiteral => |bool_lit| try self.generateBoolLiteral(bool_lit),
            .StringLiteral => |str_lit| try self.generateStringLiteral(str_lit),
            .Identifier => |ident| try self.generateIdentifier(ident),
            .BinaryOp => |bin_op| try self.generateBinaryOp(bin_op),
            .UnaryOp => |unary_op| try self.generateUnaryOp(unary_op),
            .Call => |call| try self.generateCall(call),
            .Return => |ret_val| try self.generateReturn(ret_val),
            .Variable => |var_decl| try self.generateVariable(var_decl, false),
            .Constant => |const_decl| try self.generateConstant(const_decl, false),
            .Program => |program| try self.generateProgram(program),
            .FloatLiteral => {
                std.debug.print("Warning: FloatLiteral not fully implemented in semantic analyzer\n", .{});
                try self.generateFloatLiteral(node.FloatLiteral);
            },
            .Assignment => |assign| try self.generateAssignment(assign),
            else => {
                std.debug.print("Unhandled IR node type in JS codegen: {s}\n", .{@tagName(node.*)});
                return CodeGenError.InvalidNode;
            },
        }
    }

    fn generateBlock(self: *Self, block: semantic_analysis.IrBlock) !void {
        try self.outWriter.writeAll("{\n");

        for (block.statements.items) |stmt| {
            try self.writeIndent(1);
            try self.generateNode(stmt);

            // Add semicolons for statements that need them
            switch (stmt.*) {
                .Variable, .Constant, .BinaryOp, .UnaryOp, .Call, .Identifier, .Assignment => {
                    try self.outWriter.writeAll(";\n");
                },
                .Return => {
                    try self.outWriter.writeAll("\n");
                },
                else => {},
            }
        }

        try self.outWriter.writeAll("}");
    }

    fn generateIntLiteral(self: *Self, int_lit: semantic_analysis.IrIntLiteral) !void {
        try self.outWriter.print("{d}", .{int_lit.value});
    }

    fn generateFloatLiteral(self: *Self, float_lit: semantic_analysis.IrFloatLiteral) !void {
        try self.outWriter.print("{d}", .{float_lit.value});
    }

    fn generateBoolLiteral(self: *Self, bool_lit: bool) !void {
        try self.outWriter.writeAll(if (bool_lit) "true" else "false");
    }

    fn generateStringLiteral(self: *Self, str_lit: []const u8) !void {
        try self.outWriter.writeAll("\"");

        // Escape special characters in string
        for (str_lit) |char| {
            switch (char) {
                '\n' => try self.outWriter.writeAll("\\n"),
                '\r' => try self.outWriter.writeAll("\\r"),
                '\t' => try self.outWriter.writeAll("\\t"),
                '\"' => try self.outWriter.writeAll("\\\""),
                '\\' => try self.outWriter.writeAll("\\\\"),
                else => try self.outWriter.writeByte(char),
            }
        }

        try self.outWriter.writeAll("\"");
    }

    fn generateIdentifier(self: *Self, ident: semantic_analysis.IrIdentifier) !void {
        try self.outWriter.writeAll(ident.name);
    }

    fn generateBinaryOp(self: *Self, bin_op: semantic_analysis.IrBinaryOp) !void {
        try self.outWriter.writeAll("(");
        try self.generateNode(bin_op.left);

        const op_str = switch (bin_op.op) {
            .Add => " + ",
            .Subtract => " - ",
            .Multiply => " * ",
            .Divide => " / ",
            .Modulo => " % ",
            .Equals => " === ",
            .NotEquals => " !== ",
            .LessThan => " < ",
            .GreaterThan => " > ",
            .LessThanEquals => " <= ",
            .GreaterThanEquals => " >= ",
            .LogicalAnd => " && ",
            .LogicalOr => " || ",
            else => {
                std.debug.print("Warning: Binary operation {s} not fully implemented in semantic analyzer\n", .{@tagName(bin_op.op)});
                return CodeGenError.UnsupportedOperation;
            },
        };

        try self.outWriter.writeAll(op_str);
        try self.generateNode(bin_op.right);
        try self.outWriter.writeAll(")");
    }

    fn generateUnaryOp(self: *Self, unary_op: semantic_analysis.IrUnaryOp) !void {
        try self.outWriter.writeAll("(");

        const op_str = switch (unary_op.op) {
            .Negate => "-",
            .LogicalNot => "!",
            .BitwiseNot => {
                std.debug.print("Warning: BitwiseNot not fully implemented in semantic analyzer\n", .{});
                return CodeGenError.UnsupportedOperation;
            },
        };

        try self.outWriter.writeAll(op_str);
        try self.generateNode(unary_op.operand);
        try self.outWriter.writeAll(")");
    }

    fn generateCall(self: *Self, call: semantic_analysis.IrCall) !void {
        try self.generateNode(call.callee);
        try self.outWriter.writeAll("(");

        for (call.arguments.items, 0..) |arg, i| {
            try self.generateNode(arg);
            if (i < call.arguments.items.len - 1) {
                try self.outWriter.writeAll(", ");
            }
        }

        try self.outWriter.writeAll(")");
    }

    fn generateReturn(self: *Self, ret_val: *semantic_analysis.IrNode) !void {
        if (ret_val.* == .IntLiteral and ret_val.IntLiteral.value == 0 and ret_val.IntLiteral.type == .Void) {
            try self.outWriter.writeAll("return");
        } else {
            try self.outWriter.writeAll("return ");
            try self.generateNode(ret_val);
        }
    }

    fn generateProgram(self: *Self, program: std.ArrayList(*semantic_analysis.IrNode)) !void {
        for (program.items, 0..) |item, i| {
            try self.generateNode(item);
            if (i < program.items.len - 1) {
                try self.outWriter.writeAll("\n");
            }
        }
    }

    /// Generate code for assignment statements
    fn generateAssignment(self: *Self, assign: semantic_analysis.IrAssignment) !void {
        // Generate target (left side of assignment)
        try self.generateNode(assign.target);

        // Generate assignment operator
        try self.outWriter.writeAll(" = ");

        // Generate value (right side of assignment)
        try self.generateNode(assign.value);
        try self.outWriter.writeAll(";\n");
    }
};

// Export the CodeGeneratorJs struct
pub const CodeGenerator = CodeGeneratorJs;
