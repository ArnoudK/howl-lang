const std = @import("std");
const SeaOfNodes = @import("sea_of_nodes_ir.zig").SeaOfNodes;
const IrNodeId = @import("sea_of_nodes_ir.zig").IrNodeId;
const IrOp = @import("sea_of_nodes_ir.zig").IrOp;
const IrConstant = @import("sea_of_nodes_ir.zig").IrConstant;
const INVALID_IR_NODE_ID = @import("sea_of_nodes_ir.zig").INVALID_IR_NODE_ID;
const ErrorSystem = @import("error_system.zig");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;

// ============================================================================
// JavaScript Code Generation from Sea-of-Nodes IR
// ============================================================================

/// JavaScript code generator that consumes Sea-of-Nodes IR
pub const JsIrCodegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    indent_level: u32,
    variable_counter: u32,

    // Value tracking for IR nodes
    node_values: std.AutoHashMap(IrNodeId, []const u8),

    pub fn init(allocator: std.mem.Allocator) JsIrCodegen {
        return JsIrCodegen{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
            .variable_counter = 0,
            .node_values = std.AutoHashMap(IrNodeId, []const u8).init(allocator),
        };
    }

    pub fn deinit(self: *JsIrCodegen) void {
        // Clean up allocated variable names
        var iterator = self.node_values.iterator();
        while (iterator.next()) |entry| {
            self.allocator.free(entry.value_ptr.*);
        }
        self.node_values.deinit();
        self.output.deinit();
    }

    /// Generate JavaScript code from Sea-of-Nodes IR
    pub fn generate(self: *JsIrCodegen, ir: *const SeaOfNodes, semantic_analyzer: *const SemanticAnalyzer) ![]const u8 {
        _ = semantic_analyzer; // TODO: Use semantic analysis info

        // Header
        try self.writeLine("// Generated JavaScript from Sea-of-Nodes IR");
        try self.writeLine("");

        // Get topological order of nodes for linear code generation
        const sorted_nodes = try ir.topologicalSort(self.allocator);
        defer self.allocator.free(sorted_nodes);

        // Generate main function
        try self.writeLine("function main() {");
        self.indent();

        // Process each node in topological order
        for (sorted_nodes) |node_id| {
            try self.generateNodeCode(ir, node_id);
        }

        self.dedent();
        try self.writeLine("}");
        try self.writeLine("");
        try self.writeLine("// Run the main function");
        try self.writeLine("main();");

        return self.output.toOwnedSlice();
    }

    /// Generate code for a specific IR node
    fn generateNodeCode(self: *JsIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId) !void {
        const node = ir.getNode(node_id) orelse return;

        switch (node.op) {
            .start => {
                // Start node doesn't generate code, just a comment
                try self.writeLine("// Program start");
            },

            .constant => {
                const var_name = try self.generateVariableName();
                const const_str = try node.data.constant.toString(self.allocator);
                defer self.allocator.free(const_str);

                try self.writeLineFormatted("const {s} = {s};", .{ var_name, const_str });
                try self.node_values.put(node_id, var_name);
            },

            .add, .sub, .mul, .div, .mod => {
                if (node.inputs.len >= 2) {
                    const left_val = self.node_values.get(node.inputs[0]) orelse "undefined";
                    const right_val = self.node_values.get(node.inputs[1]) orelse "undefined";
                    const var_name = try self.generateVariableName();

                    const op_str = switch (node.op) {
                        .add => "+",
                        .sub => "-",
                        .mul => "*",
                        .div => "/",
                        .mod => "%",
                        else => "?",
                    };

                    try self.writeLineFormatted("const {s} = {s} {s} {s};", .{ var_name, left_val, op_str, right_val });
                    try self.node_values.put(node_id, var_name);
                }
            },

            .eq, .ne, .lt, .le, .gt, .ge => {
                if (node.inputs.len >= 2) {
                    const left_val = self.node_values.get(node.inputs[0]) orelse "undefined";
                    const right_val = self.node_values.get(node.inputs[1]) orelse "undefined";
                    const var_name = try self.generateVariableName();

                    const op_str = switch (node.op) {
                        .eq => "===",
                        .ne => "!==",
                        .lt => "<",
                        .le => "<=",
                        .gt => ">",
                        .ge => ">=",
                        else => "?",
                    };

                    try self.writeLineFormatted("const {s} = {s} {s} {s};", .{ var_name, left_val, op_str, right_val });
                    try self.node_values.put(node_id, var_name);
                }
            },

            .logical_and, .logical_or => {
                if (node.inputs.len >= 2) {
                    const left_val = self.node_values.get(node.inputs[0]) orelse "undefined";
                    const right_val = self.node_values.get(node.inputs[1]) orelse "undefined";
                    const var_name = try self.generateVariableName();

                    const op_str = switch (node.op) {
                        .logical_and => "&&",
                        .logical_or => "||",
                        else => "?",
                    };

                    try self.writeLineFormatted("const {s} = {s} {s} {s};", .{ var_name, left_val, op_str, right_val });
                    try self.node_values.put(node_id, var_name);
                }
            },

            .logical_not => {
                if (node.inputs.len >= 1) {
                    const operand_val = self.node_values.get(node.inputs[0]) orelse "undefined";
                    const var_name = try self.generateVariableName();

                    try self.writeLineFormatted("const {s} = !{s};", .{ var_name, operand_val });
                    try self.node_values.put(node_id, var_name);
                }
            },

            .call => {
                // Simple function call handling
                const var_name = try self.generateVariableName();
                if (node.inputs.len > 0) {
                    // First input is typically control, remaining are arguments
                    try self.writeLineFormatted("const {s} = /* function call */;", .{var_name});
                    try self.node_values.put(node_id, var_name);
                }
            },

            .return_ => {
                if (node.inputs.len >= 2) {
                    // Second input is typically the return value
                    const return_val = self.node_values.get(node.inputs[1]) orelse "undefined";
                    try self.writeLineFormatted("console.log('Result:', {s});", .{return_val});
                    try self.writeLineFormatted("return {s};", .{return_val});
                }
            },

            .alloc => {
                const var_name = try self.generateVariableName();
                try self.writeLineFormatted("let {s} = undefined; // allocated variable", .{var_name});
                try self.node_values.put(node_id, var_name);
            },

            .store => {
                if (node.inputs.len >= 2) {
                    const alloc_var = self.node_values.get(node.inputs[0]) orelse "undefined";
                    const value_var = self.node_values.get(node.inputs[1]) orelse "undefined";
                    try self.writeLineFormatted("{s} = {s};", .{ alloc_var, value_var });
                }
            },

            .load => {
                if (node.inputs.len >= 1) {
                    const alloc_var = self.node_values.get(node.inputs[0]) orelse "undefined";
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("const {s} = {s};", .{ var_name, alloc_var });
                    try self.node_values.put(node_id, var_name);
                }
            },

            else => {
                // For unsupported operations, generate a comment
                try self.writeLineFormatted("// Unsupported operation: {s}", .{node.op.toString()});
            },
        }
    }

    /// Generate a unique variable name
    fn generateVariableName(self: *JsIrCodegen) ![]const u8 {
        const var_name = try std.fmt.allocPrint(self.allocator, "v{d}", .{self.variable_counter});
        self.variable_counter += 1;
        return var_name;
    }

    /// Write a line with current indentation
    fn writeLine(self: *JsIrCodegen, text: []const u8) !void {
        try self.writeIndent();
        try self.output.appendSlice(text);
        try self.output.append('\n');
    }

    /// Write a formatted line with current indentation
    fn writeLineFormatted(self: *JsIrCodegen, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndent();
        try self.output.writer().print(fmt, args);
        try self.output.append('\n');
    }

    /// Write current indentation
    fn writeIndent(self: *JsIrCodegen) !void {
        var i: u32 = 0;
        while (i < self.indent_level * 4) : (i += 1) {
            try self.output.append(' ');
        }
    }

    /// Increase indentation level
    fn indent(self: *JsIrCodegen) void {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    fn dedent(self: *JsIrCodegen) void {
        if (self.indent_level > 0) {
            self.indent_level -= 1;
        }
    }
};

/// Main entry point for JavaScript code generation from IR
pub fn generateJavaScriptFromIr(
    allocator: std.mem.Allocator,
    ir: *const SeaOfNodes,
    semantic_analyzer: *const SemanticAnalyzer,
) ![]const u8 {
    var codegen = JsIrCodegen.init(allocator);
    defer codegen.deinit();

    return codegen.generate(ir, semantic_analyzer);
}
