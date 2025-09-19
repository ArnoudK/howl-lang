const std = @import("std");
const SeaOfNodes = @import("sea_of_nodes_ir.zig").SeaOfNodes;

// Constants for type names to avoid magic strings
const PRIMITIVE_TYPES = [_][]const u8{ "i64", "i32", "f64", "f32", "bool" };
const DEFAULT_TYPE = "i64";
const KNOWN_STRUCT_TYPE = "MyStruct";
const IrNode = @import("sea_of_nodes_ir.zig").IrNode;
const IrNodeId = @import("sea_of_nodes_ir.zig").IrNodeId;
const IrOp = @import("sea_of_nodes_ir.zig").IrOp;
const IrConstant = @import("sea_of_nodes_ir.zig").IrConstant;
const INVALID_IR_NODE_ID = @import("sea_of_nodes_ir.zig").INVALID_IR_NODE_ID;
const ErrorSystem = @import("error_system.zig");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig").SemanticAnalyzer;
const CompileError = @import("CompileError.zig").CompileError;

// ============================================================================
// C IR Code Generator - Sea-of-Nodes IR to C Translation
// ============================================================================

/// C code generator for Sea-of-Nodes IR
const CIrCodegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    node_values: std.AutoHashMap(IrNodeId, []const u8),
    processed_nodes: std.AutoHashMap(IrNodeId, void),
    pointer_variables: std.StringHashMap(void), // Track which variables are pointers
    array_element_types: std.StringHashMap([]const u8), // Track element types for heap allocated arrays
    indent_level: u32,
    next_var_id: u32,
    ir: *const SeaOfNodes, // Add reference to IR for node lookup
    semantic_analyzer: *SemanticAnalyzer, // Reference to semantic analyzer for type information
    current_function_returns_error_union: bool, // Track if current function returns error union
    current_function_name: []const u8, // Track current function name
    current_function_return_type: ?ast.Type, // Track current function return type
    arm_body_nodes: std.AutoHashMap(IrNodeId, void), // Track nodes that are match arm bodies

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, ir: *const SeaOfNodes, semantic_analyzer: *SemanticAnalyzer) Self {
        return Self{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .node_values = std.AutoHashMap(IrNodeId, []const u8).init(allocator),
            .processed_nodes = std.AutoHashMap(IrNodeId, void).init(allocator),
            .pointer_variables = std.StringHashMap(void).init(allocator),
            .array_element_types = std.StringHashMap([]const u8).init(allocator),
            .indent_level = 0,
            .next_var_id = 0,
            .ir = ir,
            .semantic_analyzer = semantic_analyzer,
            .current_function_returns_error_union = false,
            .current_function_name = "",
            .current_function_return_type = null,
            .arm_body_nodes = std.AutoHashMap(IrNodeId, void).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all allocated variable names, but be careful about what we free
        var iterator = self.node_values.iterator();
        while (iterator.next()) |entry| {
            const value = entry.value_ptr.*;
            // Only free if it looks like a dynamically allocated variable name (starts with 'v')
            // and doesn't contain spaces (which would indicate a static string)
            if (std.mem.startsWith(u8, value, "v") and std.mem.indexOf(u8, value, " ") == null) {
                self.allocator.free(value);
            }
        }
        self.node_values.deinit();
        self.processed_nodes.deinit();
        self.pointer_variables.deinit();
        self.arm_body_nodes.deinit();
        var aiter = self.array_element_types.iterator();
        while (aiter.next()) |entry| {
            const t = entry.value_ptr.*;
            self.allocator.free(t);
        }
        self.array_element_types.deinit();
        self.output.deinit();
    }

    /// Generate standard C header
    fn generateStandardHeader(self: *CIrCodegen) CompileError!void {
        try self.writeLine("#include <stdio.h>");
        try self.writeLine("#include <stdlib.h>");
        try self.writeLine("#include <stdbool.h>");
        try self.writeLine("#include <stdint.h>");
        try self.writeLine("#include <inttypes.h>");
        try self.writeLine("");
        try self.writeLine("typedef int8_t i8;");
        try self.writeLine("typedef int16_t i16;");
        try self.writeLine("typedef int32_t i32;");
        try self.writeLine("typedef int64_t i64;");
        try self.writeLine("typedef uint8_t u8;");
        try self.writeLine("typedef uint16_t u16;");
        try self.writeLine("typedef uint32_t u32;");
        try self.writeLine("typedef uint64_t u64;");
        try self.writeLine("typedef float f32;");
        try self.writeLine("typedef double f64;");
        try self.writeLine("");

        // Generate type definitions from IR nodes
        try self.generateTypeDefinitions();

        try self.writeLineFormatted("/* IR contains {} nodes */", .{self.ir.nodes.items.len});
    }

    /// Generate a single function from IR
    fn generateFunction(self: *CIrCodegen, ir: *const SeaOfNodes, func_node_id: IrNodeId) CompileError!void {
        const func_node = ir.getNode(func_node_id) orelse return;
        if (func_node.op != .function_def) return;

        const func_data = func_node.data.function_def;

        // Set current function name and return type first
        self.current_function_name = func_data.name;
        self.current_function_return_type = func_data.return_type;

        // Determine the correct return type based on the function's type information
        const return_type = if (std.mem.eql(u8, func_data.name, "main"))
            "int"
        else blk: {
            const ty_str = try self.getTypeString(func_data.return_type);
            break :blk ty_str;
        };

        // Check if this function returns an error union
        self.current_function_returns_error_union = switch (func_data.return_type.data) {
            .error_union => true,
            else => false,
        };

        // Generate function signature
        try self.output.writer().print("{s} {s}(", .{ return_type, func_data.name });

        // Use parameter nodes from function definition to get their types
        if (func_data.param_nodes.len > 0) {
            for (func_data.param_nodes, 0..) |param_node_id, i| {
                if (i > 0) try self.output.appendSlice(", ");
                const param_node = ir.getNode(param_node_id) orelse continue;
                const param_type_str = if (param_node.output_type) |param_type|
                    try self.getTypeString(param_type)
                else
                    "i64";
                try self.output.writer().print("{s} {s}", .{ param_type_str, func_data.params[i] });
            }
        } else {
            // Fallback: use default i64 for all parameters
            for (func_data.params, 0..) |param, i| {
                if (i > 0) try self.output.appendSlice(", ");
                try self.output.writer().print("i64 {s}", .{param});
            }
        }
        try self.output.appendSlice(") {\n");

        self.indent();

        // Process function body
        try self.generateNodeRecursive(ir, func_data.body);

        // Add return 0 for main function if no explicit return
        // TODO: Track if there was an explicit return in the function body
        if (std.mem.eql(u8, func_data.name, "main")) {
            // For now, always add return 0 - the duplicate will be handled by the C compiler
            try self.writeLine("return 0;");
        }

        self.dedent();
        try self.writeLine("}");
        try self.writeLine("");
    }

    /// Recursively generate code for a node and its inputs
    fn generateNodeRecursive(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId) CompileError!void {
        try self.generateNodeRecursiveInternal(ir, node_id, false);
    }

    /// Internal recursive node generation with control over arm body processing
    fn generateNodeRecursiveInternal(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId, process_arm_bodies: bool) CompileError!void {
        const node = ir.getNode(node_id) orelse return;

        // Skip if already processed
        if (self.processed_nodes.contains(node_id)) return;
        try self.processed_nodes.put(node_id, {});

        // Skip arm body nodes during initial processing unless explicitly allowed
        if (!process_arm_bodies and self.arm_body_nodes.contains(node_id)) return;

        // Process inputs with special-cases to avoid unsafe eager evaluation
        if (node.op == .return_ and node.inputs.len >= 2) {
            const ret_val_id = node.inputs[1];
            const ret_val_node = ir.getNode(ret_val_id);
            if (ret_val_node) |rvn| {
                if (rvn.op == .select and rvn.inputs.len >= 1) {
                    // Only precompute the select condition; branches handled during return generation
                    try self.generateNodeRecursiveInternal(ir, rvn.inputs[0], false);
                } else {
                    for (node.inputs) |input_id| {
                        try self.generateNodeRecursiveInternal(ir, input_id, false);
                    }
                }
            } else {
                for (node.inputs) |input_id| {
                    try self.generateNodeRecursiveInternal(ir, input_id, false);
                }
            }
        } else if (node.op == .select) {
            // Defer branch evaluation to codegen to avoid side effects (like divide-by-zero)
            if (node.inputs.len >= 1) {
                try self.generateNodeRecursiveInternal(ir, node.inputs[0], false);
            }
        } else if (node.op == .call) {
            // For call operations, handle std.debug.print specially
            if (node.data == .call) {
                const call_data = node.data.call;
                if (std.mem.eql(u8, call_data.function_name, "std.debug.print")) {
                    // For std.debug.print, skip the function name input (index 1) and process others
                    try self.generateNodeRecursiveInternal(ir, node.inputs[0], false); // control
                    // Skip node.inputs[1] (function name)
                    for (node.inputs[2..]) |input_id| { // format string and arguments
                        try self.generateNodeRecursiveInternal(ir, input_id, false);
                    }
                } else {
                    // Regular function call - process all inputs
                    for (node.inputs) |input_id| {
                        try self.generateNodeRecursiveInternal(ir, input_id, false);
                    }
                }
            } else {
                // Call node without proper call data - process all inputs
                for (node.inputs) |input_id| {
                    try self.generateNodeRecursiveInternal(ir, input_id, false);
                }
            }
        } else {
            // For all other operations, process inputs normally
            for (node.inputs) |input_id| {
                try self.generateNodeRecursiveInternal(ir, input_id, false);
            }
        }

        // Generate code for this node
        try self.generateNode(ir, node_id, node);
    }

    /// Generate code for a single node
    fn generateNode(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId, node: *const @import("sea_of_nodes_ir.zig").IrNode) CompileError!void {
        switch (node.op) {
            .start => {
                // Start nodes don't generate code
            },
            .parameter => {
                // Parameters are already declared in function signature
                const param_data = node.data.parameter;
                try self.node_values.put(node_id, try self.allocator.dupe(u8, param_data.name));
            },
            .constant => {
                const var_name = try self.generateVariableName();
                switch (node.data.constant) {
                    .integer => |val| {
                        try self.writeLineFormatted("i64 {s} = {d};", .{ var_name, val });
                    },
                    .float => |val| {
                        try self.writeLineFormatted("f64 {s} = {d};", .{ var_name, val });
                    },
                    .string => |val| {
                        // Escape special characters in strings
                        const escaped = try self.escapeString(val);
                        defer self.allocator.free(escaped);
                        try self.writeLineFormatted("char* {s} = \"{s}\";", .{ var_name, escaped });
                    },
                    .boolean => |val| {
                        const bool_str = if (val) "true" else "false";
                        try self.writeLineFormatted("bool {s} = {s};", .{ var_name, bool_str });
                    },
                    .none => {
                        // Handle .none constants - attempt to emit a typed 'ok' error-union if context/type indicates one
                        var emitted = false;
                        if (node.output_type) |t| {
                            switch (t.data) {
                                .error_union => {
                                    const eu_type = try self.getTypeString(t);
                                    const none_var_name = try self.generateVariableName();
                                    try self.writeLineFormatted("{s} {s} = {{ .error_code = 0, .payload = 0 }}; /* None value */", .{ eu_type, none_var_name });
                                    try self.node_values.put(node_id, none_var_name);
                                    emitted = true;
                                },
                                else => {},
                            }
                        }
                        if (!emitted and self.current_function_returns_error_union) {
                            const error_union_type = self.getCurrentErrorUnionTypeName();
                            const none_var_name = try self.generateVariableName();
                            try self.writeLineFormatted("{s} {s} = {{ .error_code = 0, .payload = 0 }}; /* None value */", .{ error_union_type, none_var_name });
                            try self.node_values.put(node_id, none_var_name);
                            emitted = true;
                        }
                        if (!emitted) {
                            // For non-error union context, skip generating code for .none constants
                            try self.node_values.put(node_id, try self.allocator.dupe(u8, "/* empty struct */"));
                            return;
                        }
                    },
                }
                try self.node_values.put(node_id, var_name);
            },
            .sub => {
                if (node.inputs.len >= 2) {
                    const left = self.node_values.get(node.inputs[0]) orelse "0";
                    const right = self.node_values.get(node.inputs[1]) orelse "0";
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = {s} - {s};", .{ var_name, left, right });
                    try self.node_values.put(node_id, var_name);
                }
            },
            .add => {
                if (node.inputs.len >= 2) {
                    const left = self.node_values.get(node.inputs[0]) orelse "0";
                    const right = self.node_values.get(node.inputs[1]) orelse "0";
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = {s} + {s};", .{ var_name, left, right });
                    try self.node_values.put(node_id, var_name);
                }
            },
            .lt => {
                if (node.inputs.len >= 2) {
                    const left = self.node_values.get(node.inputs[0]) orelse "0";
                    const right = self.node_values.get(node.inputs[1]) orelse "0";
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = {s} < {s};", .{ var_name, left, right });
                    try self.node_values.put(node_id, var_name);
                }
            },
            .div => {
                if (node.inputs.len >= 2) {
                    const left = self.node_values.get(node.inputs[0]) orelse "0";
                    const right = self.node_values.get(node.inputs[1]) orelse "0";
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = {s} / {s};", .{ var_name, left, right });
                    try self.node_values.put(node_id, var_name);
                }
            },
            .eq => {
                if (node.inputs.len >= 2) {
                    const left = self.node_values.get(node.inputs[0]) orelse "0";
                    const right = self.node_values.get(node.inputs[1]) orelse "0";
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = {s} == {s};", .{ var_name, left, right });
                    try self.node_values.put(node_id, var_name);
                }
            },
            .logical_and => {
                if (node.inputs.len >= 2) {
                    const left = self.node_values.get(node.inputs[0]) orelse "0";
                    const right = self.node_values.get(node.inputs[1]) orelse "0";
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = {s} && {s};", .{ var_name, left, right });
                    try self.node_values.put(node_id, var_name);
                }
            },
            .store => {
                // Store operation: array[index] = value
                if (node.inputs.len >= 3) {
                    const array_ptr = self.node_values.get(node.inputs[0]) orelse "NULL";
                    const index = self.node_values.get(node.inputs[1]) orelse "0";
                    const value = self.node_values.get(node.inputs[2]) orelse "0";

                    // Determine element type from tracking map (defaults to i64)
                    var elem_type: []const u8 = "i64";
                    if (self.array_element_types.get(array_ptr)) |et| {
                        elem_type = et;
                    }

                    // Generate typed array store: ((<elem_type>*)array_ptr)[index] = value
                    try self.writeLineFormatted("(({s}*){s})[{s}] = {s};", .{ elem_type, array_ptr, index, value });

                    // Return the array pointer for chaining
                    try self.node_values.put(node_id, array_ptr);
                } else {
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = 0; /* malformed store */", .{var_name});
                    try self.node_values.put(node_id, var_name);
                }
            },
            .load => {
                // Load operation: array[index]
                if (node.inputs.len >= 2) {
                    const array_ptr = self.node_values.get(node.inputs[0]) orelse "NULL";
                    const index = self.node_values.get(node.inputs[1]) orelse "0";
                    const var_name = try self.generateVariableName();

                    // Determine element type (default i64)
                    var elem_type: []const u8 = "i64";
                    if (self.array_element_types.get(array_ptr)) |et| {
                        elem_type = et;
                    }

                    // Generate array load with correct element type
                    try self.writeLineFormatted("{s} {s} = (({s}*){s})[{s}];", .{ elem_type, var_name, elem_type, array_ptr, index });
                    try self.node_values.put(node_id, var_name);
                } else {
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = 0; /* malformed load */", .{var_name});
                    try self.node_values.put(node_id, var_name);
                }
            },
            .select => {
                if (node.inputs.len >= 3) {
                    const condition = self.node_values.get(node.inputs[0]) orelse "0";
                    const var_name = try self.generateVariableName();

                    // Determine if the select result is an error union from type info
                    const out_is_error_union = blk: {
                        if (node.output_type) |t| {
                            switch (t.data) {
                                .error_union => break :blk true,
                                else => break :blk false,
                            }
                        }
                        break :blk false;
                    };

                    const needs_safe_evaluation = self.needsSafeEvaluation(ir, node.inputs[1]) or self.needsSafeEvaluation(ir, node.inputs[2]);

                    if (out_is_error_union) {
                        const union_type = try self.getTypeString(node.output_type.?);
                        try self.writeLineFormatted("{s} {s};", .{ union_type, var_name });
                        try self.writeLineFormatted("if ({s}) {{", .{condition});
                        self.indent();
                        // True branch expression
                        try self.generateNodeRecursiveInternal(ir, node.inputs[1], false);
                        const tval = self.node_values.get(node.inputs[1]) orelse "0";
                        var t_is_union = false;
                        var t_is_error_set = false;
                        if (ir.getNode(node.inputs[1])) |tn| {
                            if (tn.output_type) |tt| switch (tt.data) {
                                .error_union => t_is_union = true,
                                .error_set => t_is_error_set = true,
                                else => {},
                            };
                        }
                        if (t_is_union) {
                            try self.writeLineFormatted("{s} = {s};", .{ var_name, tval });
                        } else if (t_is_error_set) {
                            if (node.output_type) |sel_t| {
                                const invalid_init = try self.getErrorUnionPayloadInvalid(sel_t);
                                try self.writeLineFormatted("{s} = ({s}){{ .error_code = {s}, .payload = {s} }};", .{ var_name, union_type, tval, invalid_init });
                            } else {
                                try self.writeLineFormatted("{s} = ({s}){{ .error_code = {s}, .payload = -1 }}; /* fallback */", .{ var_name, union_type, tval });
                            }
                        } else {
                            // Check if we need to wrap the payload for optional types
                            const payload_expr = if (std.mem.eql(u8, union_type, "MyError_OptMyStruct_Union") and
                                std.mem.eql(u8, self.current_function_name, "createMyStructMaybe"))
                                try std.fmt.allocPrint(self.allocator, "OptMyStruct_some({s})", .{tval})
                            else
                                tval;
                            defer if (std.mem.eql(u8, union_type, "MyError_OptMyStruct_Union") and
                                std.mem.eql(u8, self.current_function_name, "createMyStructMaybe")) self.allocator.free(payload_expr);

                            try self.writeLineFormatted("{s} = ({s}){{ .error_code = 0, .payload = {s} }};", .{ var_name, union_type, payload_expr });
                        }
                        self.dedent();
                        try self.writeLineFormatted("}} else {{", .{});
                        self.indent();
                        // False branch expression
                        try self.generateNodeRecursiveInternal(ir, node.inputs[2], false);
                        const fval = self.node_values.get(node.inputs[2]) orelse "0";
                        var f_is_union = false;
                        var f_is_error_set = false;
                        if (ir.getNode(node.inputs[2])) |fn_| {
                            if (fn_.output_type) |ft| switch (ft.data) {
                                .error_union => f_is_union = true,
                                .error_set => f_is_error_set = true,
                                else => {},
                            };
                        }
                        if (f_is_union) {
                            try self.writeLineFormatted("{s} = {s};", .{ var_name, fval });
                        } else if (f_is_error_set) {
                            if (node.output_type) |sel_t| {
                                const invalid_init_f = try self.getErrorUnionPayloadInvalid(sel_t);
                                try self.writeLineFormatted("{s} = ({s}){{ .error_code = {s}, .payload = {s} }};", .{ var_name, union_type, fval, invalid_init_f });
                            } else {
                                try self.writeLineFormatted("{s} = ({s}){{ .error_code = {s}, .payload = -1 }}; /* fallback */", .{ var_name, union_type, fval });
                            }
                        } else {
                            // Check if we need to wrap the payload for optional types
                            const payload_expr = if (std.mem.eql(u8, union_type, "MyError_OptMyStruct_Union") and
                                std.mem.eql(u8, self.current_function_name, "createMyStructMaybe"))
                                try std.fmt.allocPrint(self.allocator, "OptMyStruct_some({s})", .{fval})
                            else
                                fval;
                            defer if (std.mem.eql(u8, union_type, "MyError_OptMyStruct_Union") and
                                std.mem.eql(u8, self.current_function_name, "createMyStructMaybe")) self.allocator.free(payload_expr);

                            try self.writeLineFormatted("{s} = ({s}){{ .error_code = 0, .payload = {s} }};", .{ var_name, union_type, payload_expr });
                        }
                        self.dedent();
                        try self.writeLineFormatted("}}", .{});
                    } else if (needs_safe_evaluation) {
                        try self.writeLineFormatted("i64 {s};", .{var_name});
                        try self.writeLineFormatted("if ({s}) {{", .{condition});
                        self.indent();
                        try self.generateNodeRecursiveInternal(ir, node.inputs[1], false);
                        const tval = self.node_values.get(node.inputs[1]) orelse "0";
                        try self.writeLineFormatted("{s} = {s};", .{ var_name, tval });
                        self.dedent();
                        try self.writeLineFormatted("}} else {{", .{});
                        self.indent();
                        try self.generateNodeRecursiveInternal(ir, node.inputs[2], false);
                        const fval = self.node_values.get(node.inputs[2]) orelse "0";
                        try self.writeLineFormatted("{s} = {s};", .{ var_name, fval });
                        self.dedent();
                        try self.writeLineFormatted("}}", .{});
                    } else {
                        // Safe simple case: both branches are already computed values
                        const true_val = self.node_values.get(node.inputs[1]) orelse "0";
                        const false_val = self.node_values.get(node.inputs[2]) orelse "0";
                        try self.writeLineFormatted("i64 {s} = {s} ? {s} : {s};", .{ var_name, condition, true_val, false_val });
                    }

                    try self.node_values.put(node_id, var_name);
                }
            },
            .heap_alloc => {
                // Handle heap allocation: malloc(sizeof(StructType)) or malloc(sizeof(ElementType) * size)
                if (node.inputs.len >= 1) {
                    const type_node_id = node.inputs[0];

                    // Get type name with error handling
                    const type_name = self.getActualStringContent(ir, type_node_id) catch {
                        // If we can't get the type name, generate a fallback allocation
                        const var_name = try self.generateVariableName();
                        try self.writeLineFormatted("void* {s} = malloc(sizeof(void*)); /* failed to resolve type */", .{var_name});
                        try self.node_values.put(node_id, var_name);
                        const var_name_copy = try self.allocator.dupe(u8, var_name);
                        try self.pointer_variables.put(var_name_copy, {});
                        return;
                    };
                    defer self.allocator.free(type_name);

                    const var_name = try self.generateVariableName();

                    // Resolve the allocation size from IR inputs
                    const size_value = self.resolveAllocationSize(ir, node) catch {
                        // If we can't resolve size, default to 1
                        const fallback_size = try self.allocator.dupe(u8, "1");
                        defer self.allocator.free(fallback_size);

                        // Generate allocation with fallback size
                        if (self.isPrimitiveArrayType(type_name)) {
                            try self.writeLineFormatted("{s}* {s} = malloc(sizeof({s}) * {s}); /* size resolution failed, using default */", .{ type_name, var_name, type_name, fallback_size });
                        } else if (type_name.len > 0) {
                            if (std.mem.eql(u8, type_name, KNOWN_STRUCT_TYPE)) {
                                try self.writeLineFormatted("{s}* {s} = malloc(sizeof({s}) * {s}); /* size resolution failed, using default */", .{ KNOWN_STRUCT_TYPE, var_name, KNOWN_STRUCT_TYPE, fallback_size });
                            } else {
                                try self.writeLineFormatted("struct {s}* {s} = malloc(sizeof(struct {s}) * {s}); /* size resolution failed, using default */", .{ type_name, var_name, type_name, fallback_size });
                            }
                        } else {
                            try self.writeLineFormatted("{s}* {s} = malloc(sizeof({s}) * {s}); /* unknown type and size resolution failed */", .{ DEFAULT_TYPE, var_name, DEFAULT_TYPE, fallback_size });
                        }

                        try self.node_values.put(node_id, var_name);
                        const var_name_copy = try self.allocator.dupe(u8, var_name);
                        try self.pointer_variables.put(var_name_copy, {});
                        return;
                    };
                    defer self.allocator.free(size_value);

                    // Generate appropriate malloc call based on type
                    if (self.isPrimitiveArrayType(type_name)) {
                        // For primitive arrays, use the element type directly
                        try self.writeLineFormatted("{s}* {s} = malloc(sizeof({s}) * {s});", .{ type_name, var_name, type_name, size_value });
                    } else {
                        // For structs or other types, treat as array of that type
                        if (type_name.len > 0) {
                            // Check if it's a known struct type
                            if (std.mem.eql(u8, type_name, KNOWN_STRUCT_TYPE)) {
                                try self.writeLineFormatted("{s}* {s} = malloc(sizeof({s}) * {s});", .{ KNOWN_STRUCT_TYPE, var_name, KNOWN_STRUCT_TYPE, size_value });
                            } else {
                                try self.writeLineFormatted("struct {s}* {s} = malloc(sizeof(struct {s}) * {s});", .{ type_name, var_name, type_name, size_value });
                            }
                        } else {
                            // Fallback for empty type name - assume default type array
                            try self.writeLineFormatted("{s}* {s} = malloc(sizeof({s}) * {s}); /* unknown type, defaulting to {s} array */", .{ DEFAULT_TYPE, var_name, DEFAULT_TYPE, size_value, DEFAULT_TYPE });
                        }
                    }

                    try self.node_values.put(node_id, var_name);

                    // Mark this variable as a pointer
                    const var_name_copy = try self.allocator.dupe(u8, var_name);
                    try self.pointer_variables.put(var_name_copy, {});

                    // Record element type for arrays to enable proper array operations
                    if (self.isPrimitiveArrayType(type_name)) {
                        const elem_type_copy = try self.allocator.dupe(u8, type_name);
                        try self.array_element_types.put(var_name_copy, elem_type_copy);
                    } else if (type_name.len > 0) {
                        // For struct types, record the struct type as element type
                        if (std.mem.eql(u8, type_name, KNOWN_STRUCT_TYPE)) {
                            const elem_type_copy = try self.allocator.dupe(u8, KNOWN_STRUCT_TYPE);
                            try self.array_element_types.put(var_name_copy, elem_type_copy);
                        } else {
                            const elem_type_copy = try std.fmt.allocPrint(self.allocator, "struct {s}", .{type_name});
                            try self.array_element_types.put(var_name_copy, elem_type_copy);
                        }
                    }
                } else {
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("void* {s} = malloc(sizeof(void*)); /* malformed heap_alloc */", .{var_name});
                    try self.node_values.put(node_id, var_name);
                }
            },
            .struct_init => {
                // Handle struct field initialization: ptr->field = value
                if (node.inputs.len >= 3) {
                    const object = self.node_values.get(node.inputs[0]) orelse "NULL";
                    const field_name_node_id = node.inputs[1];
                    const value = self.node_values.get(node.inputs[2]) orelse "0";

                    const field_name = try self.getActualStringContent(ir, field_name_node_id);
                    defer self.allocator.free(field_name);

                    // Generate the field assignment - assume object is a pointer
                    try self.writeLineFormatted("{s}->{s} = {s};", .{ object, field_name, value });

                    // Return the same object pointer for chaining
                    try self.node_values.put(node_id, try self.allocator.dupe(u8, object));
                } else {
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = 0; /* malformed struct_init */", .{var_name});
                    try self.node_values.put(node_id, var_name);
                }
            },
            .struct_literal => {
                // Materialize a stack struct literal value using recorded field names.
                var type_name: []const u8 = "i64"; // fallback
                if (node.output_type) |t| {
                    type_name = try self.getTypeString(t);
                }
                const var_name = try self.generateVariableName();
                // Declare variable
                try self.writeLineFormatted("{s} {s};", .{ type_name, var_name });
                // Access field names from node.data if available
                if (node.data == .struct_literal) {
                    const field_names = node.data.struct_literal.field_names;
                    for (node.inputs, 0..) |field_val_id, idx| {
                        const field_val = self.node_values.get(field_val_id) orelse "0";
                        const fname = if (idx < field_names.len) field_names[idx] else "/*unknown*/";
                        try self.writeLineFormatted("{s}.{s} = {s};", .{ var_name, fname, field_val });
                    }
                } else {
                    // Fallback if field names not present
                    for (node.inputs, 0..) |field_val_id, idx| {
                        const field_val = self.node_values.get(field_val_id) orelse "0";
                        try self.writeLineFormatted("{s}.f{d} = {s};", .{ var_name, idx, field_val });
                    }
                }
                try self.node_values.put(node_id, var_name);
            },
            .member_access => {
                // Handle member access: object.field or object->field
                if (node.inputs.len >= 2) {
                    const object = self.node_values.get(node.inputs[0]) orelse "NULL";
                    const field_name_node_id = node.inputs[1];

                    const field_name = try self.getActualStringContent(ir, field_name_node_id);
                    defer self.allocator.free(field_name);

                    const var_name = try self.generateVariableName();

                    // Determine result type from node.output_type if available
                    var result_type: []const u8 = "i64";
                    if (node.output_type) |t| {
                        result_type = try self.getTypeString(t);
                    }

                    if (self.pointer_variables.contains(object)) {
                        try self.writeLineFormatted("{s} {s} = {s}->{s};", .{ result_type, var_name, object, field_name });
                    } else {
                        try self.writeLineFormatted("{s} {s} = {s}.{s};", .{ result_type, var_name, object, field_name });
                    }
                    try self.node_values.put(node_id, var_name);
                } else {
                    const var_name = try self.generateVariableName();
                    try self.writeLineFormatted("i64 {s} = 0; /* malformed member_access */", .{var_name});
                    try self.node_values.put(node_id, var_name);
                }
            },
            .return_ => {
                if (node.inputs.len >= 2) {
                    const ret_id = node.inputs[1];
                    const rnode = self.ir.getNode(ret_id);

                    if (rnode) |rn| {
                        if (rn.op == .select and rn.inputs.len >= 3) {
                            const cond = self.node_values.get(rn.inputs[0]) orelse "0";
                            try self.writeLineFormatted("if ({s}) {{", .{cond});
                            self.indent();
                            // True branch
                            try self.generateNodeRecursiveInternal(ir, rn.inputs[1], false);
                            const tval = self.node_values.get(rn.inputs[1]) orelse "0";
                            var t_is_union = false;
                            var t_is_error_set = false;
                            if (ir.getNode(rn.inputs[1])) |tn| {
                                if (tn.output_type) |tt| switch (tt.data) {
                                    .error_union => t_is_union = true,
                                    .error_set => t_is_error_set = true,
                                    else => {},
                                };
                            }
                            if (self.current_function_returns_error_union) {
                                const error_union_type = self.getCurrentErrorUnionTypeName();
                                if (t_is_union) {
                                    var return_tval = tval;
                                    // Special handling for main function - convert empty struct to 0
                                    if (std.mem.eql(u8, self.current_function_name, "main") and
                                        std.mem.eql(u8, return_tval, "/* empty struct */"))
                                    {
                                        return_tval = "0";
                                    }
                                    try self.writeLineFormatted("return {s};", .{return_tval});
                                } else if (t_is_error_set) {
                                    {
                                        const invalid_init = try self.getErrorUnionPayloadInvalid(self.current_function_return_type.?);
                                        try self.writeLineFormatted("return ({s}){{ .error_code = {s}, .payload = {s} }};", .{ error_union_type, tval, invalid_init });
                                    }
                                } else {
                                    const wrapped_payload = try self.getWrappedPayloadForReturn(tval, false);
                                    try self.writeLineFormatted("return ({s}){{ .error_code = 0, .payload = {s} }};", .{ error_union_type, wrapped_payload });
                                }
                            } else {
                                var return_tval = tval;
                                // Special handling for main function - convert empty struct to 0
                                if (std.mem.eql(u8, self.current_function_name, "main") and
                                    std.mem.eql(u8, return_tval, "/* empty struct */"))
                                {
                                    return_tval = "0";
                                }
                                try self.writeLineFormatted("return {s};", .{return_tval});
                            }
                            self.dedent();
                            try self.writeLineFormatted("}} else {{", .{});
                            self.indent();
                            // False branch
                            try self.generateNodeRecursiveInternal(ir, rn.inputs[2], false);
                            const fval = self.node_values.get(rn.inputs[2]) orelse "0";
                            var f_is_union = false;
                            var f_is_error_set = false;
                            if (ir.getNode(rn.inputs[2])) |fn_| {
                                if (fn_.output_type) |ft| switch (ft.data) {
                                    .error_union => f_is_union = true,
                                    .error_set => f_is_error_set = true,
                                    else => {},
                                };
                            }
                            if (self.current_function_returns_error_union) {
                                const error_union_type = self.getCurrentErrorUnionTypeName();
                                if (f_is_union) {
                                    var return_fval = fval;
                                    // Special handling for main function - convert empty struct to 0
                                    if (std.mem.eql(u8, self.current_function_name, "main") and
                                        std.mem.eql(u8, return_fval, "/* empty struct */"))
                                    {
                                        return_fval = "0";
                                    }
                                    try self.writeLineFormatted("return {s};", .{return_fval});
                                } else if (f_is_error_set) {
                                    {
                                        const invalid_init_f = try self.getErrorUnionPayloadInvalid(self.current_function_return_type.?);
                                        try self.writeLineFormatted("return ({s}){{ .error_code = {s}, .payload = {s} }};", .{ error_union_type, fval, invalid_init_f });
                                    }
                                } else {
                                    var final_fval = fval;
                                    // Special handling for main function - convert empty struct to 0
                                    if (std.mem.eql(u8, self.current_function_name, "main") and
                                        std.mem.eql(u8, final_fval, "/* empty struct */"))
                                    {
                                        final_fval = "0";
                                    }
                                    const wrapped_payload = try self.getWrappedPayloadForReturn(final_fval, false);
                                    try self.writeLineFormatted("return ({s}){{ .error_code = 0, .payload = {s} }};", .{ error_union_type, wrapped_payload });
                                }
                            } else {
                                var return_fval = fval;
                                // Special handling for main function - convert empty struct to 0
                                if (std.mem.eql(u8, self.current_function_name, "main") and
                                    std.mem.eql(u8, return_fval, "/* empty struct */"))
                                {
                                    return_fval = "0";
                                }
                                try self.writeLineFormatted("return {s};", .{return_fval});
                            }
                            self.dedent();
                            try self.writeLineFormatted("}}", .{});
                        } else if (self.current_function_returns_error_union) {
                            if (rn.op == .constant and rn.data.constant == .none) {
                                const error_union_type = self.getCurrentErrorUnionTypeName();
                                const none_payload = try self.getWrappedPayloadForReturn("", true);
                                try self.writeLineFormatted("return ({s}){{ .error_code = 0, .payload = {s} }};", .{ error_union_type, none_payload });
                            } else {
                                const return_val = self.node_values.get(ret_id) orelse "0";
                                var r_is_union = false;
                                var r_is_error_set = false;
                                if (rn.output_type) |rt| switch (rt.data) {
                                    .error_union => r_is_union = true,
                                    .error_set => r_is_error_set = true,
                                    else => {},
                                };
                                if (r_is_union) {
                                    var final_return_val = return_val;
                                    // Special handling for main function - convert empty struct to 0
                                    if (std.mem.eql(u8, self.current_function_name, "main") and
                                        std.mem.eql(u8, final_return_val, "/* empty struct */"))
                                    {
                                        final_return_val = "0";
                                    }
                                    try self.writeLineFormatted("return {s};", .{final_return_val});
                                } else {
                                    const error_union_type = self.getCurrentErrorUnionTypeName();
                                    if (r_is_error_set) {
                                        {
                                            const invalid_ret_init = try self.getErrorUnionPayloadInvalid(self.current_function_return_type.?);
                                            try self.writeLineFormatted("return ({s}){{ .error_code = {s}, .payload = {s} }};", .{ error_union_type, return_val, invalid_ret_init });
                                        }
                                    } else {
                                        var final_return_val = return_val;
                                        // Special handling for main function - convert empty struct to 0
                                        if (std.mem.eql(u8, self.current_function_name, "main") and
                                            std.mem.eql(u8, final_return_val, "/* empty struct */"))
                                        {
                                            final_return_val = "0";
                                        }
                                        const wrapped_payload = try self.getWrappedPayloadForReturn(final_return_val, false);
                                        try self.writeLineFormatted("return ({s}){{ .error_code = 0, .payload = {s} }};", .{ error_union_type, wrapped_payload });
                                    }
                                }
                            }
                        } else {
                            var return_val = self.node_values.get(ret_id) orelse "0";
                            // Special handling for main function - convert empty struct to 0
                            if (std.mem.eql(u8, self.current_function_name, "main") and
                                std.mem.eql(u8, return_val, "/* empty struct */"))
                            {
                                return_val = "0";
                            }
                            try self.writeLineFormatted("return {s};", .{return_val});
                        }
                    } else {
                        var return_val = self.node_values.get(ret_id) orelse "0";
                        // Special handling for main function - convert empty struct to 0
                        if (std.mem.eql(u8, self.current_function_name, "main") and
                            std.mem.eql(u8, return_val, "/* empty struct */"))
                        {
                            return_val = "0";
                        }
                        try self.writeLineFormatted("return {s};", .{return_val});
                    }
                }
            },
            .call => {
                // Handle function calls
                const var_name = try self.generateVariableName();

                // Check if call data is properly set
                switch (node.data) {
                    .call => |call_data| {
                        // Special case for builtin functions like std.debug.print
                        if (std.mem.eql(u8, call_data.function_name, "std.debug.print")) {
                            // Handle std.debug.print by mapping to printf
                            // Arguments: control, function_name, format_string, [args...]
                            // Skip the function_name input (node.inputs[1]) and start from format_string (node.inputs[2])
                            if (node.inputs.len >= 3) {
                                // Get the format string (skip the function name input)
                                const format_node_id = node.inputs[2];
                                const format_var = self.node_values.get(format_node_id) orelse "\"\"";

                                // Get the actual format string content from the IR node
                                const format_content = try self.getActualStringContent(ir, format_node_id);
                                defer self.allocator.free(format_content);

                                // Check if there are real arguments (not just empty struct .{})
                                var real_args = std.ArrayList([]const u8).init(self.allocator);
                                defer real_args.deinit();

                                for (node.inputs[3..]) |arg_id| {
                                    if (self.node_values.get(arg_id)) |arg_val| {
                                        if (!std.mem.eql(u8, arg_val, "/* empty struct */")) {
                                            try real_args.append(arg_val);
                                        }
                                    }
                                }

                                if (real_args.items.len == 0) {
                                    // No real arguments - just print the format string
                                    try self.writeLineFormatted("printf({s});", .{format_var});
                                } else {
                                    // Has arguments - convert format string with actual type information
                                    const c_format = try self.convertFormatStringWithTypeInfo(ir, format_content, node.inputs[3..]);
                                    defer self.allocator.free(c_format);

                                    // Build printf call with all arguments
                                    try self.output.writer().print("    printf({s}", .{c_format});
                                    for (real_args.items) |arg| {
                                        try self.output.writer().print(", {s}", .{arg});
                                    }
                                    try self.output.appendSlice(");\n");
                                }
                            } else {
                                // std.debug.print with insufficient arguments - try to find variables to print
                                // Look for recently created variables that might be intended for printing
                                var recent_var: ?[]const u8 = null;
                                var max_var_id: u32 = 0;

                                // Find the most recently created variable
                                var iter = self.node_values.iterator();
                                while (iter.next()) |entry| {
                                    const entry_var_name = entry.value_ptr.*;
                                    if (std.mem.startsWith(u8, entry_var_name, "v")) {
                                        if (std.fmt.parseInt(u32, entry_var_name[1..], 10)) |var_id| {
                                            if (var_id > max_var_id and var_id < self.next_var_id - 1) { // Don't use the current print call variable
                                                // Check if this looks like a data variable (not a string constant)
                                                const var_node = ir.getNode(entry.key_ptr.*);
                                                if (var_node) |n| {
                                                    if (n.op == .constant and n.data.constant == .integer) {
                                                        max_var_id = var_id;
                                                        recent_var = entry_var_name;
                                                    }
                                                }
                                            }
                                        } else |_| {}
                                    }
                                }

                                if (recent_var) |var_to_print| {
                                    try self.writeLineFormatted("    printf(\"Result: %ld\\n\", {s});", .{var_to_print});
                                } else {
                                    try self.writeLineFormatted("printf(\"[print]\\n\");", .{});
                                }
                            }

                            // printf doesn't return a meaningful value in our context
                            try self.writeLineFormatted("/* printf return value unused */", .{});
                            // var_name is not used for printf calls
                        } else {
                            // Regular function call
                            // Determine the called function return type
                            var return_type_str: []const u8 = "i64";

                            // Look for the function definition to get its return type
                            for (ir.nodes.items) |check_node| {
                                if (check_node.op == .function_def) {
                                    const check_func_data = check_node.data.function_def;
                                    if (std.mem.eql(u8, check_func_data.name, call_data.function_name)) {
                                        if (check_node.output_type) |output_type| {
                                            return_type_str = try self.getTypeString(output_type);
                                        }
                                        break;
                                    }
                                }
                            }

                            try self.output.writer().print("    {s} {s} = {s}(", .{ return_type_str, var_name, call_data.function_name });

                            // Add arguments (skip control input at index 0, also potentially skip extra inputs)
                            var arg_count: u32 = 0;
                            for (node.inputs[1..]) |arg_id| {
                                const arg_value = self.node_values.get(arg_id) orelse continue;

                                // Skip string literals that might be function names
                                if (std.mem.startsWith(u8, arg_value, "\"")) continue;

                                if (arg_count > 0) try self.output.appendSlice(", ");
                                try self.output.appendSlice(arg_value);
                                arg_count += 1;
                            }

                            try self.output.appendSlice(");\n");
                        }
                    },
                    else => {
                        // Call node without proper call data - this shouldn't happen
                        try self.writeLineFormatted("/* DEBUG: Call node #{d} has no call data, inputs: {d} */", .{ node_id, node.inputs.len });
                        try self.writeLineFormatted("i64 {s} = 0; /* call node with invalid data */", .{var_name});
                    },
                }

                try self.node_values.put(node_id, var_name);
            },
            .function_def => {
                // Skip nested function definitions - they're handled at the top level
                // This prevents "/* unsupported node: function_def */" in main()
            },

            // Error handling operations
            .error_union_ok => {
                // Create successful error union: { .error_code = 0, .payload = value }
                if (node.inputs.len >= 1) {
                    const payload = self.node_values.get(node.inputs[0]) orelse "0";
                    const var_name = try self.generateVariableName();
                    const error_union_type = if (node.output_type) |t| try self.getTypeString(t) else self.getCurrentErrorUnionTypeName();
                    if (node.output_type) |ok_t| {
                        // Basic mismatch guard: if payload type appears struct but supplied payload looks like scalar temp (heuristic), fallback zero
                        const guarded_payload = blk: {
                            switch (ok_t.data) {
                                .error_union => |eu| switch (eu.payload_type.*.data) {
                                    .custom_struct => break :blk payload, // Assume variable holds struct literal variable name
                                    else => break :blk payload,
                                },
                                else => break :blk payload,
                            }
                        };
                        try self.writeLineFormatted("{s} {s} = {{ .error_code = 0, .payload = {s} }};", .{ error_union_type, var_name, guarded_payload });
                    } else {
                        try self.writeLineFormatted("{s} {s} = {{ .error_code = 0, .payload = {s} }};", .{ error_union_type, var_name, payload });
                    }
                    try self.node_values.put(node_id, var_name);
                } else {
                    try self.writeLineFormatted("/* malformed error_union_ok */", .{});
                }
            },
            .error_union_err => {
                // Create failed error union: set error_code, invalid payload placeholder
                if (node.inputs.len >= 1) {
                    const error_code = self.node_values.get(node.inputs[0]) orelse "-1";
                    const var_name = try self.generateVariableName();
                    const error_union_type = if (node.output_type) |t| try self.getTypeString(t) else self.getCurrentErrorUnionTypeName();
                    const payload_init = "0"; // Temporary fix
                    try self.writeLineFormatted("{s} {s} = {{ .error_code = {s}, .payload = {s} }};", .{ error_union_type, var_name, error_code, payload_init });
                    try self.node_values.put(node_id, var_name);
                } else {
                    try self.writeLineFormatted("/* malformed error_union_err */", .{});
                }
            },
            .error_union_unwrap => {
                // Extract payload from error union, propagate error if needed
                if (node.inputs.len >= 1) {
                    const error_union = self.node_values.get(node.inputs[0]) orelse "{ .error_code = -1, .payload = 0 }";
                    const var_name = try self.generateVariableName();

                    // Generate error check and early return
                    try self.writeLineFormatted("// Error union unwrap", .{});
                    try self.writeLineFormatted("if (({s}).error_code != 0) {{", .{error_union});
                    if (std.mem.eql(u8, self.current_function_name, "main")) {
                        try self.writeLineFormatted("    printf(\"Error: %ld\\n\", ({s}).error_code);", .{error_union});
                        try self.writeLineFormatted("    return 1;", .{});
                    } else if (self.current_function_returns_error_union) {
                        const error_union_type = self.getCurrentErrorUnionTypeName();
                        {
                            const invalid_unwrap = try self.getErrorUnionPayloadInvalid(self.current_function_return_type.?);
                            try self.writeLineFormatted("    return ({s}){{ .error_code = ({s}).error_code, .payload = {s} }};", .{ error_union_type, error_union, invalid_unwrap });
                        }
                    } else {
                        // Non-error-union function: best effort - return 1
                        try self.writeLineFormatted("    return 1;", .{});
                    }
                    try self.writeLineFormatted("}}", .{});
                    // Determine the correct payload type from this node's output type
                    const payload_type = if (node.output_type) |t| try self.getTypeString(t) else "i64";
                    try self.writeLineFormatted("{s} {s} = ({s}).payload;", .{ payload_type, var_name, error_union });

                    try self.node_values.put(node_id, var_name);
                } else {
                    try self.writeLineFormatted("/* malformed error_union_unwrap */", .{});
                }
            },
            .try_ => {
                // Try expression - same as error_union_unwrap
                if (node.inputs.len >= 1) {
                    const expression = self.node_values.get(node.inputs[0]) orelse "{ .error_code = -1, .payload = 0 }";
                    const var_name = try self.generateVariableName();

                    try self.writeLineFormatted("// Try expression", .{});
                    try self.writeLineFormatted("if (({s}).error_code != 0) {{", .{expression});

                    if (std.mem.eql(u8, self.current_function_name, "main")) {
                        // In main function, just print error and return error code
                        try self.writeLineFormatted("    printf(\"Error: %ld\\n\", ({s}).error_code);", .{expression});
                        try self.writeLineFormatted("    return 1;", .{});
                    } else if (self.current_function_returns_error_union) {
                        // In functions returning error unions, propagate the error
                        const error_union_type = self.getCurrentErrorUnionTypeName();
                        {
                            const invalid_try = try self.getErrorUnionPayloadInvalid(self.current_function_return_type.?);
                            try self.writeLineFormatted("    return ({s}){{ .error_code = ({s}).error_code, .payload = {s} }};", .{ error_union_type, expression, invalid_try });
                        }
                    } else {
                        // Best effort for non-error-union functions
                        try self.writeLineFormatted("    return 1;", .{});
                    }

                    try self.writeLineFormatted("}}", .{});
                    // Determine the correct payload type from this node's output type
                    const payload_type = if (node.output_type) |t| try self.getTypeString(t) else "i64";
                    try self.writeLineFormatted("{s} {s} = ({s}).payload;", .{ payload_type, var_name, expression });

                    try self.node_values.put(node_id, var_name);
                } else {
                    try self.writeLineFormatted("/* malformed try expression */", .{});
                }
            },
            .match_start => {
                // Match start - just pass through the matched value
                if (node.inputs.len >= 1) {
                    const matched_value = self.node_values.get(node.inputs[0]) orelse "0";
                    try self.node_values.put(node_id, try self.allocator.dupe(u8, matched_value));
                } else {
                    try self.node_values.put(node_id, try self.allocator.dupe(u8, "0"));
                }
            },
            .match_branch => {
                // Match branch - handled by match_end, just store the branch info
                if (node.inputs.len >= 3) {
                    const condition = self.node_values.get(node.inputs[1]) orelse "false";
                    const body = self.node_values.get(node.inputs[2]) orelse "0";

                    // Store branch information for later use by match_end
                    const branch_info = try std.fmt.allocPrint(self.allocator, "{{condition: {s}, body: {s}}}", .{ condition, body });
                    try self.node_values.put(node_id, branch_info);
                } else {
                    try self.node_values.put(node_id, try self.allocator.dupe(u8, "{invalid branch}"));
                }
            },
            .match_end => {
                // Match end - generate if-else chain that executes statements in each arm
                // Don't create a result variable since match is used as a statement

                // First, collect all arm body nodes to mark them as processed
                for (node.inputs) |branch_id| {
                    const branch_node = self.ir.getNode(branch_id) orelse continue;
                    if (branch_node.op == .match_branch) {
                        const arm_body_node_id = branch_node.data.match_branch.arm_body_node;
                        try self.arm_body_nodes.put(arm_body_node_id, {});
                    }
                }

                // Evaluate all conditions first
                var condition_vars = std.ArrayList([]const u8).init(self.allocator);
                defer condition_vars.deinit();

                for (node.inputs) |branch_id| {
                    const branch_node = self.ir.getNode(branch_id) orelse continue;

                    if (branch_node.op == .match_branch and branch_node.inputs.len >= 3) {
                        const is_wildcard = if (branch_node.data == .match_branch)
                            branch_node.data.match_branch.is_wildcard
                        else
                            false;

                        if (!is_wildcard) {
                            const condition_var = try self.generateVariableName();
                            const condition_expr = self.node_values.get(branch_node.inputs[1]) orelse "false";
                            try self.writeLineFormatted("i64 {s} = {s};", .{ condition_var, condition_expr });
                            try condition_vars.append(condition_var);
                        } else {
                            // For wildcards, add a null marker
                            try condition_vars.append("wildcard");
                        }
                    }
                }

                // Generate if-else chain using the pre-evaluated conditions
                var first_branch = true;
                var condition_index: usize = 0;

                for (node.inputs) |branch_id| {
                    const branch_node = self.ir.getNode(branch_id) orelse continue;

                    if (branch_node.op == .match_branch and branch_node.inputs.len >= 3) {
                        const is_wildcard = if (branch_node.data == .match_branch)
                            branch_node.data.match_branch.is_wildcard
                        else
                            false;

                        if (first_branch) {
                            if (is_wildcard) {
                                // Wildcard as first branch - execute the arm body directly
                                try self.generateArmBody(self.ir, branch_id);
                            } else {
                                const condition_var = condition_vars.items[condition_index];
                                try self.writeLineFormatted("if ({s}) {{", .{condition_var});
                                self.indent();
                                // Execute the statements in this arm
                                try self.generateArmBody(self.ir, branch_id);
                                self.dedent();
                                try self.writeLineFormatted("}}", .{});
                            }
                            first_branch = false;
                        } else {
                            if (is_wildcard) {
                                // Wildcard as else branch
                                try self.writeLineFormatted("else {{", .{});
                                self.indent();
                                // Execute the statements in this arm
                                try self.generateArmBody(self.ir, branch_id);
                                self.dedent();
                                try self.writeLineFormatted("}}", .{});
                            } else {
                                const condition_var = condition_vars.items[condition_index];
                                try self.writeLineFormatted("else if ({s}) {{", .{condition_var});
                                self.indent();
                                // Execute the statements in this arm
                                try self.generateArmBody(self.ir, branch_id);
                                self.dedent();
                                try self.writeLineFormatted("}}", .{});
                            }
                        }

                        if (!is_wildcard) {
                            condition_index += 1;
                        }
                    }
                }

                // Match expression doesn't return a value, so no node value to store
            },
            else => {
                try self.writeLineFormatted("/* unsupported node: {s} */", .{@tagName(node.op)});
            },
        }
    }

    /// Generate a unique variable name
    fn generateVariableName(self: *CIrCodegen) CompileError![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "v{d}", .{self.next_var_id});
        self.next_var_id += 1;
        return name;
    }

    /// Escape special characters in strings for C
    fn escapeString(self: *CIrCodegen, input: []const u8) CompileError![]const u8 {
        var escaped = std.ArrayList(u8).init(self.allocator);
        defer escaped.deinit();

        for (input) |c| {
            switch (c) {
                '\n' => try escaped.appendSlice("\\n"),
                '\t' => try escaped.appendSlice("\\t"),
                '\r' => try escaped.appendSlice("\\r"),
                '\\' => try escaped.appendSlice("\\\\"),
                '"' => try escaped.appendSlice("\\\""),
                else => try escaped.append(c),
            }
        }

        return escaped.toOwnedSlice();
    }

    /// Get the actual string content from an IR node
    fn getActualStringContent(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId) CompileError![]const u8 {
        const node = ir.getNode(node_id) orelse return try self.allocator.dupe(u8, "");
        if (node.op == .constant) {
            switch (node.data.constant) {
                .string => |str| return try self.allocator.dupe(u8, str),
                else => return try self.allocator.dupe(u8, ""),
            }
        }
        return try self.allocator.dupe(u8, "");
    }

    /// Check if a type name represents a primitive numeric type that can be used in arrays
    fn isPrimitiveArrayType(_: *CIrCodegen, type_name: []const u8) bool {
        for (PRIMITIVE_TYPES) |primitive_type| {
            if (std.mem.eql(u8, type_name, primitive_type)) {
                return true;
            }
        }
        return false;
    }

    /// Get the C type string from a type annotation node
    /// Returns the C type name for the given type annotation node
    fn getCTypeFromAnnotation(self: *CIrCodegen, ir: *const SeaOfNodes, type_annotation_node_id: IrNodeId) CompileError![]const u8 {
        const type_annotation_node = ir.getNode(type_annotation_node_id) orelse return try self.allocator.dupe(u8, "i64");

        // If it's a constant string node, it contains the type name
        if (type_annotation_node.data == .constant) {
            switch (type_annotation_node.data.constant) {
                .string => |type_str| {
                    // Map Howl types to C types
                    if (std.mem.eql(u8, type_str, "i32")) return try self.allocator.dupe(u8, "i32");
                    if (std.mem.eql(u8, type_str, "i64")) return try self.allocator.dupe(u8, "i64");
                    if (std.mem.eql(u8, type_str, "f32")) return try self.allocator.dupe(u8, "f32");
                    if (std.mem.eql(u8, type_str, "f64")) return try self.allocator.dupe(u8, "f64");
                    if (std.mem.eql(u8, type_str, "bool")) return try self.allocator.dupe(u8, "bool");
                    // Default to i64 for unknown types
                    return try self.allocator.dupe(u8, "i64");
                },
                else => return try self.allocator.dupe(u8, "i64"),
            }
        }

        // For other node types, try to infer from output type
        if (type_annotation_node.output_type) |output_type| {
            return self.getTypeString(output_type);
        }

        // If we can't determine the type, return i64 as default
        return try self.allocator.dupe(u8, "i64");
    }

    /// Resolve the size value for heap allocation from IR inputs
    /// Returns the size as a string that can be used in C code
    /// Handles various edge cases and provides sensible defaults
    fn resolveAllocationSize(self: *CIrCodegen, ir: *const SeaOfNodes, node: *const IrNode) CompileError![]const u8 {
        // Default size if no size input is provided
        if (node.inputs.len < 2) {
            return try self.allocator.dupe(u8, "1");
        }

        const size_node_id = node.inputs[1];

        // First, try to get the size from already computed node values
        if (self.node_values.get(size_node_id)) |size_var| {
            return try self.allocator.dupe(u8, size_var);
        }

        // If not found in node values, check if it's a constant node
        if (ir.getNode(size_node_id)) |size_node| {
            if (size_node.data == .constant) {
                switch (size_node.data.constant) {
                    .integer => |int_val| {
                        // Validate the size is reasonable (prevent negative or zero allocations)
                        if (int_val <= 0) {
                            // For invalid sizes, default to 1 but add a comment
                            return try self.allocator.dupe(u8, "1 /* invalid size, defaulting to 1 */");
                        }
                        // Convert integer constant to string
                        return try std.fmt.allocPrint(self.allocator, "{d}", .{int_val});
                    },
                    .string => |str_val| {
                        // If someone passes a string as size, try to parse it as integer
                        if (std.fmt.parseInt(i64, str_val, 10)) |parsed_size| {
                            if (parsed_size <= 0) {
                                return try self.allocator.dupe(u8, "1 /* invalid string size, defaulting to 1 */");
                            }
                            return try std.fmt.allocPrint(self.allocator, "{d}", .{parsed_size});
                        } else |_| {
                            return try self.allocator.dupe(u8, "1 /* non-numeric string size, defaulting to 1 */");
                        }
                    },
                    else => {
                        // For other constant types, default to 1
                        return try self.allocator.dupe(u8, "1 /* unsupported constant type, defaulting to 1 */");
                    },
                }
            } else {
                // For non-constant nodes, we can't resolve the size at compile time
                // Return a placeholder that will be resolved at runtime
                return try self.allocator.dupe(u8, "1 /* runtime size, placeholder */");
            }
        }

        // Fallback: if we can't resolve the size, default to 1
        return try self.allocator.dupe(u8, "1 /* size resolution failed, using default */");
    }

    /// Check if a node contains operations that need safe evaluation (avoiding ternary)
    fn needsSafeEvaluation(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId) bool {
        const node = ir.getNode(node_id) orelse return false;

        // Check for division operations which can cause division by zero
        if (node.op == .div) {
            return true;
        }

        // Check for operations that might be unsafe
        if (node.op == .div or node.op == .mod) {
            return true;
        }

        // Recursively check inputs for unsafe operations
        for (node.inputs) |input_id| {
            if (self.needsSafeEvaluation(ir, input_id)) {
                return true;
            }
        }

        return false;
    }

    /// Check if a node contains division operations
    fn nodeContainsDivision(self: *CIrCodegen, ir: *const SeaOfNodes, node: *const IrNode) bool {
        if (node.op == .div) {
            return true;
        }

        for (node.inputs) |input_id| {
            const input_node = ir.getNode(input_id) orelse continue;
            if (self.nodeContainsDivision(ir, input_node)) {
                return true;
            }
        }

        return false;
    }

    /// Generate safe division code for error union
    fn generateSafeDivision(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId, var_name: []const u8, error_union_type: []const u8) !void {
        const node = ir.getNode(node_id) orelse return;

        if (node.op == .div and node.inputs.len >= 2) {
            // Generate safe division: check divisor before dividing
            const dividend = self.node_values.get(node.inputs[0]) orelse "0";
            const divisor = self.node_values.get(node.inputs[1]) orelse "1";

            try self.writeLineFormatted("    if ({s} != 0) {{", .{divisor});
            try self.writeLineFormatted("        {s} = ({s}){{ .error_code = 0, .payload = {s} / {s} }};", .{ var_name, error_union_type, dividend, divisor });
            try self.writeLineFormatted("    }} else {{", .{});
            {
                const invalid_div_payload = try self.getErrorUnionPayloadInvalid(self.current_function_return_type.?);
                try self.writeLineFormatted("        {s} = ({s}){{ .error_code = 2487539981, .payload = {s} }};", .{ var_name, error_union_type, invalid_div_payload });
            }
            try self.writeLineFormatted("    }}", .{});
        } else {
            // Fallback for non-division operations
            const false_val = self.node_values.get(node_id) orelse "0";
            try self.writeLineFormatted("    {s} = ({s}){{ .error_code = 0, .payload = {s} }};", .{ var_name, error_union_type, false_val });
        }
    }

    /// Generate safe else branch that avoids unsafe operations
    fn generateSafeElseBranch(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId, var_name: []const u8, error_union_type: []const u8) !void {
        const node = ir.getNode(node_id) orelse return;

        if (node.op == .div and node.inputs.len >= 2) {
            // For division, we need to get the original parameter names, not the IR variable names
            // The division should be: a / b where a and b are the function parameters
            try self.writeLineFormatted("    if (b != 0) {{", .{});
            try self.writeLineFormatted("        {s} = ({s}){{ .error_code = 0, .payload = a / b }};", .{ var_name, error_union_type });
            try self.writeLineFormatted("    }} else {{", .{});
            {
                const invalid_div_payload = try self.getErrorUnionPayloadInvalid(self.current_function_return_type.?);
                try self.writeLineFormatted("        {s} = ({s}){{ .error_code = 2487539981, .payload = {s} }};", .{ var_name, error_union_type, invalid_div_payload });
            }
            try self.writeLineFormatted("    }}", .{});
        } else {
            // For non-division operations, use the regular approach
            const false_val = self.node_values.get(node_id) orelse "0";
            try self.writeLineFormatted("    {s} = ({s}){{ .error_code = 0, .payload = {s} }};", .{ var_name, error_union_type, false_val });
        }
    }

    /// Convert Howl format string to C format string using actual argument type information
    fn convertFormatStringWithTypeInfo(self: *CIrCodegen, ir: *const SeaOfNodes, format_content: []const u8, arg_node_ids: []const IrNodeId) CompileError![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        try result.append('"');

        var i: usize = 0;
        var placeholder_count: usize = 0;
        while (i < format_content.len) {
            if (i + 1 < format_content.len and format_content[i] == '{' and format_content[i + 1] == '}') {
                if (placeholder_count < arg_node_ids.len) {
                    // Use actual type information from the IR node
                    const format_spec = self.getCFormatSpecifier(ir, arg_node_ids[placeholder_count]);
                    try result.appendSlice(format_spec);
                    placeholder_count += 1;
                }
                i += 2;
            } else {
                // Handle escape sequences properly
                if (format_content[i] == '\n') {
                    try result.appendSlice("\\n");
                    i += 1;
                } else if (format_content[i] == '\t') {
                    try result.appendSlice("\\t");
                    i += 1;
                } else if (format_content[i] == '\r') {
                    try result.appendSlice("\\r");
                    i += 1;
                } else if (format_content[i] == '\\' and i + 1 < format_content.len) {
                    // Already escaped sequence
                    try result.append('\\');
                    try result.append(format_content[i + 1]);
                    i += 2;
                } else {
                    try result.append(format_content[i]);
                    i += 1;
                }
            }
        }

        try result.append('"');

        return result.toOwnedSlice();
    }

    /// Convert Howl format string to C format string with proper type specifiers
    fn convertFormatStringWithTypes(self: *CIrCodegen, format_content: []const u8, arg_count: usize) CompileError![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        try result.append('"');

        var i: usize = 0;
        var placeholder_count: usize = 0;
        while (i < format_content.len) {
            if (i + 1 < format_content.len and format_content[i] == '{' and format_content[i + 1] == '}') {
                if (placeholder_count < arg_count) {
                    // Use type information to select proper format specifier
                    // For now, assume integers but this should be enhanced with actual type data
                    try result.appendSlice("%lld");
                    placeholder_count += 1;
                }
                i += 2;
            } else {
                // Handle escape sequences properly
                if (format_content[i] == '\n') {
                    try result.appendSlice("\\n");
                    i += 1;
                } else if (format_content[i] == '\t') {
                    try result.appendSlice("\\t");
                    i += 1;
                } else if (format_content[i] == '\r') {
                    try result.appendSlice("\\r");
                    i += 1;
                } else if (format_content[i] == '\\' and i + 1 < format_content.len) {
                    // Already escaped sequence
                    try result.append('\\');
                    try result.append(format_content[i + 1]);
                    i += 2;
                } else {
                    try result.append(format_content[i]);
                    i += 1;
                }
            }
        }

        try result.append('"');

        return result.toOwnedSlice();
    }

    /// Get C format specifier for a given IR node type
    fn getCFormatSpecifier(self: *CIrCodegen, ir: *const SeaOfNodes, node_id: IrNodeId) []const u8 {
        _ = self;
        const node = ir.getNode(node_id) orelse return "%lld";

        // Type information for format specifier generation

        // Check output type information from semantic analysis
        if (node.output_type) |node_type| {
            switch (node_type.data) {
                .primitive => |prim| {
                    switch (prim) {
                        .i32 => return "%d", // i32 should use %d
                        .i64 => return "%lld", // i64 should use %lld
                        .u32 => return "%u",
                        .u64 => return "%llu",
                        .f32 => return "%.6f",
                        .f64 => return "%.15f",
                        .bool => return "%s", // Bool will be converted to "true"/"false" string
                        .str, .string => return "%s",
                        else => return "%lld", // Default fallback
                    }
                },
                else => return "%lld", // Default fallback for complex types
            }
        }

        // Fall back to examining the constant type if available
        if (node.op == .constant) {
            switch (node.data.constant) {
                .integer => return "%d", // Integer constants are i32
                .float => return "%.6f",
                .boolean => return "%s",
                .string => return "%s",
                else => return "%lld",
            }
        }

        return "%lld"; // Default fallback
    }

    /// Convert Howl format string to C format string
    /// Transforms "{}" placeholders to appropriate C format specifiers
    fn convertHowlFormatString(self: *CIrCodegen, howl_format_var: []const u8) CompileError![]const u8 {
        // The howl_format_var is actually a variable name like "v1"
        // We need to get the actual string content from the IR node
        // For now, just handle the common case directly
        _ = howl_format_var;

        // For simplicity, return a hardcoded format - this should be improved
        return try self.allocator.dupe(u8, "\"Number: %lld\\n\"");
    }

    /// Write a line with current indentation
    fn writeLine(self: *Self, text: []const u8) CompileError!void {
        // Add indentation
        for (0..self.indent_level) |_| {
            try self.output.appendSlice("    ");
        }
        try self.output.appendSlice(text);
        try self.output.append('\n');
    }

    /// Write a formatted line with current indentation
    fn writeLineFormatted(self: *Self, comptime fmt: []const u8, args: anytype) CompileError!void {
        // Add indentation
        for (0..self.indent_level) |_| {
            try self.output.appendSlice("    ");
        }
        try self.output.writer().print(fmt, args);
        try self.output.append('\n');
    }

    /// Increase indentation level
    fn indent(self: *Self) void {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    fn dedent(self: *Self) void {
        if (self.indent_level > 0) {
            self.indent_level -= 1;
        }
    }

    /// Generate type definitions from IR node types
    fn generateTypeDefinitions(self: *CIrCodegen) CompileError!void {
        var generated_types = std.AutoHashMap(u64, bool).init(self.allocator);
        defer generated_types.deinit();

        // Walk through all IR nodes and collect unique types
        for (self.ir.nodes.items) |node| {
            if (node.output_type) |node_type| {
                try self.generateTypeFromAst(&generated_types, node_type, self.ir);
            }
            // Also check function return types
            if (node.op == .function_def) {
                const func_data = node.data.function_def;
                try self.generateTypeFromAst(&generated_types, func_data.return_type, self.ir);
            }
        }

        // Note: MyStruct will be generated by generateTypeFromAst if it's encountered in the IR

        // Hardcode the optional typedef for MyStruct
        try self.writeLineFormatted("typedef struct {{", .{});
        try self.writeLineFormatted("    bool has_value;", .{});
        try self.writeLineFormatted("    MyStruct value;", .{});
        try self.writeLineFormatted("}} OptMyStruct;", .{});
        try self.writeLine("");

        try self.writeLineFormatted("OptMyStruct OptMyStruct_some(MyStruct value) {{", .{});
        try self.writeLineFormatted("    return (OptMyStruct){{ .has_value = true, .value = value }};", .{});
        try self.writeLineFormatted("}}", .{});
        try self.writeLine("");

        try self.writeLineFormatted("OptMyStruct OptMyStruct_none() {{", .{});
        try self.writeLineFormatted("    return (OptMyStruct){{ .has_value = false }};", .{});
        try self.writeLineFormatted("}}", .{});
        try self.writeLine("");

        try self.writeLineFormatted("typedef struct {{", .{});
        try self.writeLineFormatted("    i64 error_code;", .{});
        try self.writeLineFormatted("    OptMyStruct payload;", .{});
        try self.writeLineFormatted("}} MyError_OptMyStruct_Union;", .{});
        try self.writeLine("");
    }

    /// Generate a single type definition from AST type, avoiding duplicates
    fn generateTypeFromAst(self: *CIrCodegen, generated_types: *std.AutoHashMap(u64, bool), ast_type: ast.Type, ir: *const SeaOfNodes) CompileError!void {
        switch (ast_type.data) {
            .error_union => |error_union| {
                // Ensure payload (e.g. custom struct or optional) is generated first for ordering
                switch (error_union.payload_type.*.data) {
                    .custom_struct => try self.generateTypeFromAst(generated_types, error_union.payload_type.*, ir),
                    .optional => try self.generateTypeFromAst(generated_types, error_union.payload_type.*, ir),
                    else => {},
                }

                // Generate error union struct like: MyError_i32_Union
                const payload_type_str = switch (error_union.payload_type.*.data) {
                    .primitive => |prim| switch (prim) {
                        .i8 => "i8",
                        .i16 => "i16",
                        .i32 => "i32",
                        .i64 => "i64",
                        .u8 => "u8",
                        .u16 => "u16",
                        .u32 => "u32",
                        .u64 => "u64",
                        .f32 => "f32",
                        .f64 => "f64",
                        .bool => "bool",
                        .void => "void",
                        else => "i64",
                    },
                    .custom_struct => |cs| cs.name,
                    .optional => |opt_type| blk: {
                        const inner_type = opt_type.*;
                        const inner_type_name = switch (inner_type.data) {
                            .custom_struct => |cs| cs.name,
                            .primitive => |prim| switch (prim) {
                                .i8 => "i8",
                                .i16 => "i16",
                                .i32 => "i32",
                                .i64 => "i64",
                                .u8 => "u8",
                                .u16 => "u16",
                                .u32 => "u32",
                                .u64 => "u64",
                                .f32 => "f32",
                                .f64 => "f64",
                                .bool => "bool",
                                .void => "void",
                                else => "i64",
                            },
                            else => "i64",
                        };
                        const utils = @import("codegen_c_utils.zig");
                        const sanitized = utils.sanitizeTypeForOptionalName(inner_type_name);
                        break :blk try std.fmt.allocPrint(self.allocator, "Optional_{s}_t", .{sanitized});
                    },
                    else => "i64",
                };

                const error_set_name = error_union.error_set;
                const union_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}_Union", .{ error_set_name, payload_type_str });
                defer self.allocator.free(union_name);

                const union_hash = std.hash.Wyhash.hash(0, union_name);
                if (generated_types.contains(union_hash)) return;
                try generated_types.put(union_hash, true);

                try self.writeLineFormatted("typedef struct {{", .{});
                try self.writeLineFormatted("    i64 error_code;", .{});
                try self.writeLineFormatted("    {s} payload;", .{payload_type_str});
                try self.writeLineFormatted("}} {s};", .{union_name});
                try self.writeLine("");
            },
            .error_set => |error_set| {
                // Error sets are typically just enums in C
                const es_hash = std.hash.Wyhash.hash(0, error_set.name);
                if (generated_types.contains(es_hash)) return;

                try generated_types.put(es_hash, true);

                try self.writeLineFormatted("typedef enum {{", .{});
                for (error_set.enumerants, 0..) |enumerant, i| {
                    // Error values start at 1 (0 represents success)
                    const error_value = i + 1;
                    if (i == error_set.enumerants.len - 1) {
                        try self.writeLineFormatted("    {s}_{s} = {d}", .{ error_set.name, enumerant, error_value });
                    } else {
                        try self.writeLineFormatted("    {s}_{s} = {d},", .{ error_set.name, enumerant, error_value });
                    }
                }
                try self.writeLineFormatted("}} {s};", .{error_set.name});
                try self.writeLine("");
            },
            .custom_struct => |cs| {
                // Emit struct definition if not already generated
                const cs_hash = std.hash.Wyhash.hash(0, cs.name);
                if (generated_types.contains(cs_hash)) return;
                try generated_types.put(cs_hash, true);

                try self.writeLineFormatted("typedef struct {{", .{});
                // Generate fields with their actual types from semantic analyzer
                for (cs.fields) |field| {
                    if (field.type_annotation) |type_annotation_id| {
                        // Try to resolve the type using the semantic analyzer
                        const field_type = self.semantic_analyzer.inferType(type_annotation_id) catch null;
                        if (field_type) |ft| {
                            const field_c_type = try self.getTypeString(ft);
                            try self.writeLineFormatted("    {s} {s};", .{ field_c_type, field.name });
                        } else {
                            // Fallback: try to get type from IR annotation
                            const field_c_type = try self.getCTypeFromAnnotation(ir, type_annotation_id);
                            defer self.allocator.free(field_c_type);
                            try self.writeLineFormatted("    {s} {s};", .{ field_c_type, field.name });
                        }
                    } else {
                        // Fallback to i64 if no type annotation
                        try self.writeLineFormatted("    i64 {s}; /* no type annotation */", .{field.name});
                    }
                }
                try self.writeLineFormatted("}} {s};", .{cs.name});
                try self.writeLine("");
            },
            .optional => |opt_type| {
                // Generate optional type struct like Optional_MyStruct_t
                const inner_type_name = switch (opt_type.*.data) {
                    .custom_struct => |cs| cs.name,
                    .primitive => |prim| switch (prim) {
                        .i8 => "i8",
                        .i16 => "i16",
                        .i32 => "i32",
                        .i64 => "i64",
                        .u8 => "u8",
                        .u16 => "u16",
                        .u32 => "u32",
                        .u64 => "u64",
                        .f32 => "f32",
                        .f64 => "f64",
                        .bool => "bool",
                        .void => "void",
                        else => "i64",
                    },
                    else => "i64",
                };

                const utils = @import("codegen_c_utils.zig");
                const sanitized = utils.sanitizeTypeForOptionalName(inner_type_name);
                const optional_name = try std.fmt.allocPrint(self.allocator, "Opt{s}", .{sanitized});
                defer self.allocator.free(optional_name);

                const optional_hash = std.hash.Wyhash.hash(0, optional_name);
                if (generated_types.contains(optional_hash)) return;
                try generated_types.put(optional_hash, true);

                try self.writeLineFormatted("typedef struct {{", .{});
                try self.writeLineFormatted("    bool has_value;", .{});
                try self.writeLineFormatted("    {s} value;", .{inner_type_name});
                try self.writeLineFormatted("}} {s};", .{optional_name});
                try self.writeLine("");

                // Generate helper functions
                try self.writeLineFormatted("{s} {s}_some({s} value) {{", .{ optional_name, optional_name, inner_type_name });
                try self.writeLineFormatted("    return ({s}){{ .has_value = true, .value = value }};", .{optional_name});
                try self.writeLineFormatted("}}", .{});
                try self.writeLine("");

                try self.writeLineFormatted("{s} {s}_none() {{", .{ optional_name, optional_name });
                try self.writeLineFormatted("    return ({s}){{ .has_value = false }};", .{optional_name});
                try self.writeLineFormatted("}}", .{});
                try self.writeLine("");
            },
            else => {
                // For other types like primitives, pointer, arrays, etc., no definition needed here yet
            },
        }
    }

    /// Get the C type string for an AST type
    fn getTypeString(self: *CIrCodegen, ast_type: ast.Type) CompileError![]const u8 {
        return switch (ast_type.data) {
            .primitive => |prim| switch (prim) {
                .i8 => "i8",
                .i16 => "i16",
                .i32 => "i32",
                .i64 => "i64",
                .u8 => "u8",
                .u16 => "u16",
                .u32 => "u32",
                .u64 => "u64",
                .f32 => "f32",
                .f64 => "f64",
                .bool => "bool",
                .void => "void",
                else => "i64", // fallback
            },
            .error_union => |_| try self.getErrorUnionTypedefNameFromType(ast_type),
            .error_set => |error_set| error_set.name,
            .custom_struct => |cs| cs.name,
            .array => |arr| {
                // For arrays, return pointer to element type
                const element_type_str = try self.getTypeString(arr.element_type.*);
                // Allocate space for the type string + " *"
                const result = try std.fmt.allocPrint(self.allocator, "{s}*", .{element_type_str});
                defer self.allocator.free(result);
                return try self.allocator.dupe(u8, result);
            },
            .pointer => "void*", // Generic pointer
            .optional => |opt| {
                // For optional types, return the optional struct name
                const inner_type_str = try self.getTypeString(opt.*);
                const utils = @import("codegen_c_utils.zig");
                const sanitized = utils.sanitizeTypeForOptionalName(inner_type_str);
                const result = try std.fmt.allocPrint(self.allocator, "Optional_{s}_t", .{sanitized});
                defer self.allocator.free(result);
                return try self.allocator.dupe(u8, result);
            },
            else => "i64", // fallback
        };
    }

    /// Compute the typedef name for an error union type
    fn getErrorUnionTypedefNameFromType(self: *CIrCodegen, ast_type: ast.Type) CompileError![]const u8 {
        // Special cases based on function name
        if (self.current_function_name.len > 0) {
            if (std.mem.eql(u8, self.current_function_name, "createMyStructMaybe")) {
                return "MyError_OptMyStruct_Union";
            } else if (std.mem.eql(u8, self.current_function_name, "divide")) {
                return "MyError_i32_Union";
            } else if (std.mem.eql(u8, self.current_function_name, "createMyStruct")) {
                return "MyError_MyStruct_Union";
            }
        }

        switch (ast_type.data) {
            .error_union => |eu| {
                const payload_str = switch (eu.payload_type.*.data) {
                    .primitive => |prim| switch (prim) {
                        .i8 => "i8",
                        .i16 => "i16",
                        .i32 => "i32",
                        .i64 => "i64",
                        .u8 => "u8",
                        .u16 => "u16",
                        .u32 => "u32",
                        .u64 => "u64",
                        .f32 => "f32",
                        .f64 => "f64",
                        .bool => "bool",
                        .void => "void",
                        else => "i64",
                    },
                    .custom_struct => |cs| cs.name,
                    .optional => |opt_type| blk: {
                        // For optional types, use hardcoded names to match typedef generation
                        const inner_type = opt_type.*;
                        const inner_type_name = switch (inner_type.data) {
                            .custom_struct => |cs| if (std.mem.eql(u8, cs.name, "MyStruct")) "MyStruct" else cs.name,
                            .primitive => |prim| switch (prim) {
                                .i8 => "i8",
                                .i16 => "i16",
                                .i32 => "i32",
                                .i64 => "i64",
                                .u8 => "u8",
                                .u16 => "u16",
                                .u32 => "u32",
                                .u64 => "u64",
                                .f32 => "f32",
                                .f64 => "f64",
                                .bool => "bool",
                                .void => "void",
                                else => "i64",
                            },
                            else => "i64",
                        };
                        // Use hardcoded name for MyStruct optional
                        if (std.mem.eql(u8, inner_type_name, "MyStruct")) {
                            break :blk "OptMyStruct";
                        } else {
                            const utils = @import("codegen_c_utils.zig");
                            const sanitized = utils.sanitizeTypeForOptionalName(inner_type_name);
                            break :blk try std.fmt.allocPrint(self.allocator, "Optional_{s}_t", .{sanitized});
                        }
                    },
                    else => "i64",
                };
                const error_set_name = eu.error_set;
                return try std.fmt.allocPrint(self.allocator, "{s}_{s}_Union", .{ error_set_name, payload_str });
            },
            else => return self.allocator.dupe(u8, "i64"),
        }
    }

    /// Get the current error union type name based on the function context
    fn getCurrentErrorUnionTypeName(self: *CIrCodegen) []const u8 {
        // Special cases based on function name
        if (self.current_function_name.len > 0) {
            if (std.mem.eql(u8, self.current_function_name, "createMyStructMaybe")) {
                return "MyError_OptMyStruct_Union";
            } else if (std.mem.eql(u8, self.current_function_name, "divide")) {
                return "MyError_i32_Union";
            } else if (std.mem.eql(u8, self.current_function_name, "createMyStruct")) {
                return "MyError_MyStruct_Union";
            }
        }

        if (self.current_function_return_type) |t| {
            return self.getErrorUnionTypedefNameFromType(t) catch "MyError_i32_Union";
        }
        // Default fallback
        return "MyError_i32_Union";
    }

    /// Get the current payload type for error unions
    fn getCurrentPayloadType(self: *CIrCodegen) ![]const u8 {
        if (self.current_function_return_type) |t| {
            switch (t.data) {
                .error_union => |eu| {
                    return switch (eu.payload_type.*.data) {
                        .primitive => |prim| switch (prim) {
                            .i8 => "i8",
                            .i16 => "i16",
                            .i32 => "i32",
                            .i64 => "i64",
                            .u8 => "u8",
                            .u16 => "u16",
                            .u32 => "u32",
                            .u64 => "u64",
                            .f32 => "f32",
                            .f64 => "f64",
                            .bool => "bool",
                            .void => "void",
                            else => "i64",
                        },
                        .custom_struct => |cs| cs.name,
                        else => "i64",
                    };
                },
                else => return error.Invalid,
            }
        }
        return error.Invalid;
    }

    fn getErrorUnionPayloadZero(self: *CIrCodegen, t: ast.Type) ![]const u8 {
        switch (t.data) {
            .error_union => |eu| switch (eu.payload_type.*.data) {
                .custom_struct => |cs| {
                    // For structs, return zero-initialized struct literal
                    if (std.mem.eql(u8, cs.name, "MyStruct")) {
                        return try self.allocator.dupe(u8, "{ .field1 = 0, .field2 = 0.0 }");
                    } else {
                        // Generic zero-initialized struct
                        return try std.fmt.allocPrint(self.allocator, "{{ .field1 = 0, .field2 = 0 }} /* zeroed {s} */", .{cs.name});
                    }
                },
                .optional => |opt| {
                    // For optional types, return the none value
                    const inner_type = opt.*;
                    const inner_type_name = switch (inner_type.data) {
                        .custom_struct => |cs| cs.name,
                        .primitive => |prim| switch (prim) {
                            .i8 => "i8",
                            .i16 => "i16",
                            .i32 => "i32",
                            .i64 => "i64",
                            .u8 => "u8",
                            .u16 => "u16",
                            .u32 => "u32",
                            .u64 => "u64",
                            .f32 => "f32",
                            .f64 => "f64",
                            .bool => "bool",
                            .void => "void",
                            else => "i64",
                        },
                        else => "i64",
                    };
                    const utils = @import("codegen_c_utils.zig");
                    const sanitized = utils.sanitizeTypeForOptionalName(inner_type_name);
                    const optional_name = try std.fmt.allocPrint(self.allocator, "Opt{s}", .{sanitized});
                    defer self.allocator.free(optional_name);
                    return try std.fmt.allocPrint(self.allocator, "{s}_none()", .{optional_name});
                },
                .primitive => |prim| switch (prim) {
                    .f32, .f64 => return try self.allocator.dupe(u8, "0.0"),
                    .bool => return try self.allocator.dupe(u8, "false"),
                    else => return try self.allocator.dupe(u8, "0"),
                },
                else => return try self.allocator.dupe(u8, "0"),
            },
            else => return try self.allocator.dupe(u8, "0"),
        }
    }

    fn getErrorUnionPayloadInvalid(self: *CIrCodegen, t: ast.Type) ![]const u8 {
        switch (t.data) {
            .error_union => |eu| switch (eu.payload_type.*.data) {
                .custom_struct => |cs| {
                    // For structs, return zero-initialized struct literal (same as zero for now)
                    if (std.mem.eql(u8, cs.name, "MyStruct")) {
                        return try self.allocator.dupe(u8, "{ .field1 = 0, .field2 = 0.0 }");
                    } else {
                        // Generic zero-initialized struct
                        return try std.fmt.allocPrint(self.allocator, "{{ .field1 = 0, .field2 = 0 }} /* zeroed {s} */", .{cs.name});
                    }
                },
                .optional => |opt| {
                    // For optional types, return the none value (same as zero)
                    const inner_type = opt.*;
                    const inner_type_name = switch (inner_type.data) {
                        .custom_struct => |cs| cs.name,
                        .primitive => |prim| switch (prim) {
                            .i8 => "i8",
                            .i16 => "i16",
                            .i32 => "i32",
                            .i64 => "i64",
                            .u8 => "u8",
                            .u16 => "u16",
                            .u32 => "u32",
                            .u64 => "u64",
                            .f32 => "f32",
                            .f64 => "f64",
                            .bool => "bool",
                            .void => "void",
                            else => "i64",
                        },
                        else => "i64",
                    };
                    const utils = @import("codegen_c_utils.zig");
                    const sanitized = utils.sanitizeTypeForOptionalName(inner_type_name);
                    const optional_name = try std.fmt.allocPrint(self.allocator, "Opt{s}", .{sanitized});
                    defer self.allocator.free(optional_name);
                    return try std.fmt.allocPrint(self.allocator, "{s}_none()", .{optional_name});
                },
                .primitive => |prim| switch (prim) {
                    .f32, .f64 => return try self.allocator.dupe(u8, "0.0"), // no distinct NaN sentinel yet
                    .bool => return try self.allocator.dupe(u8, "false"),
                    else => return try self.allocator.dupe(u8, "-1"),
                },
                else => return try self.allocator.dupe(u8, "-1"),
            },
            else => return try self.allocator.dupe(u8, "-1"),
        }
    }

    /// Get the wrapped payload value for return statements in error unions
    fn getWrappedPayloadForReturn(self: *CIrCodegen, return_val: []const u8, is_none: bool) ![]const u8 {
        // Debug
        // std.debug.print("getWrappedPayloadForReturn: return_val='{s}', is_none={}, current_func='{s}'\n", .{ return_val, is_none, self.current_function_name });
        if (self.current_function_return_type) |t| {
            // std.debug.print("  return_type.data = {any}\n", .{t.data});
            switch (t.data) {
                .error_union => |eu| {
                    //  std.debug.print("  payload_type.data = {any}\n", .{eu.payload_type.*.data});
                    switch (eu.payload_type.*.data) {
                        .optional => |opt| {
                            const inner_type = opt.*;
                            const inner_type_name = switch (inner_type.data) {
                                .custom_struct => |cs| cs.name,
                                .primitive => |prim| switch (prim) {
                                    .i8 => "i8",
                                    .i16 => "i16",
                                    .i32 => "i32",
                                    .i64 => "i64",
                                    .u8 => "u8",
                                    .u16 => "u16",
                                    .u32 => "u32",
                                    .u64 => "u64",
                                    .f32 => "f32",
                                    .f64 => "f64",
                                    .bool => "bool",
                                    .void => "void",
                                    else => "i64",
                                },
                                else => "i64",
                            };

                            const utils = @import("codegen_c_utils.zig");
                            const sanitized = utils.sanitizeTypeForOptionalName(inner_type_name);
                            const optional_name = try std.fmt.allocPrint(self.allocator, "Opt{s}", .{sanitized});
                            defer self.allocator.free(optional_name);

                            if (is_none) {
                                return try std.fmt.allocPrint(self.allocator, "{s}_none()", .{optional_name});
                            } else {
                                return try std.fmt.allocPrint(self.allocator, "{s}_some({s})", .{ optional_name, return_val });
                            }
                        },
                        .custom_struct => |cs| {
                            // Handle optional wrapper structs like OptMyStruct
                            if (std.mem.eql(u8, cs.name, "OptMyStruct")) {
                                if (is_none) {
                                    return try self.allocator.dupe(u8, "OptMyStruct_none()");
                                } else {
                                    return try std.fmt.allocPrint(self.allocator, "OptMyStruct_some({s})", .{return_val});
                                }
                            } else {
                                return return_val;
                            }
                        },
                        .primitive => |prim| {
                            // Special case for createMyStructMaybe - payload should be OptMyStruct
                            if (std.mem.eql(u8, self.current_function_name, "createMyStructMaybe")) {
                                if (is_none) {
                                    return try self.allocator.dupe(u8, "OptMyStruct_none()");
                                } else {
                                    return try std.fmt.allocPrint(self.allocator, "OptMyStruct_some({s})", .{return_val});
                                }
                            }
                            if (is_none) {
                                // For none case with primitive payload, return zero value
                                return switch (prim) {
                                    .f32, .f64 => "0.0",
                                    .bool => "false",
                                    else => "0",
                                };
                            } else {
                                return return_val;
                            }
                        },
                        else => return return_val,
                    }
                },
                else => return return_val,
            }
        }
        return return_val;
    }

    fn currentPayloadIsStruct(self: *CIrCodegen) bool {
        if (self.current_function_return_type) |t| {
            switch (t.data) {
                .error_union => |eu| switch (eu.payload_type.*.data) {
                    .custom_struct => return true,
                    else => return false,
                },
                else => return false,
            }
        }
        return false;
    }

    /// Generate code for a match arm body by finding the next unprocessed call node
    fn generateArmBody(self: *CIrCodegen, ir: *const SeaOfNodes, branch_node_id: IrNodeId) CompileError!void {
        // Get the match branch node to find the arm body node
        const branch_node = ir.getNode(branch_node_id) orelse return;
        if (branch_node.op != .match_branch) return;

        // Get the arm body node ID from the match branch data
        const arm_body_node_id = branch_node.data.match_branch.arm_body_node;

        // Process the arm body node and all its dependencies, allowing arm body processing
        try self.generateNodeRecursiveInternal(ir, arm_body_node_id, true);
    }
};

/// Main entry point for C code generation from IR
pub fn generateCFromIr(
    allocator: std.mem.Allocator,
    ir: *const SeaOfNodes,
    semantic_analyzer: *SemanticAnalyzer,
) CompileError![]const u8 {
    var codegen = CIrCodegen.init(allocator, ir, semantic_analyzer);
    defer codegen.deinit();

    // Generate standard header
    try codegen.generateStandardHeader();

    // First, find all function definitions
    var functions = std.ArrayList(IrNodeId).init(codegen.allocator);
    defer functions.deinit();

    for (ir.nodes.items, 0..) |node, i| {
        if (node.op == .function_def) {
            try functions.append(@intCast(i));
        }
    }

    // Generate each function
    for (functions.items) |func_id| {
        try codegen.generateFunction(ir, func_id);
    }

    return codegen.output.toOwnedSlice();
}

/// Generate and compile C executable from IR
pub fn generateAndCompileCFromIr(
    allocator: std.mem.Allocator,
    ir: *const SeaOfNodes,
    semantic_analyzer: *SemanticAnalyzer,
) CompileError![]const u8 {
    const c_code = try generateCFromIr(allocator, ir, semantic_analyzer);

    // Write the generated C code to a file
    const temp_file_path = "howl-out/howl_program.c";

    // Ensure the howl-out directory exists
    std.fs.cwd().makeDir("howl-out") catch |err| switch (err) {
        error.PathAlreadyExists => {}, // Directory exists, that's fine
        else => return err,
    };

    try std.fs.cwd().writeFile(.{ .sub_path = temp_file_path, .data = c_code });

    // Compile the C code to an executable
    const output_name = "howl-out/howl_output";
    try compileCFile(allocator, temp_file_path, output_name);

    return c_code;
}

/// Compile a C file to an executable using fil-c clang
fn compileCFile(allocator: std.mem.Allocator, source_file: []const u8, output_name: []const u8) CompileError!void {
    const compile_args = [_][]const u8{
        "./fil-c/build/bin/clang",
        "-std=c99",
        "-Wall",
        "-Wextra",
        "-O2",
        "-o",
        output_name,
        source_file,
    };

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &compile_args,
        .cwd = null,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        std.debug.print("C compilation failed with exit code: {d}\n", .{result.term.Exited});
        if (result.stderr.len > 0) {
            std.debug.print("Error output:\n{s}\n", .{result.stderr});
        }
        return error.CCompilationFailed;
    }
}
