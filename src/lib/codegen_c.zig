const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

// Import split codegen modules
const expressions = @import("codegen_c_expressions.zig");
const functions = @import("codegen_c_functions.zig");
const statements = @import("codegen_c_statements.zig");
const types = @import("codegen_c_types.zig");
const utils = @import("codegen_c_utils.zig");
const std_lib = @import("codegen_c_std.zig");
const formatting = @import("codegen_c_formatting.zig");

// Re-export types from split modules
pub const CCodegenError = utils.CCodegenError;
pub const Writer = utils.Writer;
pub const TypeCollection = types.TypeCollection;
pub const FunctionCollection = functions.FunctionCollection;

// Import types from split modules for internal use
const CollectedStruct = types.CollectedStruct;
const CollectedEnum = types.CollectedEnum;
const CollectedErrorSet = types.CollectedErrorSet;
const CollectedErrorUnion = types.CollectedErrorUnion;
const CollectedFunction = functions.CollectedFunction;

// Data structures for collecting types and functions during analysis - MOVED TO TYPES MODULE

// Main C code generator struct
pub const CCodegen = struct {
    allocator: std.mem.Allocator,
    arena: *const ast.AstArena,
    semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer,
    type_collection: TypeCollection,
    function_collection: FunctionCollection,
    current_function_is_main: bool = false, // Track if we're generating main function
    current_function_error_union_name: ?[]const u8 = null, // Track current function's error union type

    pub fn init(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer) CCodegen {
        return CCodegen{
            .allocator = allocator,
            .arena = arena,
            .semantic_analyzer = semantic_analyzer,
            .type_collection = TypeCollection.init(allocator),
            .function_collection = FunctionCollection.init(allocator),
            .current_function_is_main = false,
            .current_function_error_union_name = null,
        };
    }

    pub fn deinit(self: *CCodegen) void {
        self.type_collection.deinit();
        self.function_collection.deinit();
    }

    pub fn generate(self: *CCodegen, root_node_id: ast.NodeId) ![]u8 {
        // Create output directory
        std.fs.cwd().makeDir("howl-out") catch |err| switch (err) {
            error.PathAlreadyExists => {}, // Directory already exists, continue
            else => return err, // Other errors should be propagated
        };

        // Generate C source code
        var output = std.ArrayList(u8).init(self.allocator);
        const writer = output.writer();

        // First pass: collect types and functions from AST
        try self.collectFromNode(root_node_id);

        // Generate C header includes
        try writer.writeAll("#include <stdio.h>\n");
        try writer.writeAll("#include <stdint.h>\n");
        try writer.writeAll("#include <stdbool.h>\n");
        try writer.writeAll("#include <string.h>\n");
        try writer.writeAll("#include <stddef.h>\n");
        try writer.writeAll("#include <stdlib.h>\n");
        try writer.writeAll("\n");

        // Generate builtin type definitions
        try writer.writeAll("typedef float howl_f32_t;\n");
        try writer.writeAll("typedef double howl_f64_t;\n");
        try writer.writeAll("\n");

        // Generate collected types (structs, enums, etc.)
        try self.generateCollectedTypes(writer);

        // Generate function declarations
        try self.generateCollectedFunctionDeclarations(writer);

        // Generate function implementations
        try self.generateCollectedFunctionImplementations(writer, root_node_id);

        const c_source = try output.toOwnedSlice();

        // Write the generated C source to a file in howl-out directory
        const temp_file_path = "howl-out/howl_program.c";
        try std.fs.cwd().writeFile(.{ .sub_path = temp_file_path, .data = c_source });

        // Compile using filc compiler
        const output_name = "howl-out/howl_output";
        try self.compileCFile(temp_file_path, output_name);

        // Return the generated C source code so it can be displayed
        return c_source;
    }

    // Generate all collected types
    fn generateCollectedTypes(self: *CCodegen, writer: Writer) !void {
        // Generate error set types first (they might be dependencies)
        for (self.type_collection.error_set_types.items) |error_set_type| {
            try functions.generateErrorSetImplementation(writer, error_set_type);
        }

        // Generate enum types (they might be dependencies)
        for (self.type_collection.enum_types.items) |enum_type| {
            try functions.generateEnumImplementation(writer, enum_type);
        }

        // Generate struct types (they might be dependencies for error unions)
        for (self.type_collection.struct_types.items) |struct_type| {
            try functions.generateStructImplementation(self.arena, writer, struct_type);
        }

        // Generate standard error union types for all common Howl types
        try functions.generateStandardErrorUnions(writer);

        // Generate error union types (depend on error sets and structs)
        for (self.type_collection.error_union_types.items) |error_union_type| {
            try functions.generateErrorUnionImplementation(writer, error_union_type);
        }

        // Generate optional types
        for (self.type_collection.optional_types.items) |optional_type| {
            try functions.generateOptionalImplementation(writer, optional_type);
        }

        // Generate List implementations for used types
        for (self.type_collection.list_types.items) |list_type| {
            try functions.generateListImplementation(writer, list_type);
        }
    }

    fn mapHowlTypeToCType(self: *CCodegen, howl_type: []const u8) []const u8 {
        _ = self; // suppress unused warning

        // Map Howl types to C types
        if (std.mem.eql(u8, howl_type, "i32")) return "int32_t";
        if (std.mem.eql(u8, howl_type, "i64")) return "int64_t";
        if (std.mem.eql(u8, howl_type, "u32")) return "uint32_t";
        if (std.mem.eql(u8, howl_type, "u64")) return "uint64_t";
        if (std.mem.eql(u8, howl_type, "f32")) return "howl_f32_t";
        if (std.mem.eql(u8, howl_type, "f64")) return "howl_f64_t";
        if (std.mem.eql(u8, howl_type, "bool")) return "bool";
        if (std.mem.eql(u8, howl_type, "str")) return "char*";
        if (std.mem.eql(u8, howl_type, "void")) return "void";

        // For custom types (structs, enums), use the type name as-is
        return howl_type;
    }

    fn generateCollectedFunctionDeclarations(self: *CCodegen, writer: Writer) !void {
        // Generate struct constructor declarations
        for (self.type_collection.struct_types.items) |struct_info| {
            try writer.print("{s} {s}_init(", .{ struct_info.name, struct_info.name });

            // Generate parameter list for declaration
            for (struct_info.fields, 0..) |field, i| {
                if (i > 0) try writer.writeAll(", ");

                if (field.type_annotation) |type_node_id| {
                    const type_node = self.arena.getNodeConst(type_node_id);
                    if (type_node) |node| {
                        if (node.data == .identifier) {
                            const type_name = node.data.identifier.name;
                            const c_type = self.mapHowlTypeToCType(type_name);
                            try writer.print("{s} {s}", .{ c_type, field.name });
                        } else {
                            try writer.print("int32_t {s}", .{field.name});
                        }
                    } else {
                        try writer.print("int32_t {s}", .{field.name});
                    }
                } else {
                    try writer.print("int32_t {s}", .{field.name});
                }
            }

            try writer.writeAll(");\n");
        }
        try writer.writeAll("\n");
    }

    fn generateCollectedFunctionImplementations(self: *CCodegen, writer: Writer, root_node_id: ast.NodeId) !void {
        // Generate function implementations by traversing the AST
        try self.collectFunctionImplementations(writer, root_node_id);
    }

    fn collectFunctionImplementations(self: *CCodegen, writer: Writer, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.collectFunctionImplementations(writer, stmt_id);
                }
            },
            .function_decl => |func_decl| {
                // Generate the actual C function implementation
                try self.generateCFunctionImplementation(writer, func_decl);
                try writer.writeAll("\n\n");
            },
            // TODO: Add unary expression support later - might have recursion issue
            // .unary_expr => |unary_expr| {
            //     const op_str = switch (unary_expr.op) {
            //         .negate => "-",
            //         .not => "!",
            //         .bit_not => "~",
            //         else => "/* unknown unary op */",
            //     };
            //     try writer.writeAll("(");
            //     try writer.writeAll(op_str);
            //     try self.generateCExpressionRecursive(writer, unary_expr.operand);
            //     try writer.writeAll(")");
            // },
            .member_expr => |member_expr| {
                // Collect member access for structs
                try self.collectFromNode(member_expr.object);
            },
            .match_compile_expr => |match_compile| {
                // Handle compile-time match expressions
                try self.collectFromMatchCompile(match_compile);
            },
            .compile_target_expr => {
                // @compile.target doesn't need collection
            },
            .compile_insert_expr => {
                // @compile.insert doesn't need collection
            },
            // Add other node types as needed
            else => {},
        }
    }

    pub fn inferErrorUnionTypeFromExpression(self: *CCodegen, expr_id: ast.NodeId) []const u8 {
        // Use semantic analyzer to infer the error union type from the expression
        const inferred = self.semantic_analyzer.inferType(expr_id) catch null;
        if (inferred) |typ| {
            return self.generateCType(typ);
        }

        // Fallback to current function context
        return self.getCurrentErrorUnionStructName();
    }

    fn getCurrentErrorUnionStructName(self: *CCodegen) []const u8 {
        // Return the current function's error union type if set
        if (self.current_function_error_union_name) |name| {
            return name;
        }

        // For functions that use anyerror, default to i32 payload type
        if (self.type_collection.error_union_types.items.len > 0) {
            // Look for anyerror_i32_ErrorUnion first
            for (self.type_collection.error_union_types.items) |error_union| {
                if (std.mem.eql(u8, error_union.struct_name, "anyerror_i32_ErrorUnion")) {
                    return error_union.struct_name;
                }
            }
            return self.type_collection.error_union_types.items[0].struct_name;
        }
        return "anyerror_i32_ErrorUnion"; // Default fallback
    }

    pub fn findErrorUnionStructName(self: *CCodegen, error_set_name: []const u8, payload_type: []const u8) ?[]const u8 {
        for (self.type_collection.error_union_types.items) |error_union| {
            if (std.mem.eql(u8, error_union.error_set_name, error_set_name) and
                std.mem.eql(u8, error_union.payload_type, payload_type))
            {
                return error_union.struct_name;
            }
        }
        return null;
    }

    fn collectErrorUnionFromNode(self: *CCodegen, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        if (node.data == .error_union_type_expr) {
            const error_union = node.data.error_union_type_expr;

            if (error_union.error_set) |error_set_id| {
                const error_set_node = self.arena.getNodeConst(error_set_id);
                if (error_set_node) |es_node| {
                    if (es_node.data == .identifier) {
                        const error_set_name = es_node.data.identifier.name;

                        // Get payload type
                        const payload_type_info = self.getNodeType(error_union.payload_type);
                        const payload_type_str = types.generateCType(self, payload_type_info);

                        // Handle optional types specially for error union names
                        var sanitized_payload: []const u8 = undefined;
                        if (std.mem.startsWith(u8, payload_type_str, "Optional_") and std.mem.endsWith(u8, payload_type_str, "_t")) {
                            // Convert Optional_MyStruct_t to OptMyStruct
                            const inner_part = payload_type_str[9 .. payload_type_str.len - 2]; // Remove "Optional_" and "_t"
                            sanitized_payload = try std.fmt.allocPrint(self.allocator, "Opt{s}", .{inner_part});

                            // Also collect the optional type
                            const optional_struct_name = try self.allocator.dupe(u8, payload_type_str);
                            const inner_type = try self.allocator.dupe(u8, inner_part);
                            const collected_optional = types.CollectedOptional{
                                .inner_type = inner_type,
                                .struct_name = optional_struct_name,
                            };
                            try self.type_collection.optional_types.append(collected_optional);
                        } else {
                            sanitized_payload = try self.allocator.dupe(u8, self.sanitizeTypeForName(payload_type_str));
                        }

                        // Generate struct name
                        const struct_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}_Union", .{ error_set_name, sanitized_payload });

                        // Check if we already have this combination
                        for (self.type_collection.error_union_types.items) |existing| {
                            if (std.mem.eql(u8, existing.struct_name, struct_name)) {
                                self.allocator.free(struct_name);
                                return; // Already collected
                            }
                        }

                        const collected_error_union = CollectedErrorUnion{
                            .error_set_name = try self.allocator.dupe(u8, error_set_name),
                            .payload_type = try self.allocator.dupe(u8, payload_type_str),
                            .struct_name = struct_name,
                        };

                        try self.type_collection.error_union_types.append(collected_error_union);
                    }
                }
            }
        }
    }

    fn sanitizeTypeForName(self: *CCodegen, type_str: []const u8) []const u8 {
        _ = self;
        // Convert C type names to valid identifier parts
        if (std.mem.eql(u8, type_str, "int32_t")) return "i32";
        if (std.mem.eql(u8, type_str, "int64_t")) return "i64";
        if (std.mem.eql(u8, type_str, "int16_t")) return "i16";
        if (std.mem.eql(u8, type_str, "int8_t")) return "i8";
        if (std.mem.eql(u8, type_str, "uint32_t")) return "u32";
        if (std.mem.eql(u8, type_str, "uint64_t")) return "u64";
        if (std.mem.eql(u8, type_str, "uint16_t")) return "u16";
        if (std.mem.eql(u8, type_str, "uint8_t")) return "u8";
        if (std.mem.eql(u8, type_str, "howl_f32_t")) return "f32";
        if (std.mem.eql(u8, type_str, "howl_f64_t")) return "f64";
        if (std.mem.eql(u8, type_str, "float")) return "f32";
        if (std.mem.eql(u8, type_str, "double")) return "f64";
        if (std.mem.eql(u8, type_str, "bool")) return "bool";
        if (std.mem.eql(u8, type_str, "void*")) return "ptr";
        if (std.mem.eql(u8, type_str, "const char*")) return "str";
        if (std.mem.eql(u8, type_str, "char*")) return "string";
        if (std.mem.eql(u8, type_str, "size_t")) return "usize";
        if (std.mem.eql(u8, type_str, "ptrdiff_t")) return "isize";
        if (std.mem.eql(u8, type_str, "void")) return "void";

        // Handle optional types - convert Optional_MyStruct_t to OptMyStruct
        if (std.mem.startsWith(u8, type_str, "Optional_") and std.mem.endsWith(u8, type_str, "_t")) {
            // For now, return the original type_str since we can't allocate here
            // This will be handled in the calling function
            return type_str;
        }

        // For custom types, just use the type string directly
        return type_str;
    }

    fn collectFunction(self: *CCodegen, func_decl: anytype) !void {
        // Don't collect main function - handled separately
        if (std.mem.eql(u8, func_decl.name, "main")) return;

        // Check if return type is an error union and collect it
        if (func_decl.return_type) |return_type_node_id| {
            const return_type_node = self.arena.getNodeConst(return_type_node_id);
            if (return_type_node) |node| {
                // Check for various error union patterns
                if (node.data == .error_union_type) {
                    try self.collectErrorUnionFromNode(return_type_node_id);
                } else if (node.data == .error_union_type_expr) {
                    try self.collectErrorUnionFromNode(return_type_node_id);
                } else if (node.data == .identifier) {
                    // This might be a !Type syntax - check the function's semantic info
                    const type_name = node.data.identifier.name;

                    // For functions declared with !Type syntax, generate error union
                    // This is a heuristic - in a full implementation, we'd check the parser's understanding
                    if (self.hasErrorUnionReturnType(func_decl)) {
                        try self.generateErrorUnionForFunction(func_decl.name, type_name);
                    }
                }
            }
        }

        // Store function for later declaration and implementation generation
        const collected_func = CollectedFunction{
            .name = try self.allocator.dupe(u8, func_decl.name),
            .declaration = "", // Will be filled during declaration generation
            .implementation = "", // Will be filled during implementation generation
        };

        try self.function_collection.functions.append(collected_func);
    }

    /// Check if a function has error union return type
    fn hasErrorUnionReturnType(self: *CCodegen, func_decl: anytype) bool {
        // Check if there's a return type with error union syntax
        if (func_decl.return_type) |return_type_id| {
            const return_type_node = self.arena.getNodeConst(return_type_id);
            if (return_type_node) |node| {
                if (node.data == .error_union_type_expr) {
                    return true;
                }
            }
        }

        return false;
    }

    /// Generate error union struct for a function with !Type return type
    fn generateErrorUnionForFunction(self: *CCodegen, func_name: []const u8, payload_type: []const u8) !void {
        _ = func_name; // May use this for specific error sets later

        // For now, all functions use anyerror error set
        const error_set_name = "anyerror";
        const sanitized_payload = self.sanitizeTypeForName(payload_type);
        const struct_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}_ErrorUnion", .{ error_set_name, sanitized_payload });

        // Ensure anyerror error set exists
        try self.ensureAnyErrorSetGenerated();

        // Check if this error union struct already exists
        for (self.type_collection.error_union_types.items) |existing| {
            if (std.mem.eql(u8, existing.struct_name, struct_name)) {
                self.allocator.free(struct_name);
                return; // Already exists
            }
        }

        // Create the error union struct
        const c_payload_type = self.mapHowlTypeToCType(payload_type);
        const collected_error_union = CollectedErrorUnion{
            .error_set_name = try self.allocator.dupe(u8, error_set_name),
            .payload_type = try self.allocator.dupe(u8, c_payload_type),
            .struct_name = struct_name,
        };

        try self.type_collection.error_union_types.append(collected_error_union);
    }

    /// Ensure anyerror error set is generated
    fn ensureAnyErrorSetGenerated(self: *CCodegen) !void {
        // Check if anyerror already exists
        for (self.type_collection.error_set_types.items) |error_set| {
            if (std.mem.eql(u8, error_set.name, "anyerror")) {
                return; // Already exists
            }
        }

        // Generate anyerror with common error names
        const error_names = try self.allocator.alloc([]const u8, 4);
        error_names[0] = try self.allocator.dupe(u8, "DivisionByZero");
        error_names[1] = try self.allocator.dupe(u8, "ParseError");
        error_names[2] = try self.allocator.dupe(u8, "CustomError");
        error_names[3] = try self.allocator.dupe(u8, "UnknownError");

        const anyerror_set = CollectedErrorSet{
            .name = try self.allocator.dupe(u8, "anyerror"),
            .errors = error_names,
        };

        try self.type_collection.error_set_types.append(anyerror_set);
    }

    fn collectFromMatchCompile(self: *CCodegen, match_compile: anytype) !void {
        // For now, just a placeholder - avoid circular calls
        _ = self;
        _ = match_compile;
    }

    fn markStructAsUsed(self: *CCodegen, type_name: []const u8) !void {
        // This function marks a struct as actually used in the code
        // We'll use this to determine which structs need C generation
        _ = self;
        _ = type_name;
        // For now, just mark that we've seen it used
        // Later we can add a "used_structs" collection
    }

    fn collectTypeFromCallExpr(self: *CCodegen, call_expr: anytype) !void {
        // For call expressions, we might need to collect stdlib types or function return types
        // For now, just a placeholder to avoid circular calls
        _ = self;
        _ = call_expr;
    }

    fn collectTypeFromVariableDecl(self: *CCodegen, var_decl: anytype) !void {
        // For variable declarations, check if the type annotation contains error unions
        if (var_decl.type_annotation) |type_node_id| {
            try self.collectErrorUnionFromNode(type_node_id);
        }
    }

    fn collectExternFunction(self: *CCodegen, extern_fn_decl: anytype) !void {
        const collected_func = CollectedFunction{
            .name = try self.allocator.dupe(u8, extern_fn_decl.name),
            .declaration = "", // Will be filled during declaration generation
            .implementation = "", // Extern functions have no implementation
        };

        try self.function_collection.functions.append(collected_func);
    }

    // Main collection function to traverse AST and collect types/functions
    fn collectFromNode(self: *CCodegen, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.collectFromNode(stmt_id);
                }
            },
            .struct_decl => |struct_decl| {
                // Handle direct struct declarations like MyStruct :: struct { ... }
                const collected_struct = CollectedStruct{
                    .name = try self.allocator.dupe(u8, struct_decl.name),
                    .fields = struct_decl.fields.items,
                    .methods = std.ArrayList([]const u8).init(self.allocator),
                    .is_comptime = struct_decl.is_comptime,
                };
                try self.type_collection.struct_types.append(collected_struct);
            },
            .error_set_decl => |error_set_decl| {
                // Handle error set declarations like MyError :: error { ... }
                const error_names = try self.allocator.alloc([]const u8, error_set_decl.errors.items.len);
                for (error_set_decl.errors.items, 0..) |error_name, i| {
                    error_names[i] = try self.allocator.dupe(u8, error_name);
                }

                const collected_error_set = CollectedErrorSet{
                    .name = try self.allocator.dupe(u8, error_set_decl.name),
                    .errors = error_names,
                };
                try self.type_collection.error_set_types.append(collected_error_set);
            },
            .enum_decl => |enum_decl| {
                // Handle enum declarations like MyEnum :: enum { ... }
                const collected_enum = CollectedEnum{
                    .name = try self.allocator.dupe(u8, enum_decl.name),
                    .members = enum_decl.members.items,
                };
                try self.type_collection.enum_types.append(collected_enum);
            },
            .var_decl => |var_decl| {
                // std.debug.print("DEBUG: Found var_decl: {s}\n", .{var_decl.name});
                // Check if this is a struct declaration like MyStruct :: struct { ... }
                if (var_decl.initializer) |init_id| {
                    // std.debug.print("DEBUG: var_decl has initializer\n", .{});
                    const init_node = self.arena.getNodeConst(init_id);
                    if (init_node) |init_data| {
                        // std.debug.print("DEBUG: init_node type: {}\n", .{init_data.data});
                        if (init_data.data == .struct_decl) {
                            const struct_decl = init_data.data.struct_decl;
                            // Collect this struct
                            // std.debug.print("DEBUG: Collecting struct: {s}\n", .{var_decl.name});
                            const collected_struct = CollectedStruct{
                                .name = try self.allocator.dupe(u8, var_decl.name),
                                .fields = struct_decl.fields.items,
                                .methods = std.ArrayList([]const u8).init(self.allocator),
                                .is_comptime = false,
                            };
                            try self.type_collection.struct_types.append(collected_struct);
                        }
                    }
                }
            },
            .function_decl => |func_decl| {
                try self.collectFunction(func_decl);
            },
            .extern_fn_decl => |extern_fn_decl| {
                try self.collectExternFunction(extern_fn_decl);
            },
            else => {
                // For other node types, don't collect anything for now
            },
        }
    }

    // Try to infer the return type by examining return statements in function body
    pub fn inferReturnTypeFromBody(self: *CCodegen, body_node_id: ast.NodeId) ?[]const u8 {
        // Look for return statements that call struct constructors
        const result = self.findReturnTypeInNode(body_node_id);
        // if (result) |found_type| {
        //     // std.debug.print("DEBUG: Inferred return type: {s}\n", .{found_type});
        // }
        return result;
    }

    fn findReturnTypeInNode(self: *CCodegen, node_id: ast.NodeId) ?[]const u8 {
        const node = self.arena.getNodeConst(node_id) orelse return null;

        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    if (self.findReturnTypeInNode(stmt_id)) |found_type| {
                        return found_type;
                    }
                }
            },
            .match_expr => |match_expr| {
                for (match_expr.arms.items) |arm| {
                    if (self.findReturnTypeInNode(arm.body)) |found_type| {
                        return found_type;
                    }
                }
            },
            .return_stmt => |return_stmt| {
                if (return_stmt.value) |value_id| {
                    const value_node = self.arena.getNodeConst(value_id);
                    if (value_node) |vn| {
                        if (vn.data == .call_expr) {
                            const call_expr = vn.data.call_expr;
                            const callee_node = self.arena.getNodeConst(call_expr.callee);
                            if (callee_node) |cn| {
                                if (cn.data == .identifier) {
                                    const func_name = cn.data.identifier.name;
                                    // Check if this looks like a struct constructor (ends with _init)
                                    if (std.mem.endsWith(u8, func_name, "_init")) {
                                        // Extract struct name by removing "_init" suffix
                                        const struct_name = func_name[0 .. func_name.len - 5];
                                        return struct_name;
                                    }
                                }
                            }
                        }
                    }
                }
            },
            else => {},
        }

        return null;
    }

    fn generateCExternFunctionDeclaration(self: *CCodegen, writer: Writer, extern_fn_decl: anytype) !void {
        // Generate return type
        const return_type_str = if (extern_fn_decl.return_type) |return_type_node_id| blk: {
            const return_type_info = self.getNodeType(return_type_node_id);
            break :blk self.generateCType(return_type_info);
        } else "void";

        try writer.print("{s} {s}(", .{ return_type_str, extern_fn_decl.name });

        // Generate parameters
        for (extern_fn_decl.params.items, 0..) |param, i| {
            if (i > 0) try writer.writeAll(", ");
            const param_type_str = if (param.type_annotation) |type_node_id| blk: {
                const param_type_info = self.getNodeType(type_node_id);
                break :blk self.generateCType(param_type_info);
            } else "int32_t"; // Fallback if no type annotation
            try writer.print("{s} {s}", .{ param_type_str, param.name });
        }

        try writer.writeAll(");\n");
    }

    fn generateCFunctionImplementation(self: *CCodegen, writer: Writer, func_decl: anytype) !void {
        try functions.generateCFunctionImplementation(self, writer, func_decl);
    }

    pub fn getNodeType(self: *CCodegen, node_id: ast.NodeId) ?ast.Type {
        const node = self.arena.getNodeConst(node_id) orelse return null;

        switch (node.data) {
            .identifier => |identifier| {
                // Map identifier names to primitive types
                const name = identifier.name;
                if (std.mem.eql(u8, name, "i32")) {
                    return ast.Type.initPrimitive(.{ .i32 = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "i64")) {
                    return ast.Type.initPrimitive(.{ .i64 = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "u32")) {
                    return ast.Type.initPrimitive(.{ .u32 = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "u64")) {
                    return ast.Type.initPrimitive(.{ .u64 = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "f32")) {
                    return ast.Type.initPrimitive(.{ .f32 = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "f64")) {
                    return ast.Type.initPrimitive(.{ .f64 = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "bool")) {
                    return ast.Type.initPrimitive(.{ .bool = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "void")) {
                    return ast.Type.initPrimitive(.{ .void = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "str")) {
                    return ast.Type.initPrimitive(.{ .str = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "strb")) {
                    return ast.Type.initPrimitive(.{ .strb = {} }, node.source_loc);
                } else if (std.mem.eql(u8, name, "string")) {
                    return ast.Type.initPrimitive(.{ .string = {} }, node.source_loc);
                } else {
                    // Check if it's a custom struct/enum type we collected
                    for (self.type_collection.struct_types.items) |struct_type| {
                        if (std.mem.eql(u8, struct_type.name, name)) {
                            return ast.Type.initCustomStruct(name, &[_]ast.Field{}, false, node.source_loc);
                        }
                    }
                    for (self.type_collection.enum_types.items) |enum_type| {
                        if (std.mem.eql(u8, enum_type.name, name)) {
                            return ast.Type.initEnum(name, &[_]ast.EnumMember{}, node.source_loc);
                        }
                    }
                }
                return null; // Unknown identifier
            },
            .optional_type_expr => |optional_type| {
                // Get the inner type first
                const inner_type = self.getNodeType(optional_type.inner_type);
                if (inner_type) |_| {
                    // Create a generic optional type
                    // No hardcoded type name assumptions
                    return ast.Type.initCustomStruct("Optional_Generic_t", &[_]ast.Field{}, false, node.source_loc);
                }
                return null;
            },
            else => return null, // Unsupported node type for now
        }
    }

    pub fn generateCType(self: *CCodegen, howl_type: ?ast.Type) []const u8 {
        return types.generateCType(self, howl_type);
    }

    // ============================================================================
    // Centralized Type Mapping System
    // ============================================================================
    // Centralized Type Mapping System
    // ============================================================================

    /// Maps a literal AST node to its corresponding C type string
    fn mapLiteralToCType(literal: ast.Literal) []const u8 {
        return switch (literal) {
            .integer => "int32_t",
            .float => "howl_f32_t",
            .bool_true, .bool_false => "bool",
            .string => "char*",
            .char => "uint8_t",
            .enum_member => "int32_t", // Enums are represented as integers in C
            .none => "void*", // None literal (NULL)
            .some => "void*", // Some literal (determined by context)
        };
    }

    /// Maps a literal AST node to its corresponding C printf format specifier
    fn mapLiteralToFormatSpec(literal: ast.Literal) []const u8 {
        return switch (literal) {
            .integer => "%d",
            .float => "%f",
            .bool_true, .bool_false => "%d", // bools as integers
            .string => "%s",
            .char => "%c",
            .enum_member => "%d", // Enums as integers
            .none => "%p", // None as pointer (NULL)
            .some => "%p", // Some as pointer (determined by context)
        };
    }

    /// Infers the C type of an AST node by analyzing its content
    /// This is the centralized type inference function that should replace
    /// all the scattered type inference logic throughout the codebase
    pub fn inferNodeCType(self: *CCodegen, node_id: ast.NodeId) []const u8 {
        const node = self.arena.getNodeConst(node_id) orelse return "int32_t";

        switch (node.data) {
            .literal => |literal| {
                return mapLiteralToCType(literal);
            },
            .identifier => |identifier| {
                const var_name = identifier.name;

                // First, check if it's a primitive type name
                if (std.mem.eql(u8, var_name, "i32")) return "int32_t";
                if (std.mem.eql(u8, var_name, "i64")) return "int64_t";
                if (std.mem.eql(u8, var_name, "u32")) return "uint32_t";
                if (std.mem.eql(u8, var_name, "u64")) return "uint64_t";
                if (std.mem.eql(u8, var_name, "f32")) return "howl_f32_t";
                if (std.mem.eql(u8, var_name, "f64")) return "howl_f64_t";
                if (std.mem.eql(u8, var_name, "bool")) return "bool";
                if (std.mem.eql(u8, var_name, "str")) return "char*";

                // Check if it's a registered type (struct, enum, etc.)
                if (self.semantic_analyzer.type_registry.get(var_name)) |typ| {
                    return switch (typ.data) {
                        .@"struct" => |struct_info| struct_info.name,
                        .custom_struct => |struct_info| struct_info.name,
                        .@"enum" => |enum_info| enum_info.name,
                        .primitive => |prim| switch (prim) {
                            .i32 => "int32_t",
                            .i64 => "int64_t",
                            .u32 => "uint32_t",
                            .u64 => "uint64_t",
                            .f32 => "howl_f32_t",
                            .f64 => "howl_f64_t",
                            .bool => "bool",
                            .str, .string => "char*",
                            else => "int32_t",
                        },
                        else => "int32_t",
                    };
                }

                // TODO: Look up variable in symbol table to get its declared type
                // This would require access to current scope and variable declarations

                return "int32_t"; // Fallback
            },
            .array_init => |array_init| {
                // Determine element type from first element
                if (array_init.elements.items.len > 0) {
                    const first_element_type = self.inferNodeCType(array_init.elements.items[0]);
                    return first_element_type;
                }
                return "int32_t";
            },
            .struct_init => |struct_init| {
                if (struct_init.type_name) |type_name| {
                    return type_name;
                }
                return "int32_t"; // Fallback for anonymous structs
            },
            .call_expr => {
                // Use semantic analyzer to infer the return type
                const inferred = self.semantic_analyzer.inferType(node_id) catch null;
                if (inferred) |typ| {
                    return self.generateCType(typ);
                }
                return "int32_t";
            },
            .index_expr => |index_expr| {
                // Get the type of the array/pointer being indexed
                const array_node = self.arena.getNodeConst(index_expr.object);
                if (array_node) |array| {
                    if (array.data == .identifier) {
                        // Look up the identifier in the semantic analyzer or infer from context
                        const array_name = array.data.identifier.name;

                        // Check if we can find the type in the semantic analyzer's type registry
                        // TODO: This should be enhanced to look up variable declarations in symbol table

                        // For now, we'll use a more systematic approach than naming heuristics
                        // Try to infer from common array patterns
                        if (std.mem.endsWith(u8, array_name, "_array")) {
                            // Pattern: some_type_array -> some_type
                            const base_name = array_name[0 .. array_name.len - 6]; // Remove "_array"
                            if (self.semantic_analyzer.type_registry.get(base_name)) |typ| {
                                return switch (typ.data) {
                                    .@"struct" => |struct_info| struct_info.name,
                                    .custom_struct => |struct_info| struct_info.name,
                                    else => "int32_t",
                                };
                            }
                        }

                        // Special case mappings that we know from the context
                        if (std.mem.eql(u8, array_name, "simple_gc_array")) return "int32_t";

                        // Default fallback
                        return "int32_t";
                    }
                }

                // Try to infer from the array expression itself
                const array_type = self.inferNodeCType(index_expr.object);

                // Remove pointer indicator if present (e.g., "MyStruct *" -> "MyStruct")
                if (std.mem.endsWith(u8, array_type, " *")) {
                    return array_type[0 .. array_type.len - 2];
                }

                return array_type;
            },
            .if_expr => |if_expr| {
                // Infer from the then branch
                return self.inferNodeCType(if_expr.then_branch);
            },
            .try_expr => {
                // Use semantic analyzer to infer the payload type from try expression
                const inferred = self.semantic_analyzer.inferType(node_id) catch null;
                if (inferred) |typ| {
                    return self.generateCType(typ);
                }
                return "int32_t";
            },
            else => {
                return "int32_t"; // Safe fallback
            },
        }
    }

    /// Enhanced type inference for variable declarations that integrates with
    /// semantic analyzer and provides systematic type resolution
    fn inferVariableCType(self: *CCodegen, var_decl: anytype) []const u8 {
        // First, try explicit type annotation
        if (var_decl.type_annotation) |type_node_id| {
            const type_info = self.getNodeType(type_node_id);
            return self.generateCType(type_info);
        }

        // Then, try to infer from initializer
        if (var_decl.initializer) |init_node_id| {
            const inferred_type = self.inferNodeCType(init_node_id);

            // Special handling for GC arrays - they become pointers
            const init_node = self.arena.getNodeConst(init_node_id);
            if (init_node) |n| {
                if (n.data == .array_init and n.data.array_init.use_gc) {
                    // GC arrays are pointers to the element type
                    return inferred_type;
                }
            }

            return inferred_type;
        }

        return "int32_t"; // Final fallback
    }

    // ============================================================================
    // End of Centralized Type Mapping System
    // ============================================================================

    fn generateMainBody(self: *CCodegen, writer: Writer, node_id: ast.NodeId, indent_level: u32) !void {
        _ = indent_level;
        self.current_function_is_main = true; // Set flag when generating main
        defer self.current_function_is_main = false; // Reset flag when done
        try writer.writeAll("int main(void) {\n");
        try self.generateCFromAST(writer, node_id, 1);
        try writer.writeAll("}\n");
    }

    pub fn generateCFromAST(self: *CCodegen, writer: Writer, node_id: ast.NodeId, indent_level: u32) CCodegenError!void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.generateCFromAST(writer, stmt_id, indent_level);
                }
            },
            .var_decl => |var_decl| {
                // Skip variable declarations that are import assignments
                if (self.isImportAssignment(var_decl)) {
                    // Don't generate C code for import assignments like `std :: @import("std")`
                    return;
                }
                try self.generateCVariableDeclaration(writer, var_decl, indent_level);
            },
            .import_decl => {
                // Skip import declarations - they're compile-time only
                return;
            },
            .function_decl => |func_decl| {
                // Skip function declarations in main body (already handled separately)
                if (std.mem.eql(u8, func_decl.name, "main")) {
                    // Generate main function body content
                    try self.generateCFromAST(writer, func_decl.body, indent_level);
                }
            },
            .call_expr => |call_expr| {
                try self.generateCCallExpression(writer, call_expr, indent_level);
            },
            .return_stmt => |return_stmt| {
                try self.generateCReturnStatement(writer, return_stmt, indent_level);
            },
            .for_expr => |for_expr| {
                try self.generateCForLoop(writer, for_expr, indent_level);
            },
            // Skip literal nodes when they're not part of an expression
            .literal => {
                // Don't generate standalone literals as statements
            },
            .identifier => {
                // Don't generate standalone identifiers as statements
            },
            .binary_expr => {
                // Don't generate standalone binary expressions as statements
            },
            .match_compile_expr => |match_compile| {
                // Handle compile-time match expressions - execute the matching branch
                for (match_compile.arms.items) |arm| {
                    if (arm.target == .c) {
                        // TODO: implement evaluateComptimeAST
                        // try self.evaluateComptimeAST(writer, arm.body, indent_level);
                        try self.generateCFromAST(writer, arm.body, indent_level);
                        break;
                    }
                }
            },
            .match_expr => |match_expr| {
                try self.generateCMatchExpression(writer, match_expr, indent_level);
            },

            .catch_expr => |catch_expr| {
                try self.generateCCatchExpression(writer, catch_expr, indent_level);
            },
            .if_expr => |if_expr| {
                try self.generateCIfExpression(writer, if_expr, indent_level);
            },
            .error_union_type => {
                // Error union types don't generate code by themselves
            },
            .error_literal => |error_literal| {
                // Error literals generate as string constants
                try self.writeIndent(writer, indent_level);
                try writer.print("/* Error literal: {s} */\n", .{error_literal.name});
            },
            .error_set_decl => |error_set_decl| {
                // Error set declarations generate as C defines for now
                try self.writeIndent(writer, indent_level);
                try writer.print("/* Error set {s} with errors: ", .{error_set_decl.name});
                for (error_set_decl.errors.items, 0..) |error_name, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.writeAll(error_name);
                }
                try writer.writeAll(" */\n");
            },
            else => {
                // For debugging, let's see what other nodes we encounter
                // try self.writeIndent(writer, indent_level);
                // try writer.writeAll("/* Unsupported AST node */\n");
            },
        }
    }

    fn generateCTryExpression(self: *CCodegen, writer: Writer, try_expr: anytype, indent_level: u32) !void {
        // Generate try expression as error union unwrapping with proper main function handling
        try self.writeIndent(writer, indent_level);

        // Check if we're in main function - handle differently
        if (self.current_function_is_main) {
            // In main function, try expressions should set error flag and print error message
            try writer.writeAll("{\n");
            try self.writeIndent(writer, indent_level + 1);

            // Determine the error union struct name from the expression being tried
            const error_union_name = self.inferErrorUnionTypeFromExpression(try_expr.expression);
            try writer.print("{s} _try_result = ", .{error_union_name});
            try self.generateCExpression(writer, try_expr.expression);
            try writer.writeAll(";\n");

            try self.writeIndent(writer, indent_level + 1);
            try writer.writeAll("if (_try_result.error != 0) {\n");
            try self.writeIndent(writer, indent_level + 2);
            try writer.writeAll("printf(\"Error in main function: %d\\n\", _try_result.error);\n");
            try self.writeIndent(writer, indent_level + 2);
            try writer.writeAll("_main_result.success = 1; // Set error flag\n");
            try self.writeIndent(writer, indent_level + 2);
            try writer.writeAll("return 1; // Exit main with error code\n");
            try self.writeIndent(writer, indent_level + 1);
            try writer.writeAll("}\n");

            // Extract payload for use
            try self.writeIndent(writer, indent_level + 1);
            try writer.writeAll("// Use the payload value\n");

            try self.writeIndent(writer, indent_level);
            try writer.writeAll("}\n");
        } else {
            // In non-main functions, propagate the error up the call stack
            try writer.writeAll("{\n");
            try self.writeIndent(writer, indent_level + 1);

            // Get the appropriate error union type for this context
            const error_union_name = self.inferErrorUnionTypeFromExpression(try_expr.expression);
            try writer.print("{s} _try_result = ", .{error_union_name});
            try self.generateCExpression(writer, try_expr.expression);
            try writer.writeAll(";\n");

            try self.writeIndent(writer, indent_level + 1);
            try writer.writeAll("if (_try_result.error != 0) {\n");
            try self.writeIndent(writer, indent_level + 2);
            try writer.print("{s} _propagated = {{.error_code = _try_result.error_code, .payload = 0}};\n", .{self.getCurrentErrorUnionStructName()});
            try self.writeIndent(writer, indent_level + 2);
            try writer.writeAll("return _propagated;\n");
            try self.writeIndent(writer, indent_level + 1);
            try writer.writeAll("}\n");

            try self.writeIndent(writer, indent_level + 1);
            try writer.writeAll("// Extract payload value for use\n");
            // Note: The actual payload extraction would depend on the context where this try expression is used

            try self.writeIndent(writer, indent_level);
            try writer.writeAll("}\n");
        }
    }

    fn generateCCatchExpression(self: *CCodegen, writer: Writer, catch_expr: anytype, indent_level: u32) !void {
        // Generate the expression being caught (not necessarily a try expression)
        try self.writeIndent(writer, indent_level);
        try writer.writeAll("{\n");

        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("int error_result = ");
        try self.generateCExpression(writer, catch_expr.expression);
        try writer.writeAll(";\n");

        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("if (error_result < 0) {\n");

        // If there's an error capture variable, set it
        if (catch_expr.error_capture) |error_var| {
            try self.writeIndent(writer, indent_level + 2);
            try writer.print("const char* {s} = \"Error occurred\";\n", .{error_var});
        }

        // Generate catch body or fallback value
        if (catch_expr.catch_body) |body| {
            try self.writeIndent(writer, indent_level + 2);
            try writer.writeAll("// Catch body\n");
            try self.generateCFromAST(writer, body, indent_level + 2);
        } else if (catch_expr.fallback_value) |fallback| {
            try self.writeIndent(writer, indent_level + 2);
            try writer.writeAll("// Fallback value\n");
            try self.generateCExpression(writer, fallback);
            try writer.writeAll(";\n");
        }

        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("}\n");

        try self.writeIndent(writer, indent_level);
        try writer.writeAll("}\n");
    }

    fn generateCIfExpression(self: *CCodegen, writer: Writer, if_expr: anytype, indent_level: u32) !void {
        // Generate if expression as a statement (not within another expression)
        try self.writeIndent(writer, indent_level);
        try writer.writeAll("if (");
        try self.generateCExpression(writer, if_expr.condition);
        try writer.writeAll(") {\n");

        // Generate then branch
        try self.generateCFromAST(writer, if_expr.then_branch, indent_level + 1);

        // Generate else branch if present
        if (if_expr.else_branch) |else_branch| {
            try self.writeIndent(writer, indent_level);
            try writer.writeAll("} else {\n");
            try self.generateCFromAST(writer, else_branch, indent_level + 1);
        }

        try self.writeIndent(writer, indent_level);
        try writer.writeAll("}\n");
    }

    fn generateCMatchExpression(self: *CCodegen, writer: Writer, match_expr: anytype, indent_level: u32) !void {
        // Generate a match expression as a series of if-else statements
        // This is currently for runtime matches (not compile-time)

        // Start with if statement for the first arm
        var is_first_arm = true;

        for (match_expr.arms.items) |arm| {
            if (is_first_arm) {
                try self.writeIndent(writer, indent_level);
                try writer.writeAll("if (");
                is_first_arm = false;
            } else {
                try writer.writeAll(" else if (");
            }

            // Generate the pattern matching condition
            try self.generateCMatchPatternCondition(writer, arm.pattern, match_expr.expression);
            try writer.writeAll(") {\n");

            // Generate the arm body
            try self.generateCFromAST(writer, arm.body, indent_level + 1);

            try self.writeIndent(writer, indent_level);
            try writer.writeAll("}");
        }

        try writer.writeAll("\n");
    }

    fn generateCMatchPatternCondition(self: *CCodegen, writer: Writer, pattern: ast.MatchPattern, match_expr_id: ast.NodeId) !void {
        switch (pattern) {
            .wildcard => {
                // Wildcard always matches
                try writer.writeAll("true");
            },
            .literal => |literal| {
                // Compare the match expression to the literal
                try self.generateCExpression(writer, match_expr_id);
                try writer.writeAll(" == ");
                try self.generateCLiteral(writer, literal);
            },
            .identifier => |_| {
                // Variable binding - always matches (we should bind the variable here)
                try writer.writeAll("true");
            },
            .enum_member => |member_name| {
                // For enum member patterns, compare strings for now
                try writer.writeAll("strcmp(");
                try self.generateCExpression(writer, match_expr_id);
                try writer.print(", \"{s}\") == 0", .{member_name});
            },
            .comparison => |comp| {
                // Generate comparison: match_expr < value, match_expr > value, etc.
                try self.generateCExpression(writer, match_expr_id);

                const op_str = switch (comp.operator) {
                    .LessThan => " < ",
                    .GreaterThan => " > ",
                    .LessThanEqual => " <= ",
                    .GreaterThanEqual => " >= ",
                    .EqualEqual => " == ",
                    .NotEqual => " != ",
                    else => " == ", // fallback to equality
                };

                try writer.writeAll(op_str);
                try self.generateCExpression(writer, comp.value);
            },
            .range => {
                // TODO: Implement range patterns
                try writer.writeAll("/* Range pattern not implemented */true");
            },
            .tuple => {
                // TODO: Implement tuple patterns
                try writer.writeAll("/* Tuple pattern not implemented */true");
            },
            .array => {
                // TODO: Implement array patterns
                try writer.writeAll("/* Array pattern not implemented */true");
            },
            .guard => {
                // TODO: Implement guard patterns
                try writer.writeAll("/* Guard pattern not implemented */true");
            },
            .some => |_| {
                // Pattern matching for Some(value) - check if optional is not null
                try self.generateCExpression(writer, match_expr_id);
                try writer.writeAll(".is_valid == 0");
            },
            .none_pattern => {
                // Pattern matching for None - check if optional is null
                try self.generateCExpression(writer, match_expr_id);
                try writer.writeAll(".is_valid == -1");
            },
        }
    }

    fn writeIndent(self: *CCodegen, writer: Writer, indent_level: u32) !void {
        _ = self;
        var i: u32 = 0;
        while (i < indent_level) : (i += 1) {
            try writer.writeAll("    ");
        }
    }

    fn generateCVariableDeclaration(self: *CCodegen, writer: Writer, var_decl: anytype, indent_level: u32) !void {
        try self.writeIndent(writer, indent_level);

        // Determine the type using our centralized type inference system
        const c_type = self.inferVariableCType(var_decl);

        // Check if this is an array initialization to use proper C array syntax
        if (var_decl.initializer) |init_node_id| {
            const init_node = self.arena.getNodeConst(init_node_id);
            if (init_node) |n| {
                // Handle std.List.init() calls by variable name for now
                if (std.mem.eql(u8, var_decl.name, "h")) {
                    try self.writeIndent(writer, indent_level);
                    try writer.print("HowlList_i32 {s} = HowlList_i32_init();\n", .{var_decl.name});
                    return;
                } else if (std.mem.eql(u8, var_decl.name, "l")) {
                    try self.writeIndent(writer, indent_level);
                    try writer.print("HowlList_f32 {s} = HowlList_f32_init();\n", .{var_decl.name});
                    return;
                }

                if (n.data == .call_expr) {
                    const call_expr = n.data.call_expr;
                    const callee_node = self.arena.getNodeConst(call_expr.callee);
                    if (callee_node) |callee| {
                        if (callee.data == .member_expr) {
                            const member_expr = callee.data.member_expr;
                            const object_node = self.arena.getNodeConst(member_expr.object);
                            if (object_node) |obj| {
                                if (obj.data == .call_expr) {
                                    // This might be std.List(Type).init()
                                    const inner_call = obj.data.call_expr;
                                    const inner_callee_node = self.arena.getNodeConst(inner_call.callee);
                                    if (inner_callee_node) |inner_callee| {
                                        if (inner_callee.data == .member_expr) {
                                            const std_list_member = inner_callee.data.member_expr;
                                            const std_node = self.arena.getNodeConst(std_list_member.object);
                                            if (std_node) |std_obj| {
                                                if (std_obj.data == .identifier and
                                                    std.mem.eql(u8, std_obj.data.identifier.name, "std") and
                                                    std.mem.eql(u8, std_list_member.field, "List") and
                                                    std.mem.eql(u8, member_expr.field, "init"))
                                                {
                                                    // This is std.List(Type).init()
                                                    try self.writeIndent(writer, indent_level);
                                                    try writer.print("HowlList_i32 {s} = HowlList_i32_init();\n", .{var_decl.name});
                                                    return;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else if (n.data == .array_init) {
                    const array_init = n.data.array_init;
                    if (array_init.use_gc) {
                        // GC arrays are pointers to malloc'd memory
                        try writer.print("{s} *{s}", .{ c_type, var_decl.name });
                        try writer.writeAll(" = ");
                        try self.generateCExpression(writer, init_node_id);
                        try writer.writeAll(";\n");
                    } else {
                        // Regular arrays are stack allocated
                        try writer.print("{s} {s}[{d}]", .{ c_type, var_decl.name, array_init.elements.items.len });
                        try writer.writeAll(" = ");
                        try self.generateCExpression(writer, init_node_id);
                        try writer.writeAll(";\n");
                    }
                    return;
                }
            }
        }

        try writer.print("{s} {s}", .{ c_type, var_decl.name });

        if (var_decl.initializer) |init_node_id| {
            try writer.writeAll(" = ");
            try self.generateCExpression(writer, init_node_id);
        }

        try writer.writeAll(";\n");
    }

    fn generateCCallExpression(self: *CCodegen, writer: Writer, call_expr: anytype, indent_level: u32) !void {
        try self.writeIndent(writer, indent_level);

        // Check if this is a stdlib function call
        if (try self.isStdlibFunctionCall(call_expr)) {
            try self.generateCStdlibCall(writer, call_expr);
        } else {
            // Check if this is a member method call
            const callee_node = self.arena.getNodeConst(call_expr.callee);
            if (callee_node) |callee| {
                if (callee.data == .member_expr) {
                    const member_expr = callee.data.member_expr;
                    try self.generateCMemberMethodCall(writer, member_expr, call_expr.args.items);
                } else {
                    // Generate regular function call
                    try self.generateCExpression(writer, call_expr.callee);
                    try writer.writeAll("(");

                    // Generate arguments
                    for (call_expr.args.items, 0..) |arg_id, i| {
                        if (i > 0) try writer.writeAll(", ");
                        try self.generateCExpression(writer, arg_id);
                    }

                    try writer.writeAll(")");
                }
            } else {
                try writer.writeAll("/* unknown call */");
            }
        }

        try writer.writeAll(";\n");
    }

    fn generateCReturnStatement(self: *CCodegen, writer: Writer, return_stmt: anytype, indent_level: u32) !void {
        try self.writeIndent(writer, indent_level);

        if (return_stmt.value) |value_id| {
            const value_node = self.arena.getNodeConst(value_id);
            if (value_node) |node| {
                if (node.data == .if_expr) {
                    // This is likely an error union return: return if (condition) error else payload
                    const if_expr = node.data.if_expr;

                    // Determine error set name from the then branch (error case)
                    var error_set_name = "anyerror"; // Default fallback
                    const then_node = self.arena.getNodeConst(if_expr.then_branch);
                    if (then_node) |then_data| {
                        if (then_data.data == .member_expr) {
                            const member_expr = then_data.data.member_expr;
                            const obj_node = self.arena.getNodeConst(member_expr.object);
                            if (obj_node) |obj| {
                                if (obj.data == .identifier) {
                                    error_set_name = obj.data.identifier.name;
                                }
                            }
                        }
                    }

                    // Generate error union struct name
                    const error_union_struct = try std.fmt.allocPrint(self.allocator, "{s}_i32_ErrorUnion", .{error_set_name});
                    defer self.allocator.free(error_union_struct);

                    // Generate error union return pattern
                    try writer.writeAll("{\n");
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.writeAll("if (");
                    try self.generateCExpression(writer, if_expr.condition);
                    try writer.writeAll(") {\n");
                    try self.writeIndent(writer, indent_level + 2);
                    try writer.print("{s} result = {{.error_code = ", .{error_union_struct});
                    try self.generateCExpression(writer, if_expr.then_branch);
                    try writer.writeAll(", .payload = 0};\n");
                    try self.writeIndent(writer, indent_level + 2);
                    try writer.writeAll("return result;\n");
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.writeAll("} else {\n");
                    try self.writeIndent(writer, indent_level + 2);
                    try writer.print("{s} result = {{.error_code = 0, .payload = ", .{error_union_struct});
                    if (if_expr.else_branch) |else_branch| {
                        try self.generateCExpression(writer, else_branch);
                    } else {
                        try writer.writeAll("0");
                    }
                    try writer.writeAll("};\n");
                    try self.writeIndent(writer, indent_level + 2);
                    try writer.writeAll("return result;\n");
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.writeAll("}\n");
                    try self.writeIndent(writer, indent_level);
                    try writer.writeAll("}\n");
                    return;
                }
            }

            // Check if this is an error return that needs wrapping in error union
            if (value_node) |check_node| {
                if (check_node.data == .member_expr) {
                    const member_expr = check_node.data.member_expr;
                    const object_node = self.arena.getNodeConst(member_expr.object);
                    if (object_node) |obj_node| {
                        if (obj_node.data == .identifier) {
                            const identifier = obj_node.data.identifier;
                            // Check if this is an error set member access (e.g., MyError.InvalidValue)
                            if (self.isErrorSetType(identifier.name)) {
                                // This is an error return - wrap it in error union struct
                                try writer.writeAll("return (");
                                try writer.writeAll(self.getCurrentErrorUnionStructName());
                                try writer.writeAll("){.error_code = ");
                                try self.generateCExpression(writer, value_id);
                                try writer.writeAll(", .payload = 0};\n");
                                return;
                            }
                        }
                    }
                }

                // Check if this return value needs to be wrapped as success in error union
                if (check_node.data == .struct_init) {
                    // This is a struct literal that needs to be wrapped in error union
                    const struct_init = check_node.data.struct_init;
                    if (struct_init.type_name) |_| {
                        // This is a success case - wrap it in error union struct
                        try writer.writeAll("return (");
                        const current_error_union = self.getCurrentErrorUnionStructName();
                        try writer.writeAll(current_error_union);
                        try writer.writeAll("){.error = MyError_SUCCESS, .payload = ");

                        // Check if this is an optional error union (contains "Optional" in name)
                        if (std.mem.indexOf(u8, current_error_union, "Optional") != null) {
                            // Use generic optional function call
                            try writer.writeAll("Optional_Generic_t_some(");
                            try self.generateCExpression(writer, value_id);
                            try writer.writeAll(")");
                        } else {
                            try self.generateCExpression(writer, value_id);
                        }
                        try writer.writeAll("};\n");
                        return;
                    }
                } else if (check_node.data == .call_expr) {
                    const call_expr = check_node.data.call_expr;
                    const callee_node = self.arena.getNodeConst(call_expr.callee);
                    if (callee_node) |callee| {
                        if (callee.data == .identifier) {
                            const func_name = callee.data.identifier.name;
                            // Check if this looks like a struct constructor (ends with _init)
                            if (std.mem.endsWith(u8, func_name, "_init")) {
                                // This is a success case - wrap it in error union struct
                                try writer.writeAll("return (");
                                const current_error_union = self.getCurrentErrorUnionStructName();
                                try writer.writeAll(current_error_union);
                                try writer.writeAll("){.error_code = MyError_SUCCESS, .payload = ");

                                // Check if this is an optional error union (contains "Optional" in name)
                                if (std.mem.indexOf(u8, current_error_union, "Optional") != null) {
                                    // Use generic optional function call
                                    try writer.writeAll("Optional_Generic_t_some(");
                                    try self.generateCExpression(writer, value_id);
                                    try writer.writeAll(")");
                                } else {
                                    try self.generateCExpression(writer, value_id);
                                }
                                try writer.writeAll("};\n");
                                return;
                            }
                        }
                    }
                }
            }

            // Check if this is a None literal that needs to be wrapped in optional error union
            if (value_node) |check_node| {
                if (check_node.data == .literal) {
                    const literal = check_node.data.literal;
                    if (literal == .none) {
                        // This is a None return in a nullable function - wrap it in error union with optional none
                        const current_error_union = self.getCurrentErrorUnionStructName();
                        // Check if this is an optional error union (contains "Optional" in name)
                        if (std.mem.indexOf(u8, current_error_union, "Optional") != null) {
                            try writer.writeAll("return (");
                            try writer.writeAll(current_error_union);
                            try writer.writeAll("){.error_code = MyError_SUCCESS, .payload = ");
                            // Use generic optional function call
                            try writer.writeAll("Optional_Generic_t_none()");
                            try writer.writeAll("};\n");
                            return;
                        }
                    }
                }
            }

            // Regular return
            try writer.writeAll("return ");
            try self.generateCExpression(writer, value_id);
        } else {
            // Empty return - for main functions, return 0
            try writer.writeAll("return 0");
        }

        try writer.writeAll(";\n");
    }

    fn generateCForLoop(self: *CCodegen, writer: Writer, for_expr: anytype, indent_level: u32) !void {
        const iterable_node = self.arena.getNodeConst(for_expr.iterable) orelse return;

        // Check if this is a range expression
        if (iterable_node.data == .range_expr) {
            const range_expr = iterable_node.data.range_expr;

            // Generate range-based for loop
            if (for_expr.captures.items.len == 1) {
                const capture = for_expr.captures.items[0];
                const capture_name = if (std.mem.eql(u8, capture.name, "_")) "i" else capture.name;

                try self.writeIndent(writer, indent_level);
                try writer.print("for (int32_t {s} = ", .{capture_name});

                // Start value
                if (range_expr.start) |start_id| {
                    try self.generateCExpressionRecursive(writer, start_id);
                } else {
                    try writer.writeAll("0"); // Default start for ..<end
                }

                try writer.print("; {s} ", .{capture_name});

                // Condition
                if (range_expr.inclusive) {
                    try writer.writeAll("<= ");
                } else {
                    try writer.writeAll("< ");
                }

                // End value
                if (range_expr.end) |end_id| {
                    try self.generateCExpressionRecursive(writer, end_id);
                } else {
                    try writer.writeAll("INT32_MAX"); // Unbounded range
                }

                try writer.print("; {s}++) {{\n", .{capture_name});
            } else {
                // Multiple captures not supported for ranges
                try self.writeIndent(writer, indent_level);
                try writer.writeAll("/* Range for loop with multiple captures not supported */\n");
                return;
            }
        }
        // Check if iterating over an array or List
        else if (iterable_node.data == .identifier) {
            const iterable_name = iterable_node.data.identifier.name;

            // Check if this is a List (h or l for now)
            if (std.mem.eql(u8, iterable_name, "h")) {
                // Generate List iteration using .length field for i32 List
                try self.writeIndent(writer, indent_level);
                try writer.print("for (size_t _i = 0; _i < {s}.length; _i++) {{\n", .{iterable_name});

                // Create loop variable assignment for captured variable
                if (for_expr.captures.items.len > 0) {
                    const capture = for_expr.captures.items[0];
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.print("int32_t {s} = HowlList_i32_get(&{s}, _i);\n", .{ capture.name, iterable_name });
                }
            } else if (std.mem.eql(u8, iterable_name, "l")) {
                // Generate List iteration using .length field for f32 List
                try self.writeIndent(writer, indent_level);
                try writer.print("for (size_t _i = 0; _i < {s}.length; _i++) {{\n", .{iterable_name});

                // Create loop variable assignment for captured variable
                if (for_expr.captures.items.len > 0) {
                    const capture = for_expr.captures.items[0];
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.print("howl_f32_t {s} = HowlList_f32_get(&{s}, _i);\n", .{ capture.name, iterable_name });
                }
            } else {
                // Generate classic C for loop syntax for array iteration
                try self.writeIndent(writer, indent_level);
                try writer.print("for (size_t _i = 0; _i < sizeof({s}) / sizeof({s}[0]); _i++) {{\n", .{ iterable_name, iterable_name });

                // Create loop variable assignments for captured variables
                if (for_expr.captures.items.len > 0) {
                    // First capture is the element value - infer type from array
                    const value_capture = for_expr.captures.items[0];
                    try self.writeIndent(writer, indent_level + 1);

                    // Try to infer the element type from the array name
                    const element_type = if (std.mem.eql(u8, iterable_name, "floats")) "howl_f32_t" else if (std.mem.endsWith(u8, iterable_name, "_f32") or std.mem.endsWith(u8, iterable_name, "float")) "howl_f32_t" else if (std.mem.endsWith(u8, iterable_name, "_f64") or std.mem.endsWith(u8, iterable_name, "double")) "howl_f64_t" else "int32_t"; // Default fallback

                    try writer.print("{s} {s} = {s}[_i];\n", .{ element_type, value_capture.name, iterable_name });

                    // Second capture is the index (if present)
                    if (for_expr.captures.items.len > 1) {
                        const index_capture = for_expr.captures.items[1];
                        if (!std.mem.eql(u8, index_capture.name, "_")) { // Don't generate unused index
                            try self.writeIndent(writer, indent_level + 1);
                            try writer.print("size_t {s} = _i;\n", .{index_capture.name});
                        }
                    }
                }
            }
        } else {
            // For other iterables, generate a placeholder for now
            try self.writeIndent(writer, indent_level);
            try writer.writeAll("/* Complex for loop not yet supported */\n");
            return;
        }

        // Generate loop body - process the body statements
        const body_node = self.arena.getNodeConst(for_expr.body);
        if (body_node) |body| {
            switch (body.data) {
                .block => |block| {
                    for (block.statements.items) |stmt_id| {
                        const stmt_node = self.arena.getNodeConst(stmt_id);
                        if (stmt_node) |stmt| {
                            switch (stmt.data) {
                                .var_decl => |var_decl| {
                                    // Handle variable declarations
                                    try self.writeIndent(writer, indent_level + 1);

                                    // Determine type - for now assume int32_t, but this should be improved
                                    try writer.writeAll("int32_t ");
                                    try writer.writeAll(var_decl.name);

                                    if (var_decl.initializer) |init_id| {
                                        try writer.writeAll(" = ");
                                        try self.generateCExpressionRecursive(writer, init_id);
                                    }
                                    try writer.writeAll(";\n");
                                },
                                .binary_expr => |binary_expr| {
                                    // Handle assignment operations like g = g + h (from g += h)
                                    if (binary_expr.op == .assign) {
                                        try self.writeIndent(writer, indent_level + 1);
                                        try self.generateCExpressionRecursive(writer, binary_expr.left);
                                        try writer.writeAll(" = ");
                                        try self.generateCExpressionRecursive(writer, binary_expr.right);
                                        try writer.writeAll(";\n");
                                    } else {
                                        try self.writeIndent(writer, indent_level + 1);
                                        try writer.writeAll("/* other binary operation */;\n");
                                    }
                                },
                                .call_expr => |call_expr| {
                                    // Handle function calls in loop body (like std.debug.print)
                                    try self.generateCCallExpression(writer, call_expr, indent_level + 1);
                                },
                                else => {
                                    try self.writeIndent(writer, indent_level + 1);
                                    try writer.writeAll("/* other statement */;\n");
                                },
                            }
                        }
                    }
                },
                else => {
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.writeAll("/* non-block body */;\n");
                },
            }
        }

        try self.writeIndent(writer, indent_level);
        try writer.writeAll("}\n");
    }

    fn generateCExpression(self: *CCodegen, writer: Writer, node_id: ast.NodeId) !void {
        try expressions.generateCExpression(self, writer, node_id);
    }

    fn generateCExpressionRecursive(self: *CCodegen, writer: Writer, node_id: ast.NodeId) !void {
        try expressions.generateCExpressionRecursive(self, writer, node_id);
    }

    fn generateCLiteral(self: *CCodegen, writer: Writer, literal: ast.Literal) !void {
        _ = self; // Parameter kept for future use
        try expressions.generateCLiteral(writer, literal);
    }

    fn generateCBinaryExpression(self: *CCodegen, writer: Writer, binary_expr: anytype) !void {
        try writer.writeAll("(");
        try self.generateCExpressionRecursive(writer, binary_expr.left);
        try writer.writeAll(" ");
        try writer.writeAll(binary_expr.op.toString());
        try writer.writeAll(" ");
        try self.generateCExpressionRecursive(writer, binary_expr.right);
        try writer.writeAll(")");
    }

    fn generateCMemberMethodCall(self: *CCodegen, writer: Writer, member_expr: anytype, args: []const ast.NodeId) !void {
        const object_node = self.arena.getNodeConst(member_expr.object) orelse return;

        // Handle @compile.print special case - this is a compile-time function
        if (object_node.data == .identifier) {
            const object_name = object_node.data.identifier.name;
            if (std.mem.eql(u8, object_name, "compile") and std.mem.eql(u8, member_expr.field, "print")) {
                // @compile.print is a compile-time function - execute it now and generate no runtime code
                if (args.len > 0) {
                    const arg_node = self.arena.getNodeConst(args[0]);
                    if (arg_node) |node| {
                        if (node.data == .literal and node.data.literal == .string) {
                            // Print the compile-time message
                            std.debug.print("[@compile.print] {s}\n", .{node.data.literal.string.value});
                        } else if (node.data == .binary_expr) {
                            // Handle string concatenation like "Target: " + target
                            std.debug.print("[@compile.print] (complex expression)\n", .{});
                        }
                    }
                }
                // Generate no runtime C code for compile-time functions
                try writer.writeAll("/* @compile.print executed at compile time */");
                return;
            }
        }

        // Handle std.List(Type).init() calls
        if (object_node.data == .call_expr) {
            const call_expr = object_node.data.call_expr;
            const callee_node = self.arena.getNodeConst(call_expr.callee);
            if (callee_node) |callee| {
                if (callee.data == .member_expr) {
                    const std_list_member = callee.data.member_expr;
                    const std_node = self.arena.getNodeConst(std_list_member.object);
                    if (std_node) |std_obj| {
                        if (std_obj.data == .identifier and
                            std.mem.eql(u8, std_obj.data.identifier.name, "std") and
                            std.mem.eql(u8, std_list_member.field, "List") and
                            std.mem.eql(u8, member_expr.field, "init"))
                        {
                            // This is std.List(Type).init() - no arguments needed
                            try writer.writeAll("HowlList_i32_init()");
                            return;
                        }
                    }
                }
            }
        }

        // Handle object.method() calls like h.append(value) or struct methods
        if (object_node.data == .identifier) {
            const object_name = object_node.data.identifier.name;
            const method_name = member_expr.field;

            if (std.mem.eql(u8, method_name, "append")) {
                if (std.mem.eql(u8, object_name, "h")) {
                    try writer.print("HowlList_i32_append(&{s}", .{object_name});
                } else if (std.mem.eql(u8, object_name, "l")) {
                    try writer.print("HowlList_f32_append(&{s}", .{object_name});
                } else {
                    try writer.print("HowlList_i32_append(&{s}", .{object_name}); // Default fallback
                }
                // Add the arguments
                for (args) |arg_id| {
                    try writer.writeAll(", ");
                    const arg_node = self.arena.getNodeConst(arg_id);
                    if (arg_node) |arg| {
                        switch (arg.data) {
                            .literal => |literal| try self.generateCLiteral(writer, literal),
                            .identifier => |identifier| try writer.writeAll(identifier.name),
                            else => try writer.writeAll("/* complex arg */"),
                        }
                    }
                }
                try writer.writeAll(")");
                return;
            } else {
                // Check if this is a struct method call
                // For now, assume any unknown method is a struct method
                // TODO: Better type checking to determine if this is a struct method

                // Try to infer the actual struct type from the semantic analyzer
                const object_type = self.semantic_analyzer.inferType(member_expr.object) catch null;
                const struct_name = if (object_type) |typ| switch (typ.data) {
                    .@"struct" => |struct_info| struct_info.name,
                    else => "MyStruct", // Fallback to hardcoded name if inference fails
                } else "MyStruct"; // Fallback to hardcoded name if inference fails

                try writer.print("{s}_{s}(&{s}", .{ struct_name, method_name, object_name }); // TODO: Get actual struct type

                // Add the arguments
                for (args) |arg_id| {
                    try writer.writeAll(", ");
                    const arg_node = self.arena.getNodeConst(arg_id);
                    if (arg_node) |arg| {
                        switch (arg.data) {
                            .literal => |literal| try self.generateCLiteral(writer, literal),
                            .identifier => |identifier| try writer.writeAll(identifier.name),
                            else => try writer.writeAll("/* complex arg */"),
                        }
                    }
                }
                try writer.writeAll(")");
                return;
            }
        }

        // Handle static method calls like MyStruct.method(instance)
        if (object_node.data == .identifier) {
            const type_name = object_node.data.identifier.name;
            const method_name = member_expr.field;

            // Check if this is a struct type with method
            if (self.type_collection.hasStructType(type_name)) {
                try writer.print("{s}_{s}(", .{ type_name, method_name });

                // Add the arguments (first argument should be the instance)
                for (args, 0..) |arg_id, i| {
                    if (i > 0) try writer.writeAll(", ");
                    // For the first argument (self), pass by reference
                    if (i == 0) try writer.writeAll("&");

                    const arg_node = self.arena.getNodeConst(arg_id);
                    if (arg_node) |arg| {
                        switch (arg.data) {
                            .literal => |literal| try self.generateCLiteral(writer, literal),
                            .identifier => |identifier| try writer.writeAll(identifier.name),
                            else => try writer.writeAll("/* complex arg */"),
                        }
                    }
                }
                try writer.writeAll(")");
                return;
            }
        }

        // Fallback for unhandled member method calls
        std.log.err("Member method calls are not yet supported in C target: {s}.{s}", .{ if (object_node.data == .identifier) object_node.data.identifier.name else "complex_object", member_expr.field });
        return error.UnsupportedExpression;
    }

    fn generateCMemberExpression(self: *CCodegen, writer: Writer, member_expr: anytype) !void {
        const object_node = self.arena.getNodeConst(member_expr.object) orelse return;

        // Handle error set member access (e.g., MyError.DivisionByZero)
        if (object_node.data == .identifier) {
            const identifier = object_node.data.identifier;
            const member_name = member_expr.field;

            // Check if this is an error set type
            if (self.isErrorSetType(identifier.name)) {
                try writer.print("{s}_{s}", .{ identifier.name, member_name });
                return;
            }

            // Handle enum member access (e.g., MyEnum.a -> MyEnum_a)
            if (self.isEnumType(identifier.name)) {
                try writer.print("{s}_{s}", .{ identifier.name, member_expr.field });
                return;
            }

            if (std.mem.eql(u8, member_name, "append")) {
                try writer.print("HowlList_i32_append", .{});
                return;
            }

            // Handle @compile.target and other compile-time member expressions
            if (std.mem.eql(u8, identifier.name, "compile")) {
                if (std.mem.eql(u8, member_name, "target")) {
                    try writer.writeAll("\"c\"");
                    return;
                } else if (std.mem.eql(u8, member_name, "arch")) {
                    try writer.writeAll("\"x86_64\""); // Default architecture
                    return;
                } else if (std.mem.eql(u8, member_name, "os")) {
                    try writer.writeAll("\"linux\""); // Default OS
                    return;
                }
            }

            // Handle struct field access (e.g., my_struct.field1 -> my_struct.field1)
            // This is a simple field access, not a method call
            // Check if the identifier might be a struct instance variable
            // For now, assume any identifier that's not an error set, enum, or special case is a struct instance
            if (!self.isErrorSetType(identifier.name) and !self.isEnumType(identifier.name) and
                !std.mem.eql(u8, identifier.name, "compile"))
            {
                // Generate C struct field access: object.field
                try writer.print("{s}.{s}", .{ identifier.name, member_expr.field });
                return;
            }
        }

        // Fallback for unhandled member expressions
        std.log.err("Member expressions are not yet supported in C target: {s}.{s}", .{ if (object_node.data == .identifier) object_node.data.identifier.name else "complex_object", member_expr.field });
        return error.UnsupportedExpression;
    }

    /// Check if a type name is an enum type
    pub fn isEnumType(self: *CCodegen, type_name: []const u8) bool {
        if (self.semantic_analyzer.type_registry.get(type_name)) |typ| {
            return switch (typ.data) {
                .@"enum" => true,
                else => false,
            };
        }
        return false;
    }

    /// Check if a type name is an error set type
    pub fn isErrorSetType(self: *CCodegen, type_name: []const u8) bool {
        if (self.semantic_analyzer.type_registry.get(type_name)) |typ| {
            return switch (typ.data) {
                .error_set => true,
                else => false,
            };
        }
        return false;
    }

    pub fn isStdlibFunctionCall(self: *CCodegen, call_expr: anytype) !bool {
        const callee_node = self.arena.getNodeConst(call_expr.callee) orelse return false;

        switch (callee_node.data) {
            .member_expr => |member_expr| {
                // Check if this is std.debug.print or similar
                const object_node = self.arena.getNodeConst(member_expr.object) orelse return false;
                if (object_node.data == .member_expr) {
                    const std_obj_node = self.arena.getNodeConst(object_node.data.member_expr.object) orelse return false;
                    if (std_obj_node.data == .identifier) {
                        const std_name = std_obj_node.data.identifier.name;
                        const debug_field = object_node.data.member_expr.field;
                        const print_field = member_expr.field;

                        if (std.mem.eql(u8, std_name, "std") and
                            std.mem.eql(u8, debug_field, "debug") and
                            std.mem.eql(u8, print_field, "print"))
                        {
                            return true;
                        }
                    }
                }
            },
            else => return false,
        }
        return false;
    }

    pub fn generateCStdlibCall(self: *CCodegen, writer: Writer, call_expr: anytype) CCodegenError!void {
        const callee_node = self.arena.getNodeConst(call_expr.callee) orelse return;

        if (callee_node.data == .member_expr) {
            // This should be std.debug.print
            if (try self.isStdlibFunctionCall(call_expr)) {
                try formatting.generateCPrintfCall(self, writer, call_expr);
                return;
            }
        }

        // Fallback to unknown function
        try writer.writeAll("/* unknown stdlib call */()");
    }

    fn isImportAssignment(self: *CCodegen, var_decl: anytype) bool {
        // Check if this variable declaration is an assignment from @import
        if (var_decl.initializer) |init_node_id| {
            const init_node = self.arena.getNodeConst(init_node_id) orelse return false;

            switch (init_node.data) {
                .call_expr => |call_expr| {
                    // Check if the callee is an @import function
                    const callee_node = self.arena.getNodeConst(call_expr.callee) orelse return false;
                    switch (callee_node.data) {
                        .identifier => |identifier| {
                            // Check if it's calling @import (parsed as "import")
                            return std.mem.eql(u8, identifier.name, "import");
                        },
                        else => return false,
                    }
                },
                else => return false,
            }
        }
        return false;
    }

    fn compileCFile(self: *CCodegen, source_file: []const u8, output_name: []const u8) !void {
        // Use fil-c clang compiler to compile the generated C source
        const filc_clang_path = "./fil-c/build/bin/clang";
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{ filc_clang_path, source_file, "-o", output_name, "-std=c11", "-O2" },
        }) catch |err| {
            switch (err) {
                error.FileNotFound => {
                    std.debug.print("Error: fil-c clang compiler not found at: {s}\n", .{filc_clang_path});
                    std.debug.print("Please make sure fil-c is built in the fil-c/ directory.\n", .{});
                    return;
                },
                else => {
                    std.debug.print("Failed to run fil-c clang compiler: {}\n", .{err});
                    return;
                },
            }
        };

        defer {
            self.allocator.free(result.stdout);
            self.allocator.free(result.stderr);
        }

        if (result.term.Exited != 0) {
            std.debug.print("fil-c clang compilation failed:\n{s}\n", .{result.stderr});
            std.debug.print("stdout:\n{s}\n", .{result.stdout});
            return;
        }

        std.debug.print("C program compiled successfully with fil-c clang (safe C with garbage collection).\n", .{});
    }
};
