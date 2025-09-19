const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");
const utils = @import("codegen_c_utils.zig");
const types = @import("codegen_c_types.zig");

const CCodegenError = utils.CCodegenError;
const Writer = utils.Writer;

pub const CollectedFunction = struct {
    name: []const u8,
    declaration: []const u8,
    implementation: []const u8,
};

pub const FunctionCollection = struct {
    functions: std.ArrayList(CollectedFunction),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) FunctionCollection {
        return FunctionCollection{
            .functions = std.ArrayList(CollectedFunction).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *FunctionCollection) void {
        // Free all allocated strings in collected functions
        for (self.functions.items) |func| {
            self.allocator.free(func.name);
            self.allocator.free(func.declaration);
            if (func.implementation.len > 0) {
                self.allocator.free(func.implementation);
            }
        }
        self.functions.deinit();
    }
};

/// Generate collected function declarations
pub fn generateCollectedFunctionDeclarations(
    type_collection: *const types.TypeCollection,
    arena: *const ast.AstArena,
    writer: Writer,
) CCodegenError!void {
    // Generate struct constructor declarations
    for (type_collection.struct_types.items) |struct_info| {
        try writer.print("{s} {s}_init(", .{ struct_info.name, struct_info.name });

        // Generate parameter list for declaration
        for (struct_info.fields, 0..) |field, i| {
            if (i > 0) try writer.writeAll(", ");

            if (field.type_annotation) |type_node_id| {
                const type_node = arena.getNodeConst(type_node_id);
                if (type_node) |node| {
                    if (node.data == .identifier) {
                        const type_name = node.data.identifier.name;
                        const c_type = utils.mapHowlTypeToCType(type_name);
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

/// Generate extern function declaration
pub fn generateCExternFunctionDeclaration(
    writer: Writer,
    extern_fn_decl: anytype,
    getNodeType: anytype,
) !void {
    // Generate return type
    const return_type_str = if (extern_fn_decl.return_type) |return_type_node_id| blk: {
        const return_type_info = getNodeType(return_type_node_id);
        break :blk types.generateCType(undefined, return_type_info);
    } else "void";

    try writer.print("{s} {s}(", .{ return_type_str, extern_fn_decl.name });

    // Generate parameters
    for (extern_fn_decl.params.items, 0..) |param, i| {
        if (i > 0) try writer.writeAll(", ");
        const param_type_str = if (param.type_annotation) |type_node_id| blk: {
            const param_type_info = getNodeType(type_node_id);
            break :blk types.generateCType(undefined, param_type_info);
        } else "int32_t"; // Fallback if no type annotation
        try writer.print("{s} {s}", .{ param_type_str, param.name });
    }

    try writer.writeAll(");\n");
}

/// Generate struct constructor implementation
pub fn generateStructConstructor(
    arena: *const ast.AstArena,
    writer: Writer,
    struct_info: types.CollectedStruct,
) CCodegenError!void {
    // Generate a constructor function: MyStruct MyStruct_init(field1, field2, ...)
    try writer.print("{s} {s}_init(", .{ struct_info.name, struct_info.name });

    // Generate parameter list
    for (struct_info.fields, 0..) |field, i| {
        if (i > 0) try writer.writeAll(", ");

        // Generate parameter type and name
        if (field.type_annotation) |type_node_id| {
            const type_node = arena.getNodeConst(type_node_id);
            if (type_node) |node| {
                if (node.data == .identifier) {
                    const type_name = node.data.identifier.name;
                    const c_type = utils.mapHowlTypeToCType(type_name);
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

    try writer.writeAll(") {\n");

    // Generate constructor body
    try writer.print("    {s} result = {{", .{struct_info.name});

    for (struct_info.fields, 0..) |field, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.print(".{s} = {s}", .{ field.name, field.name });
    }

    try writer.writeAll("};\n");
    try writer.writeAll("    return result;\n");
    try writer.writeAll("}\n\n");
}

pub fn generateErrorSetImplementation(writer: Writer, error_set_info: types.CollectedErrorSet) CCodegenError!void {
    try writer.writeAll("// ============================================================================\n");
    try writer.print("// Error Set {s} Implementation\n", .{error_set_info.name});
    try writer.writeAll("// ============================================================================\n\n");

    // Generate C enum definition for error codes
    try writer.print("typedef enum {{\n", .{});
    try writer.print("    {s}_SUCCESS = 0,\n", .{error_set_info.name});

    for (error_set_info.errors, 0..) |error_name, i| {
        try writer.print("    {s}_{s} = -{d}", .{ error_set_info.name, error_name, i + 1 });
        if (i < error_set_info.errors.len - 1) {
            try writer.writeAll(",\n");
        } else {
            try writer.writeAll("\n");
        }
    }

    try writer.print("}} {s}_t;\n\n", .{error_set_info.name});
}

pub fn generateErrorUnionImplementation(writer: Writer, error_union_info: types.CollectedErrorUnion) CCodegenError!void {
    try writer.writeAll("// ============================================================================\n");
    try writer.print("// Error Union {s} Implementation\n", .{error_union_info.struct_name});
    try writer.writeAll("// ============================================================================\n\n");

    // Generate specialized error union struct with integer error codes
    // Error codes: -1 = null/none, 0 = success, >0 = specific errors
    try writer.print("typedef struct {s} {{\n", .{error_union_info.struct_name});
    try writer.print("    int error_code;  // -1=null, 0=success, >0=error\n", .{});
    try writer.print("    {s} payload;\n", .{error_union_info.payload_type});
    try writer.print("}} {s};\n\n", .{error_union_info.struct_name});
}

/// Generate error union structs for all common Howl types
pub fn generateStandardErrorUnions(writer: Writer) CCodegenError!void {
    try writer.writeAll("// ============================================================================\n");
    try writer.writeAll("// Standard Error Union Types for All Howl Types\n");
    try writer.writeAll("// ============================================================================\n\n");

    // List of all Howl types that might be used in error unions
    const howl_types = [_]struct { howl: []const u8, c: []const u8 }{
        .{ .howl = "i8", .c = "int8_t" },
        .{ .howl = "i16", .c = "int16_t" },
        .{ .howl = "i32", .c = "int32_t" },
        .{ .howl = "i64", .c = "int64_t" },
        .{ .howl = "u8", .c = "uint8_t" },
        .{ .howl = "u16", .c = "uint16_t" },
        .{ .howl = "u32", .c = "uint32_t" },
        .{ .howl = "u64", .c = "uint64_t" },
        .{ .howl = "f32", .c = "float" },
        .{ .howl = "f64", .c = "double" },
        .{ .howl = "bool", .c = "bool" },
        .{ .howl = "str", .c = "char*" },
        .{ .howl = "void", .c = "void" },
    };

    // Generate error union for each type
    for (howl_types) |type_info| {
        try writer.print("typedef struct ErrorUnion_{s} {{\n", .{type_info.howl});
        try writer.print("    int error_code;  // -1=null, 0=success, >0=error\n", .{});
        try writer.print("    {s} payload;\n", .{type_info.c});
        try writer.print("}} ErrorUnion_{s};\n\n", .{type_info.howl});
    }
}

pub fn generateEnumImplementation(writer: Writer, enum_info: types.CollectedEnum) CCodegenError!void {
    try writer.writeAll("// ============================================================================\n");
    try writer.print("// Enum {s} Implementation\n", .{enum_info.name});
    try writer.writeAll("// ============================================================================\n\n");

    // Generate C enum definition
    try writer.print("typedef enum {{\n", .{});

    for (enum_info.members, 0..) |member, i| {
        if (member.value) |_| {
            // Member has explicit value - we'll need to evaluate it
            // For now, just use the index as a simple fallback
            try writer.print("    {s}_{s} = {d}", .{ enum_info.name, member.name, i });
        } else {
            // No explicit value, use automatic numbering
            try writer.print("    {s}_{s}", .{ enum_info.name, member.name });
        }

        if (i < enum_info.members.len - 1) {
            try writer.writeAll(",\n");
        } else {
            try writer.writeAll("\n");
        }
    }

    try writer.print("}} {s};\n\n", .{enum_info.name});
}

pub fn generateStructImplementation(
    arena: *const ast.AstArena,
    writer: Writer,
    struct_info: types.CollectedStruct,
) CCodegenError!void {
    try writer.writeAll("// ============================================================================\n");
    try writer.print("// Struct {s} Implementation\n", .{struct_info.name});
    try writer.writeAll("// ============================================================================\n\n");

    // Generate the struct definition
    try writer.print("typedef struct {s} {{\n", .{struct_info.name});

    // Generate struct fields
    for (struct_info.fields) |field| {
        try writer.writeAll("    ");

        // Generate field type
        if (field.type_annotation) |type_node_id| {
            const type_node = arena.getNodeConst(type_node_id);
            if (type_node) |node| {
                if (node.data == .identifier) {
                    const type_name = node.data.identifier.name;
                    const c_type = utils.mapHowlTypeToCType(type_name);
                    try writer.print("{s}", .{c_type});
                } else {
                    // Fallback for complex types
                    try writer.writeAll("int32_t /* complex type */");
                }
            } else {
                try writer.writeAll("int32_t /* unknown type */");
            }
        } else {
            try writer.writeAll("int32_t /* inferred type */");
        }

        try writer.print(" {s};\n", .{field.name});
    }

    try writer.print("}} {s};\n\n", .{struct_info.name});

    // Generate a constructor function
    try generateStructConstructor(arena, writer, struct_info);
}

pub fn generateOptionalImplementation(writer: Writer, optional_info: types.CollectedOptional) CCodegenError!void {
    try writer.writeAll("// ============================================================================\n");
    try writer.print("// Optional Type {s} Implementation (using -1 sentinel)\n", .{optional_info.struct_name});
    try writer.writeAll("// ============================================================================\n\n");

    // Generate C struct definition for optional using ErrorUnion pattern with -1 sentinel
    try writer.print("typedef struct {s} {{\n", .{optional_info.struct_name});
    try writer.writeAll("    int32_t is_valid; // -1 = None, 0 = Some\n");
    try writer.print("    {s} value;\n", .{optional_info.inner_type});
    try writer.print("}} {s};\n\n", .{optional_info.struct_name});

    // Generate helper functions for creating None and Some values using -1 sentinel
    try writer.print("static inline {s} {s}_none(void) {{\n", .{ optional_info.struct_name, optional_info.struct_name });
    try writer.print("    return ({s}){{.is_valid = -1, .value = {{0}}}};\n", .{optional_info.struct_name});
    try writer.writeAll("}\n\n");

    try writer.print("static inline {s} {s}_some({s} value) {{\n", .{ optional_info.struct_name, optional_info.struct_name, optional_info.inner_type });
    try writer.print("    return ({s}){{.is_valid = 0, .value = value}};\n", .{optional_info.struct_name});
    try writer.writeAll("}\n\n");
}

pub fn generateListImplementation(writer: Writer, list_type: []const u8) CCodegenError!void {
    // Implementation placeholder
    _ = writer;
    _ = list_type;
}

/// Helper function to check if function has error union syntax (!Type)
fn hasErrorUnionSyntax(func_decl: anytype) bool {
    // This would check the original source for ! prefix
    // For now, we'll look at the return type and see if it's wrapped in error union semantics
    _ = func_decl;
    return false; // TODO: Implement proper syntax checking
}

/// Helper function to sanitize type names for error union struct names
fn sanitizeTypeForName(type_str: []const u8) []const u8 {
    // Convert C type names to valid identifier parts
    if (std.mem.eql(u8, type_str, "int32_t")) return "i32";
    if (std.mem.eql(u8, type_str, "int64_t")) return "i64";
    if (std.mem.eql(u8, type_str, "uint32_t")) return "u32";
    if (std.mem.eql(u8, type_str, "uint64_t")) return "u64";
    if (std.mem.eql(u8, type_str, "float")) return "f32";
    if (std.mem.eql(u8, type_str, "double")) return "f64";
    if (std.mem.eql(u8, type_str, "bool")) return "bool";
    if (std.mem.eql(u8, type_str, "char*")) return "str";
    return type_str;
}

/// Ensure anyerror error set is generated
fn ensureAnyErrorSet(codegen: anytype) CCodegenError!void {
    // Check if anyerror is already in collected error sets
    for (codegen.type_collection.error_set_types.items) |error_set| {
        if (std.mem.eql(u8, error_set.name, "anyerror")) {
            return; // Already exists
        }
    }

    // Create anyerror error set with common error names
    const error_names = try codegen.allocator.alloc([]const u8, 4);
    error_names[0] = try codegen.allocator.dupe(u8, "DivisionByZero");
    error_names[1] = try codegen.allocator.dupe(u8, "ParseError");
    error_names[2] = try codegen.allocator.dupe(u8, "CustomError");
    error_names[3] = try codegen.allocator.dupe(u8, "UnknownError");

    const anyerror_set = types.CollectedErrorSet{
        .name = try codegen.allocator.dupe(u8, "anyerror"),
        .errors = error_names,
    };

    try codegen.type_collection.error_set_types.append(anyerror_set);
}

/// Ensure error union struct is generated
fn ensureErrorUnionStruct(codegen: anytype, error_set_name: []const u8, payload_type: []const u8, struct_name: []const u8) CCodegenError!void {
    // Check if this error union struct already exists
    for (codegen.type_collection.error_union_types.items) |error_union| {
        if (std.mem.eql(u8, error_union.struct_name, struct_name)) {
            return; // Already exists
        }
    }

    // Create the error union struct
    const c_payload_type = utils.mapHowlTypeToCType(payload_type);
    const error_union_struct = types.CollectedErrorUnion{
        .error_set_name = try codegen.allocator.dupe(u8, error_set_name),
        .payload_type = try codegen.allocator.dupe(u8, c_payload_type),
        .struct_name = try codegen.allocator.dupe(u8, struct_name),
    };

    try codegen.type_collection.error_union_types.append(error_union_struct);
}

/// Generate main C function implementation
pub fn generateCFunctionImplementation(
    codegen: anytype,
    writer: Writer,
    func_decl: anytype,
) CCodegenError!void {
    // Special handling for main function
    const is_main_function = std.mem.eql(u8, func_decl.name, "main");

    // Try to infer return type from function body if semantic analysis failed
    const inferred_return_type = codegen.inferReturnTypeFromBody(func_decl.body);

    // Generate return type and collect error union info for functions
    const return_type_str = if (is_main_function) "int" else if (inferred_return_type) |inferred| inferred else if (func_decl.return_type) |return_type_node_id| blk: {
        // Check if this is an error union type (indicated by ! prefix in Howl)
        const return_type_node = codegen.arena.getNodeConst(return_type_node_id);
        if (return_type_node) |node| {
            if (node.data == .error_union_type) {
                const error_union = node.data.error_union_type;
                if (error_union.error_set) |error_set_id| {
                    const error_set_node = codegen.arena.getNodeConst(error_set_id);
                    if (error_set_node) |es_node| {
                        if (es_node.data == .identifier) {
                            const error_set_name = es_node.data.identifier.name;

                            // Get payload type to find the correct specialized struct
                            const payload_type_info = codegen.getNodeType(error_union.payload_type);
                            const payload_type_str = codegen.generateCType(payload_type_info);

                            if (codegen.findErrorUnionStructName(error_set_name, payload_type_str)) |struct_name| {
                                codegen.current_function_error_union_name = struct_name; // Set current function's error union type
                                break :blk try codegen.allocator.dupe(u8, struct_name);
                            } else {
                                // Fallback to old naming if not found
                                break :blk try std.fmt.allocPrint(codegen.allocator, "{s}_ErrorUnion", .{error_set_name});
                            }
                        }
                    }
                }
                // Fallback for anonymous error union
                break :blk "struct { int32_t error; int32_t payload; }";
            }
            // Check if this is a type identifier that implies error union (!Type syntax)
            else if (node.data == .identifier) {
                const type_name = node.data.identifier.name;
                // If the function signature shows !Type, generate error union
                if (func_decl.is_error_union or hasErrorUnionSyntax(func_decl)) {
                    // Generate "anyerror!Type" error union for functions with ! prefix
                    const error_union_name = try std.fmt.allocPrint(codegen.allocator, "anyerror_{s}_ErrorUnion", .{sanitizeTypeForName(type_name)});

                    // Ensure we have the anyerror error set generated
                    try ensureAnyErrorSet(codegen);

                    // Generate the error union struct if it doesn't exist
                    try ensureErrorUnionStruct(codegen, "anyerror", type_name, error_union_name);

                    codegen.current_function_error_union_name = error_union_name;
                    break :blk error_union_name;
                }
                break :blk utils.mapHowlTypeToCType(type_name);
            }
        }
        const return_type_info = codegen.getNodeType(return_type_node_id);
        break :blk codegen.generateCType(return_type_info);
    } else "void";

    try writer.print("{s} {s}(", .{ return_type_str, func_decl.name });

    // Generate parameters
    for (func_decl.params.items, 0..) |param, i| {
        if (i > 0) try writer.writeAll(", ");
        const param_type_str = if (param.type_annotation) |type_node_id| blk: {
            const param_type_info = codegen.getNodeType(type_node_id);
            break :blk codegen.generateCType(param_type_info);
        } else "int32_t"; // Fallback if no type annotation
        try writer.print("{s} {s}", .{ param_type_str, param.name });
    }

    try writer.writeAll(") {\n");

    // Store previous error union name and restore it after function generation
    const prev_error_union_name = codegen.current_function_error_union_name;
    defer codegen.current_function_error_union_name = prev_error_union_name;

    // Set main function flag for special handling
    if (is_main_function) {
        codegen.current_function_is_main = true;

        // For main functions that have try expressions, we need to handle errors specially
        // Generate error handling wrapper for main function
        try writer.writeAll("    // Error handling wrapper for main function\n");
        try writer.writeAll("    struct { int32_t success; } _main_result = {0};\n");
        try writer.writeAll("    \n");
        try writer.writeAll("    // Main function body with error handling:\n");
    }
    defer if (is_main_function) {
        codegen.current_function_is_main = false;
    };

    // Generate function body
    try codegen.generateCFromAST(writer, func_decl.body, if (is_main_function) 1 else 1);

    // Add return statement for main function
    if (is_main_function) {
        try writer.writeAll("    // Return 0 for success, 1 for error\n");
        try writer.writeAll("    return _main_result.success;\n");
    }

    try writer.writeAll("}\n\n");
}
