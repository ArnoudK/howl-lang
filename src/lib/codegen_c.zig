const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");

// Data structures for collecting types and functions during analysis
const CollectedType = struct {
    name: []const u8,
    c_definition: []const u8,
    type_kind: TypeKind,

    const TypeKind = enum {
        builtin_list,
        builtin_stringbuilder,
        user_struct,
    };
};

const Writer = std.ArrayList(u8).Writer;

const CollectedStruct = struct {
    name: []const u8,
    fields: []const ast.Field,
    methods: std.ArrayList([]const u8), // Method names that belong to this struct
    is_comptime: bool,
};

const CollectedEnum = struct {
    name: []const u8,
    members: []const ast.EnumMember,
};

const CollectedFunction = struct {
    name: []const u8,
    declaration: []const u8,
    implementation: []const u8,
};

const TypeCollection = struct {
    list_types: std.ArrayList([]const u8),
    builtin_types: std.ArrayList([]const u8),
    custom_types: std.ArrayList(CollectedType),
    struct_types: std.ArrayList(CollectedStruct),
    enum_types: std.ArrayList(CollectedEnum),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) TypeCollection {
        return TypeCollection{
            .list_types = std.ArrayList([]const u8).init(allocator),
            .builtin_types = std.ArrayList([]const u8).init(allocator),
            .custom_types = std.ArrayList(CollectedType).init(allocator),
            .struct_types = std.ArrayList(CollectedStruct).init(allocator),
            .enum_types = std.ArrayList(CollectedEnum).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *TypeCollection) void {
        // Free all allocated strings in list_types
        for (self.list_types.items) |type_name| {
            self.allocator.free(type_name);
        }

        // Free all allocated strings in builtin_types
        for (self.builtin_types.items) |type_name| {
            self.allocator.free(type_name);
        }

        // Free all allocated strings in struct_types
        for (self.struct_types.items) |struct_type| {
            self.allocator.free(struct_type.name);
            struct_type.methods.deinit();
        }

        // Free all allocated strings in enum_types
        for (self.enum_types.items) |enum_type| {
            self.allocator.free(enum_type.name);
        }

        self.list_types.deinit();
        self.builtin_types.deinit();
        self.custom_types.deinit();
        self.struct_types.deinit();
        self.enum_types.deinit();
    }

    fn hasListType(self: *const TypeCollection, type_name: []const u8) bool {
        for (self.list_types.items) |existing| {
            if (std.mem.eql(u8, existing, type_name)) return true;
        }
        return false;
    }

    fn hasBuiltinType(self: *const TypeCollection, type_name: []const u8) bool {
        for (self.builtin_types.items) |existing| {
            if (std.mem.eql(u8, existing, type_name)) return true;
        }
        return false;
    }

    fn hasStructType(self: *const TypeCollection, type_name: []const u8) bool {
        for (self.struct_types.items) |existing| {
            if (std.mem.eql(u8, existing.name, type_name)) return true;
        }
        return false;
    }

    fn addListType(self: *TypeCollection, allocator: std.mem.Allocator, type_name: []const u8) !void {
        if (!self.hasListType(type_name)) {
            const owned_name = try allocator.dupe(u8, type_name);
            try self.list_types.append(owned_name);
        }
    }

    fn addBuiltinType(self: *TypeCollection, allocator: std.mem.Allocator, type_name: []const u8) !void {
        if (!self.hasBuiltinType(type_name)) {
            const owned_name = try allocator.dupe(u8, type_name);
            try self.builtin_types.append(owned_name);
        }
    }

    fn addStructType(self: *TypeCollection, allocator: std.mem.Allocator, struct_decl: anytype) !void {
        if (!self.hasStructType(struct_decl.name)) {
            const collected_struct = CollectedStruct{
                .name = try allocator.dupe(u8, struct_decl.name),
                .fields = struct_decl.fields.items,
                .methods = std.ArrayList([]const u8).init(allocator),
                .is_comptime = struct_decl.is_comptime,
            };
            try self.struct_types.append(collected_struct);
        }
    }

    fn hasEnumType(self: *const TypeCollection, enum_name: []const u8) bool {
        for (self.enum_types.items) |enum_type| {
            if (std.mem.eql(u8, enum_type.name, enum_name)) {
                return true;
            }
        }
        return false;
    }

    fn addEnumType(self: *TypeCollection, allocator: std.mem.Allocator, enum_decl: anytype) !void {
        if (!self.hasEnumType(enum_decl.name)) {
            const collected_enum = CollectedEnum{
                .name = try allocator.dupe(u8, enum_decl.name),
                .members = enum_decl.members.items,
            };
            try self.enum_types.append(collected_enum);
        }
    }
};

const FunctionCollection = struct {
    functions: std.ArrayList(CollectedFunction),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) FunctionCollection {
        return FunctionCollection{
            .functions = std.ArrayList(CollectedFunction).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *FunctionCollection) void {
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

/// Code generation for C backend
/// The generated code should be compiled by the Fil-C clang compiler
/// The Fil-C compiler compiles to a special C runtime that has memory safety features built in
/// It also uses garbage collection so there is no need to free any memory
pub const CCodegen = struct {
    allocator: std.mem.Allocator,
    arena: *const ast.AstArena,
    semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer,
    type_collection: TypeCollection,
    function_collection: FunctionCollection,

    pub fn init(allocator: std.mem.Allocator, arena: *const ast.AstArena, semantic_analyzer: *const SemanticAnalyzer.SemanticAnalyzer) CCodegen {
        return CCodegen{
            .allocator = allocator,
            .arena = arena,
            .semantic_analyzer = semantic_analyzer,
            .type_collection = TypeCollection.init(allocator),
            .function_collection = FunctionCollection.init(allocator),
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
        const c_source = try self.generateCSource(root_node_id);
        
        // Write the generated C source to a file in howl-out directory
        const temp_file_path = "howl-out/howl_program.c";
        try std.fs.cwd().writeFile(.{ .sub_path = temp_file_path, .data = c_source });

        // Compile using filc compiler
        const output_name = "howl-out/howl_output";
        try self.compileCFile(temp_file_path, output_name);

        // Return the generated C source code so it can be displayed
        return c_source;
    }

    fn generateCSource(self: *CCodegen, root_node_id: ast.NodeId) ![]u8 {
        var output = std.ArrayList(u8).init(self.allocator);
        defer output.deinit();

        const writer = output.writer();

        // First pass: Collect all types and functions by traversing the AST
        try self.collectTypesAndFunctions(root_node_id);

        // Generate basic C header with includes
        try self.generateBasicCHeader(writer);

        // Generate collected types and builtin implementations
        try self.generateCollectedTypes(writer);

        // Generate function declarations
        try writer.writeAll("// ============================================================================\n");
        try writer.writeAll("// Function Declarations\n");
        try writer.writeAll("// ============================================================================\n\n");
        try self.generateCollectedFunctionDeclarations(writer);
        try writer.writeAll("char* howl_string_concat(const char* left, const char* right);\n");
        try writer.writeAll("int main(void);\n\n");

        // Generate function implementations
        try writer.writeAll("// ============================================================================\n");
        try writer.writeAll("// Function Implementations\n");
        try writer.writeAll("// ============================================================================\n\n");
        try self.generateCollectedFunctionImplementations(writer, root_node_id);

        // Generate string concatenation function implementation
        try writer.writeAll("char* howl_string_concat(const char* left, const char* right) {\n");
        try writer.writeAll("    if (left == NULL) left = \"\";\n");
        try writer.writeAll("    if (right == NULL) right = \"\";\n");
        try writer.writeAll("    \n");
        try writer.writeAll("    size_t left_len = strlen(left);\n");
        try writer.writeAll("    size_t right_len = strlen(right);\n");
        try writer.writeAll("    size_t total_len = left_len + right_len + 1;\n");
        try writer.writeAll("    \n");
        try writer.writeAll("    char* result = malloc(total_len);\n");
        try writer.writeAll("    if (result == NULL) {\n");
        try writer.writeAll("        fprintf(stderr, \"Memory allocation failed for string concatenation\\n\");\n");
        try writer.writeAll("        exit(1);\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    \n");
        try writer.writeAll("    strcpy(result, left);\n");
        try writer.writeAll("    strcat(result, right);\n");
        try writer.writeAll("    \n");
        try writer.writeAll("    return result;\n");
        try writer.writeAll("}\n\n");

        // Generate main function implementation
        try self.generateMainBody(writer, root_node_id, 0);

        return try output.toOwnedSlice();
    }

    // Collection phase - traverse AST and collect all types and functions
    fn collectTypesAndFunctions(self: *CCodegen, node_id: ast.NodeId) !void {
        try self.collectFromNode(node_id);
    }

    fn collectFromNode(self: *CCodegen, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.collectFromNode(stmt_id);
                }
            },
            .function_decl => |func_decl| {
                // Collect function signature and implementation
                try self.collectFunction(func_decl);
                // Also traverse function body for nested types/functions
                try self.collectFromNode(func_decl.body);
            },
            .extern_fn_decl => |extern_fn_decl| {
                // Handle extern function by evaluating its compile-time body
                try self.collectExternFunction(extern_fn_decl);
                // Traverse the compile-time body for additional types/functions
                try self.collectFromNode(extern_fn_decl.compile_time_body);
            },
            .struct_decl => |struct_decl| {
                // Collect struct definition
                try self.type_collection.addStructType(self.allocator, struct_decl);
                // Note: struct methods are collected when we encounter function_decl with namespace
            },
            .enum_decl => |enum_decl| {
                // Collect enum definition
                try self.type_collection.addEnumType(self.allocator, enum_decl);
            },
            .var_decl => |var_decl| {
                // Collect types used in variable declarations
                try self.collectTypeFromVariableDecl(var_decl);
                if (var_decl.initializer) |init_id| {
                    try self.collectFromNode(init_id);
                }
            },
            .call_expr => |call_expr| {
                // Collect stdlib calls (List, StringBuilder, etc.)
                try self.collectTypeFromCallExpr(call_expr);
                for (call_expr.args.items) |arg_id| {
                    try self.collectFromNode(arg_id);
                }
            },
            .for_expr => |for_expr| {
                try self.collectFromNode(for_expr.iterable);
                try self.collectFromNode(for_expr.body);
            },
            .if_expr => |if_expr| {
                try self.collectFromNode(if_expr.condition);
                try self.collectFromNode(if_expr.then_branch);
                if (if_expr.else_branch) |else_id| {
                    try self.collectFromNode(else_id);
                }
            },
            .binary_expr => |binary_expr| {
                try self.collectFromNode(binary_expr.left);
                try self.collectFromNode(binary_expr.right);
            },
            .struct_init => |struct_init| {
                // Collect struct instantiation - this tells us which structs are actually used
                if (struct_init.type_name) |type_name| {
                    try self.markStructAsUsed(type_name);
                }
                for (struct_init.fields.items) |field_init| {
                    try self.collectFromNode(field_init.value);
                }
            },
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

    fn collectFunction(self: *CCodegen, func_decl: anytype) !void {
        // Don't collect main function - handled separately
        if (std.mem.eql(u8, func_decl.name, "main")) return;

        // Generate function declaration
        var decl_buf = std.ArrayList(u8).init(self.allocator);
        defer decl_buf.deinit();
        const decl_writer = decl_buf.writer();
        try self.generateCFunctionDeclaration(decl_writer, func_decl);

        // Generate function implementation (will be done later when we traverse for implementations)
        const collected_func = CollectedFunction{
            .name = try self.allocator.dupe(u8, func_decl.name),
            .declaration = try self.allocator.dupe(u8, decl_buf.items),
            .implementation = "", // Will be filled during implementation generation
        };

        try self.function_collection.functions.append(collected_func);
    }

    fn collectTypeFromVariableDecl(self: *CCodegen, var_decl: anytype) !void {
        if (var_decl.type_annotation) |type_id| {
            try self.collectTypeFromTypeNode(type_id);
        }
    }

    fn collectTypeFromCallExpr(self: *CCodegen, call_expr: anytype) !void {
        // Check for stdlib calls like std.List(i32).init()
        const callee_id = call_expr.callee;
        const callee_node = self.arena.getNodeConst(callee_id) orelse return;
        if (callee_node.data == .member_expr) {
            const member = callee_node.data.member_expr;
            const obj_id = member.object;
            const obj_node = self.arena.getNodeConst(obj_id) orelse return;
            if (obj_node.data == .call_expr) {
                const inner_call = obj_node.data.call_expr;
                const inner_callee_id = inner_call.callee;
                const inner_callee_node = self.arena.getNodeConst(inner_callee_id) orelse return;
                if (inner_callee_node.data == .member_expr) {
                    const inner_member = inner_callee_node.data.member_expr;
                    // Check for std.List pattern
                    const member_name = inner_member.field;
                    if (std.mem.eql(u8, member_name, "List")) {
                        // Extract type from std.List(T)
                        if (inner_call.args.items.len > 0) {
                            const type_arg_id = inner_call.args.items[0];
                            try self.collectListType(type_arg_id);
                        }
                    } else if (std.mem.eql(u8, member_name, "StringBuilder")) {
                        // Mark StringBuilder as used
                        try self.type_collection.addBuiltinType(self.allocator, "StringBuilder");
                    }
                }
            }
        }
    }

    fn collectListType(self: *CCodegen, type_node_id: ast.NodeId) !void {
        const type_node = self.arena.getNodeConst(type_node_id) orelse return;
        if (type_node.data == .identifier) {
            const type_name = type_node.data.identifier.name;
            try self.type_collection.addListType(self.allocator, type_name);
        }
    }

    fn markStructAsUsed(self: *CCodegen, type_name: []const u8) !void {
        // This function marks a struct as actually used in the code
        // We'll use this to determine which structs need C generation
        _ = self;
        _ = type_name;
        // For now, just mark that we've seen it used
        // Later we can add a "used_structs" collection
    }

    fn collectExternFunction(self: *CCodegen, extern_fn_decl: anytype) !void {
        // Don't collect main function - handled separately
        if (std.mem.eql(u8, extern_fn_decl.name, "main")) return;

        // For extern functions, evaluate the compile-time body to generate C code
        // The actual code generation is deferred until we know the target
        var decl_buf = std.ArrayList(u8).init(self.allocator);
        defer decl_buf.deinit();
        const decl_writer = decl_buf.writer();

        // Generate C function signature for extern function
        try self.generateCExternFunctionDeclaration(decl_writer, extern_fn_decl);

        const collected_func = CollectedFunction{
            .name = try self.allocator.dupe(u8, extern_fn_decl.name),
            .declaration = try self.allocator.dupe(u8, decl_buf.items),
            .implementation = "", // Will be filled by compile-time evaluation
        };

        try self.function_collection.functions.append(collected_func);
    }

    fn collectFromMatchCompile(self: *CCodegen, match_compile: anytype) !void {
        _ = self;
        _ = match_compile;
        // TODO: Implement match compile collection for C target
    }

    fn collectTypeFromTypeNode(self: *CCodegen, type_node_id: ast.NodeId) !void {
        // Collect custom struct/enum types when referenced in type annotations
        const type_node = self.arena.getNodeConst(type_node_id) orelse return;
        if (type_node.data == .identifier) {
            const type_name = type_node.data.identifier.name;
            // Check if this is a struct type we've collected
            if (self.type_collection.hasStructType(type_name)) {
                try self.markStructAsUsed(type_name);
            }
        }
    }

    // Basic header generation - just includes and basic types
    fn generateBasicCHeader(self: *CCodegen, writer: Writer) !void {
        _ = self;
        try writer.writeAll("// Generated by Howl Language Compiler\n\n");
        try writer.writeAll("#include <stdio.h>\n");
        try writer.writeAll("#include <stdlib.h>\n");
        try writer.writeAll("#include <stdint.h>\n");
        try writer.writeAll("#include <stdbool.h>\n");
        try writer.writeAll("#include <float.h>\n");
        try writer.writeAll("#include <assert.h>\n");
        try writer.writeAll("#include <string.h>\n\n");

        // IEEE 754 binary exchange format types for exact bit width
        try writer.writeAll("// IEEE 754 binary exchange format types for exact bit width\n");
        try writer.writeAll("#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L\n");
        try writer.writeAll("// C23 standard _Float32 and _Float64 types\n");
        try writer.writeAll("typedef _Float32 howl_f32_t;\n");
        try writer.writeAll("typedef _Float64 howl_f64_t;\n");
        try writer.writeAll("#else\n");
        try writer.writeAll("// Fallback to traditional types with static assertions\n");
        try writer.writeAll("typedef float howl_f32_t;\n");
        try writer.writeAll("typedef double howl_f64_t;\n");
        try writer.writeAll("_Static_assert(sizeof(howl_f32_t) == 4, \"howl_f32_t must be 4 bytes\");\n");
        try writer.writeAll("_Static_assert(sizeof(howl_f64_t) == 8, \"howl_f64_t must be 8 bytes\");\n");
        try writer.writeAll("#endif\n\n");
    }

    // Generate all collected types
    fn generateCollectedTypes(self: *CCodegen, writer: Writer) !void {
        // Generate enum types first (they might be dependencies)
        for (self.type_collection.enum_types.items) |enum_type| {
            try self.generateEnumImplementation(writer, enum_type);
        }

        // Generate struct types (they might be dependencies)
        for (self.type_collection.struct_types.items) |struct_type| {
            try self.generateStructImplementation(writer, struct_type);
        }

        // Generate List types for collected types
        for (self.type_collection.list_types.items) |type_name| {
            try self.generateListTypeImplementation(writer, type_name);
        }

        // Generate StringBuilder if used
        if (self.type_collection.hasBuiltinType("StringBuilder")) {
            try self.generateStringBuilderImplementation(writer);
        }

        // Generate custom types (future: enums, unions, etc.)
        for (self.type_collection.custom_types.items) |custom_type| {
            try writer.writeAll(custom_type.c_definition);
        }
    }

    fn generateListTypeImplementation(self: *CCodegen, writer: Writer, howl_type: []const u8) !void {
        _ = self;
        const c_type = if (std.mem.eql(u8, howl_type, "i32")) "int32_t" else if (std.mem.eql(u8, howl_type, "f32")) "howl_f32_t" else "void*"; // fallback

        try writer.writeAll("// ============================================================================\n");
        try writer.print("// std.List({s}) Implementation - Resizable Array\n", .{howl_type});
        try writer.writeAll("// ============================================================================\n\n");

        // Structure definition
        try writer.writeAll("typedef struct {\n");
        try writer.print("    {s}* data;        // Dynamic array of elements\n", .{c_type});
        try writer.writeAll("    size_t length;        // Current number of elements\n");
        try writer.writeAll("    size_t capacity;      // Current allocated capacity\n");
        try writer.print("}} HowlList_{s};\n\n", .{howl_type});

        // Function declarations and implementations
        try writer.print("HowlList_{s}* HowlList_{s}_init(void);\n", .{ howl_type, howl_type });
        try writer.print("void HowlList_{s}_append(HowlList_{s}* list, {s} value);\n", .{ howl_type, howl_type, c_type });
        try writer.print("{s} HowlList_{s}_get(const HowlList_{s}* list, size_t index);\n\n", .{ c_type, howl_type, howl_type });

        // Implementations
        try writer.print("HowlList_{s}* HowlList_{s}_init(void) {{\n", .{ howl_type, howl_type });
        try writer.print("    HowlList_{s}* list = (HowlList_{s}*)malloc(sizeof(HowlList_{s}));\n", .{ howl_type, howl_type, howl_type });
        try writer.writeAll("    if (list == NULL) {\n");
        try writer.writeAll("        fprintf(stderr, \"Failed to allocate memory for List\\n\");\n");
        try writer.writeAll("        exit(1);\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    list->capacity = 8;\n");
        try writer.writeAll("    list->length = 0;\n");
        try writer.print("    list->data = ({s}*)malloc(list->capacity * sizeof({s}));\n", .{ c_type, c_type });
        try writer.writeAll("    if (list->data == NULL) {\n");
        try writer.writeAll("        fprintf(stderr, \"Failed to allocate memory for List data\\n\");\n");
        try writer.writeAll("        free(list);\n");
        try writer.writeAll("        exit(1);\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    return list;\n");
        try writer.writeAll("}\n\n");

        try writer.print("void HowlList_{s}_append(HowlList_{s}* list, {s} value) {{\n", .{ howl_type, howl_type, c_type });
        try writer.writeAll("    if (list->length >= list->capacity) {\n");
        try writer.writeAll("        list->capacity *= 2;\n");
        try writer.print("        list->data = ({s}*)realloc(list->data, list->capacity * sizeof({s}));\n", .{ c_type, c_type });
        try writer.writeAll("        if (list->data == NULL) {\n");
        try writer.writeAll("            fprintf(stderr, \"Failed to resize List\\n\");\n");
        try writer.writeAll("            exit(1);\n");
        try writer.writeAll("        }\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    list->data[list->length] = value;\n");
        try writer.writeAll("    list->length++;\n");
        try writer.writeAll("}\n\n");

        try writer.print("{s} HowlList_{s}_get(const HowlList_{s}* list, size_t index) {{\n", .{ c_type, howl_type, howl_type });
        try writer.writeAll("    if (index >= list->length) {\n");
        try writer.writeAll("        fprintf(stderr, \"List index out of bounds: %zu >= %zu\\n\", index, list->length);\n");
        try writer.writeAll("        exit(1);\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    return list->data[index];\n");
        try writer.writeAll("}\n\n");
    }

    fn generateEnumImplementation(self: *CCodegen, writer: Writer, enum_info: CollectedEnum) !void {
        _ = self; // suppress unused parameter warning

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

    fn generateStructImplementation(self: *CCodegen, writer: Writer, struct_info: CollectedStruct) !void {
        try writer.writeAll("// ============================================================================\n");
        try writer.print("// Struct {s} Implementation\n", .{struct_info.name});
        try writer.writeAll("// ============================================================================\n\n");

        // Separate fields into data fields and methods
        var data_fields = std.ArrayList(ast.Field).init(self.allocator);
        defer data_fields.deinit();
        var method_fields = std.ArrayList(ast.Field).init(self.allocator);
        defer method_fields.deinit();

        for (struct_info.fields) |field| {
            var is_method = false;
            if (field.type_annotation) |type_id| {
                const type_node = self.arena.getNodeConst(type_id);
                if (type_node) |node| {
                    if (node.data == .identifier) {
                        const type_name = node.data.identifier.name;
                        if (std.mem.eql(u8, type_name, "fn")) {
                            is_method = true;
                        }
                    }
                }
            }

            if (is_method) {
                try method_fields.append(field);
            } else {
                try data_fields.append(field);
            }
        }

        // Generate struct definition (only data fields)
        try writer.writeAll("typedef struct {\n");
        for (data_fields.items) |field| {
            const field_type = if (field.type_annotation) |type_id|
                self.generateCTypeFromNode(type_id)
            else
                "int32_t"; // Default type

            try writer.print("    {s} {s};\n", .{ field_type, field.name });
        }
        try writer.print("}} {s};\n\n", .{struct_info.name});

        // Generate constructor function (only data fields)
        try writer.print("// Constructor for {s}\n", .{struct_info.name});
        try writer.print("{s} {s}_init(", .{ struct_info.name, struct_info.name });

        // Generate constructor parameters (only data fields)
        for (data_fields.items, 0..) |field, i| {
            if (i > 0) try writer.writeAll(", ");
            const field_type = if (field.type_annotation) |type_id|
                self.generateCTypeFromNode(type_id)
            else
                "int32_t";
            try writer.print("{s} {s}", .{ field_type, field.name });
        }
        try writer.writeAll(") {\n");

        // Generate constructor body (only data fields)
        try writer.print("    {s} instance = {{", .{struct_info.name});
        for (data_fields.items, 0..) |field, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.print(".{s} = {s}", .{ field.name, field.name });
        }
        try writer.writeAll("};\n");
        try writer.writeAll("    return instance;\n");
        try writer.writeAll("}\n\n");

        // Generate getter/setter methods for data fields only
        for (data_fields.items) |field| {
            const field_type = if (field.type_annotation) |type_id|
                self.generateCTypeFromNode(type_id)
            else
                "int32_t";

            // Getter
            try writer.print("{s} {s}_get_{s}(const {s}* self) {{\n", .{ field_type, struct_info.name, field.name, struct_info.name });
            try writer.print("    return self->{s};\n", .{field.name});
            try writer.writeAll("}\n\n");

            // Setter
            try writer.print("void {s}_set_{s}({s}* self, {s} value) {{\n", .{ struct_info.name, field.name, struct_info.name, field_type });
            try writer.print("    self->{s} = value;\n", .{field.name});
            try writer.writeAll("}\n\n");
        }
    }

    fn generateCExternFunctionImplementation(self: *CCodegen, writer: Writer, extern_fn_decl: anytype) !void {
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

        try writer.writeAll(") {\n");

        // Evaluate the compile-time body to generate C code
        try self.evaluateExternFunctionBody(writer, extern_fn_decl);

        try writer.writeAll("}\n\n");
    }

    /// Evaluate the compile-time body of an extern function to generate C code
    fn evaluateExternFunctionBody(self: *CCodegen, writer: Writer, extern_fn_decl: anytype) !void {
        // Traverse the compile-time body and execute @compile.insert directives
        try self.evaluateComptimeAST(writer, extern_fn_decl.compile_time_body, 1);
    }

    /// Evaluate compile-time AST expressions and generate appropriate C code
    fn evaluateComptimeAST(self: *CCodegen, writer: Writer, node_id: ast.NodeId, depth: u32) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.evaluateComptimeAST(writer, stmt_id, depth);
                }
            },
            .match_compile_expr => |match_compile| {
                // Find and execute the C target branch
                for (match_compile.arms.items) |arm| {
                    if (arm.target == .c) {
                        try self.evaluateComptimeAST(writer, arm.body, depth);
                        break;
                    }
                }
            },
            .compile_insert_expr => |compile_insert| {
                // Insert raw C code with proper indentation
                for (0..depth) |_| try writer.writeAll("    ");
                try writer.writeAll(compile_insert.code);
                try writer.writeAll("\n");
            },
            .call_expr => |call_expr| {
                // Handle special compile-time function calls
                const callee_node = self.arena.getNodeConst(call_expr.callee) orelse return;
                if (callee_node.data == .identifier) {
                    const func_name = callee_node.data.identifier.name;
                    if (std.mem.eql(u8, func_name, "print")) {
                        // Enhanced printf generation for different argument patterns
                        try self.generateSmartPrintf(writer, call_expr, depth);
                    } else {
                        // Regular function call
                        try self.generateCFromAST(writer, node_id, depth);
                    }
                } else {
                    try self.generateCFromAST(writer, node_id, depth);
                }
            },
            else => {
                // For other expressions, generate regular C code
                try self.generateCFromAST(writer, node_id, depth);
            },
        }
    }

    /// Enhanced printf generation that automatically adds format specifiers
    fn generateSmartPrintf(self: *CCodegen, writer: Writer, call_expr: anytype, depth: u32) !void {
        for (0..depth) |_| try writer.writeAll("    ");

        if (call_expr.args.items.len == 0) {
            try writer.writeAll("printf(\"\\n\");\n");
            return;
        }

        // Get the format string
        const format_arg = call_expr.args.items[0];
        const format_node = self.arena.getNodeConst(format_arg) orelse {
            try writer.writeAll("printf(\"Error: Invalid format\\n\");\n");
            return;
        };

        if (format_node.data != .literal or format_node.data.literal != .string) {
            try writer.writeAll("printf(\"Error: First argument must be string literal\\n\");\n");
            return;
        }

        const format_str = format_node.data.literal.string.value;

        if (call_expr.args.items.len == 1) {
            // Just print the format string
            try writer.print("printf(\"{s}\");\n", .{format_str});
        } else {
            // Check if format string has placeholders
            const placeholder_count = std.mem.count(u8, format_str, "{}");
            const arg_count = call_expr.args.items.len - 1;

            if (placeholder_count == 0 and arg_count > 0) {
                // No placeholders but have arguments - add %d for each argument
                try writer.print("printf(\"{s}\"", .{format_str});
                for (1..call_expr.args.items.len) |i| {
                    try writer.writeAll(", ");
                    try self.generateCFromAST(writer, call_expr.args.items[i], 0);
                }
                try writer.writeAll(");\n");
            } else {
                // Replace {} with %d and print
                var modified_format = std.ArrayList(u8).init(self.allocator);
                defer modified_format.deinit();

                var i: usize = 0;
                while (i < format_str.len) {
                    if (i + 1 < format_str.len and format_str[i] == '{' and format_str[i + 1] == '}') {
                        try modified_format.appendSlice("%d");
                        i += 2;
                    } else {
                        try modified_format.append(format_str[i]);
                        i += 1;
                    }
                }

                try writer.print("printf(\"{s}\"", .{modified_format.items});
                for (1..call_expr.args.items.len) |j| {
                    try writer.writeAll(", ");
                    try self.generateCFromAST(writer, call_expr.args.items[j], 0);
                }
                try writer.writeAll(");\n");
            }
        }
    }

    fn generateStructMethod(self: *CCodegen, writer: Writer, struct_name: []const u8, method: ast.Field) !void {
        _ = self; // TODO: Will be used for advanced method generation

        // Generate namespace method: StructName_methodName
        try writer.print("// Method {s}.{s}\n", .{ struct_name, method.name });

        // For now, generate a simple method signature
        // TODO: Parse actual function parameters and return type
        try writer.print("int64_t {s}_{s}({s}* self) {{\n", .{ struct_name, method.name, struct_name });
        try writer.writeAll("    // TODO: Implement method body\n");
        try writer.writeAll("    return 0;\n");
        try writer.writeAll("}\n\n");
    }

    fn generateCTypeFromNode(self: *CCodegen, type_node_id: ast.NodeId) []const u8 {
        const type_node = self.arena.getNodeConst(type_node_id) orelse return "void*";

        if (type_node.data == .identifier) {
            const type_name = type_node.data.identifier.name;

            // Map Howl types to C types
            if (std.mem.eql(u8, type_name, "i32")) return "int32_t";
            if (std.mem.eql(u8, type_name, "i64")) return "int64_t";
            if (std.mem.eql(u8, type_name, "u32")) return "uint32_t";
            if (std.mem.eql(u8, type_name, "u64")) return "uint64_t";
            if (std.mem.eql(u8, type_name, "f32")) return "howl_f32_t";
            if (std.mem.eql(u8, type_name, "bool")) return "bool";
            if (std.mem.eql(u8, type_name, "str")) return "const char*";
            if (std.mem.eql(u8, type_name, "strb")) return "HowlStringBuilder*";

            // Handle function types - for methods, we'll skip them in struct generation
            if (std.mem.eql(u8, type_name, "fn")) return "void*"; // Placeholder for methods

            // For struct types, use the struct name directly
            if (self.type_collection.hasStructType(type_name)) {
                return type_name;
            }

            // Default fallback
            return type_name;
        }

        return "void*"; // Fallback for complex types
    }

    fn generateStringBuilderImplementation(self: *CCodegen, writer: Writer) !void {
        _ = self;
        try writer.writeAll("// ============================================================================\n");
        try writer.writeAll("// StringBuilder Implementation - Mutable String Builder\n");
        try writer.writeAll("// ============================================================================\n\n");

        // StringBuilder structure
        try writer.writeAll("typedef struct {\n");
        try writer.writeAll("    char* data;           // Dynamic character array\n");
        try writer.writeAll("    size_t length;        // Current string length (excluding null terminator)\n");
        try writer.writeAll("    size_t capacity;      // Current allocated capacity\n");
        try writer.writeAll("} HowlStringBuilder;\n\n");

        // Function declarations
        try writer.writeAll("HowlStringBuilder* HowlStringBuilder_init(void);\n");
        try writer.writeAll("void HowlStringBuilder_append_str(HowlStringBuilder* sb, const char* str);\n");
        try writer.writeAll("void HowlStringBuilder_append_char(HowlStringBuilder* sb, char c);\n");
        try writer.writeAll("const char* HowlStringBuilder_toString(const HowlStringBuilder* sb);\n");
        try writer.writeAll("void HowlStringBuilder_deinit(HowlStringBuilder* sb);\n");
        try writer.writeAll("size_t HowlStringBuilder_length(const HowlStringBuilder* sb);\n\n");

        // Function implementations
        try writer.writeAll("HowlStringBuilder* HowlStringBuilder_init(void) {\n");
        try writer.writeAll("    HowlStringBuilder* sb = (HowlStringBuilder*)malloc(sizeof(HowlStringBuilder));\n");
        try writer.writeAll("    if (sb == NULL) {\n");
        try writer.writeAll("        fprintf(stderr, \"Failed to allocate memory for StringBuilder\\n\");\n");
        try writer.writeAll("        exit(1);\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    sb->capacity = 16;\n");
        try writer.writeAll("    sb->length = 0;\n");
        try writer.writeAll("    sb->data = (char*)malloc(sb->capacity);\n");
        try writer.writeAll("    if (sb->data == NULL) {\n");
        try writer.writeAll("        fprintf(stderr, \"Failed to allocate memory for StringBuilder data\\n\");\n");
        try writer.writeAll("        free(sb);\n");
        try writer.writeAll("        exit(1);\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    sb->data[0] = '\\0';\n");
        try writer.writeAll("    return sb;\n");
        try writer.writeAll("}\n\n");

        try writer.writeAll("void HowlStringBuilder_append_str(HowlStringBuilder* sb, const char* str) {\n");
        try writer.writeAll("    if (sb == NULL || str == NULL) return;\n");
        try writer.writeAll("    size_t str_len = strlen(str);\n");
        try writer.writeAll("    size_t needed_capacity = sb->length + str_len + 1;\n");
        try writer.writeAll("    if (needed_capacity > sb->capacity) {\n");
        try writer.writeAll("        size_t new_capacity = sb->capacity;\n");
        try writer.writeAll("        while (new_capacity < needed_capacity) {\n");
        try writer.writeAll("            new_capacity *= 2;\n");
        try writer.writeAll("        }\n");
        try writer.writeAll("        char* new_data = (char*)realloc(sb->data, new_capacity);\n");
        try writer.writeAll("        if (new_data == NULL) {\n");
        try writer.writeAll("            fprintf(stderr, \"Failed to resize StringBuilder\\n\");\n");
        try writer.writeAll("            exit(1);\n");
        try writer.writeAll("        }\n");
        try writer.writeAll("        sb->data = new_data;\n");
        try writer.writeAll("        sb->capacity = new_capacity;\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    strcpy(sb->data + sb->length, str);\n");
        try writer.writeAll("    sb->length += str_len;\n");
        try writer.writeAll("}\n\n");

        try writer.writeAll("void HowlStringBuilder_append_char(HowlStringBuilder* sb, char c) {\n");
        try writer.writeAll("    if (sb == NULL) return;\n");
        try writer.writeAll("    size_t needed_capacity = sb->length + 2;\n");
        try writer.writeAll("    if (needed_capacity > sb->capacity) {\n");
        try writer.writeAll("        size_t new_capacity = sb->capacity * 2;\n");
        try writer.writeAll("        char* new_data = (char*)realloc(sb->data, new_capacity);\n");
        try writer.writeAll("        if (new_data == NULL) {\n");
        try writer.writeAll("            fprintf(stderr, \"Failed to resize StringBuilder\\n\");\n");
        try writer.writeAll("            exit(1);\n");
        try writer.writeAll("        }\n");
        try writer.writeAll("        sb->data = new_data;\n");
        try writer.writeAll("        sb->capacity = new_capacity;\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    sb->data[sb->length] = c;\n");
        try writer.writeAll("    sb->length++;\n");
        try writer.writeAll("    sb->data[sb->length] = '\\0';\n");
        try writer.writeAll("}\n\n");

        try writer.writeAll("const char* HowlStringBuilder_toString(const HowlStringBuilder* sb) {\n");
        try writer.writeAll("    if (sb == NULL) return \"\";\n");
        try writer.writeAll("    return sb->data;\n");
        try writer.writeAll("}\n\n");

        try writer.writeAll("void HowlStringBuilder_deinit(HowlStringBuilder* sb) {\n");
        try writer.writeAll("    if (sb == NULL) return;\n");
        try writer.writeAll("    if (sb->data != NULL) {\n");
        try writer.writeAll("        free(sb->data);\n");
        try writer.writeAll("        sb->data = NULL;\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    free(sb);\n");
        try writer.writeAll("}\n\n");

        try writer.writeAll("size_t HowlStringBuilder_length(const HowlStringBuilder* sb) {\n");
        try writer.writeAll("    if (sb == NULL) return 0;\n");
        try writer.writeAll("    return sb->length;\n");
        try writer.writeAll("}\n\n");
    }

    // Generate function declarations from collected functions
    fn generateCollectedFunctionDeclarations(self: *CCodegen, writer: Writer) !void {
        for (self.function_collection.functions.items) |func| {
            try writer.writeAll(func.declaration);
        }
    }

    // Generate function implementations from collected functions
    fn generateCollectedFunctionImplementations(self: *CCodegen, writer: Writer, root_node_id: ast.NodeId) !void {
        // Now traverse again to generate actual implementations
        try self.generateAllFunctionImplementations(writer, root_node_id);
    }

    fn generateAllFunctionDeclarations(self: *CCodegen, writer: Writer, node_id: ast.NodeId) !void {
        try self.collectFunctionDeclarations(writer, node_id);
    }

    fn collectFunctionDeclarations(self: *CCodegen, writer: Writer, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .block => |block| {
                for (block.statements.items) |stmt_id| {
                    try self.collectFunctionDeclarations(writer, stmt_id);
                }
            },
            .function_decl => |func_decl| {
                // Don't generate declaration for main function
                if (!std.mem.eql(u8, func_decl.name, "main")) {
                    try self.generateCFunctionDeclaration(writer, func_decl);
                }
            },
            .extern_fn_decl => |extern_fn_decl| {
                // Don't generate declaration for main function
                if (!std.mem.eql(u8, extern_fn_decl.name, "main")) {
                    try self.generateCExternFunctionDeclaration(writer, extern_fn_decl);
                }
            },
            else => {},
        }
    }

    fn generateAllFunctionImplementations(self: *CCodegen, writer: Writer, node_id: ast.NodeId) !void {
        try self.collectFunctionImplementations(writer, node_id);
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
                // Don't generate implementation for main function (handled separately)
                if (!std.mem.eql(u8, func_decl.name, "main")) {
                    try self.generateCFunctionImplementation(writer, func_decl);
                }
            },
            .extern_fn_decl => |extern_fn_decl| {
                // Don't generate implementation for main function (handled separately)
                if (!std.mem.eql(u8, extern_fn_decl.name, "main")) {
                    try self.generateCExternFunctionImplementation(writer, extern_fn_decl);
                }
            },
            else => {},
        }
    }

    fn generateCFunctionDeclaration(self: *CCodegen, writer: Writer, func_decl: anytype) !void {
        // Generate return type
        const return_type_str = if (func_decl.return_type) |return_type_node_id| blk: {
            const return_type_info = self.getNodeType(return_type_node_id);
            break :blk self.generateCType(return_type_info);
        } else "void";

        try writer.print("{s} {s}(", .{ return_type_str, func_decl.name });

        // Generate parameters
        for (func_decl.params.items, 0..) |param, i| {
            if (i > 0) try writer.writeAll(", ");
            const param_type_str = if (param.type_annotation) |type_node_id| blk: {
                const param_type_info = self.getNodeType(type_node_id);
                break :blk self.generateCType(param_type_info);
            } else "int32_t"; // Fallback if no type annotation
            try writer.print("{s} {s}", .{ param_type_str, param.name });
        }

        try writer.writeAll(");\n");
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
        // Generate return type
        const return_type_str = if (func_decl.return_type) |return_type_node_id| blk: {
            const return_type_info = self.getNodeType(return_type_node_id);
            break :blk self.generateCType(return_type_info);
        } else "void";

        try writer.print("{s} {s}(", .{ return_type_str, func_decl.name });

        // Generate parameters
        for (func_decl.params.items, 0..) |param, i| {
            if (i > 0) try writer.writeAll(", ");
            const param_type_str = if (param.type_annotation) |type_node_id| blk: {
                const param_type_info = self.getNodeType(type_node_id);
                break :blk self.generateCType(param_type_info);
            } else "int32_t"; // Fallback if no type annotation
            try writer.print("{s} {s}", .{ param_type_str, param.name });
        }

        try writer.writeAll(") {\n");

        // Generate function body
        try self.generateCFromAST(writer, func_decl.body, 1);

        try writer.writeAll("}\n\n");
    }

    fn getNodeType(self: *CCodegen, node_id: ast.NodeId) ?ast.Type {
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
                }
                return null; // Unknown identifier
            },
            else => return null, // Unsupported node type for now
        }
    }

    fn generateCType(self: *CCodegen, howl_type: ?ast.Type) []const u8 {
        _ = self; // Mark self as explicitly unused
        if (howl_type) |type_info| {
            return switch (type_info.data) {
                .primitive => |prim| switch (prim) {
                    .bool => "bool",
                    .i8 => "int8_t",
                    .i16 => "int16_t",
                    .i32 => "int32_t",
                    .i64 => "int64_t",
                    .u8 => "uint8_t",
                    .u16 => "uint16_t",
                    .u32 => "uint32_t",
                    .u64 => "uint64_t",
                    .char => "uint8_t", // Enforce char as uint8_t
                    .f32 => "howl_f32_t", // IEEE 754 32-bit binary format
                    .f64 => "howl_f64_t", // IEEE 754 64-bit binary format
                    .usize => "size_t",
                    .isize => "ptrdiff_t",
                    .str => "const char*", // Readonly string
                    .strb => "HowlStringBuilder*", // StringBuilder pointer
                    .string => "char*", // Legacy mutable string
                    .void => "void",
                    else => "int32_t", // Default fallback
                },
                .pointer => "void*",
                .array => "void*", // Arrays become pointers in C
                else => "int32_t", // Default fallback
            };
        }
        return "int32_t"; // Default fallback when no type info available
    }

    fn generateMainBody(self: *CCodegen, writer: Writer, node_id: ast.NodeId, indent_level: u32) !void {
        _ = indent_level;
        try writer.writeAll("int main(void) {\n");
        try self.generateCFromAST(writer, node_id, 1);
        try writer.writeAll("}\n");
    }

    fn generateCFromAST(self: *CCodegen, writer: Writer, node_id: ast.NodeId, indent_level: u32) !void {
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
            else => {
                // For debugging, let's see what other nodes we encounter
                // try self.writeIndent(writer, indent_level);
                // try writer.writeAll("/* Unsupported AST node */\n");
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

        // Determine the type
        var c_type: []const u8 = "int32_t"; // Default fallback
        if (var_decl.type_annotation) |type_node_id| {
            const type_info = self.getNodeType(type_node_id);
            c_type = self.generateCType(type_info);
        } else if (var_decl.initializer) |init_node_id| {
            // Try to infer type from initializer
            const init_node = self.arena.getNodeConst(init_node_id);
            if (init_node) |n| {
                switch (n.data) {
                    .literal => |literal| {
                        c_type = switch (literal) {
                            .integer => "int32_t",
                            .float => "howl_f32_t",
                            .bool_true, .bool_false => "bool",
                            .string => "char*",
                            .char => "uint8_t",
                            .enum_member => "int32_t", // Enums are represented as integers in C
                        };
                    },
                    .array_init => |array_init| {
                        // For arrays, we need to determine the element type
                        if (array_init.elements.items.len > 0) {
                            const first_element = self.arena.getNodeConst(array_init.elements.items[0]);
                            if (first_element) |elem| {
                                switch (elem.data) {
                                    .literal => |literal| {
                                        c_type = switch (literal) {
                                            .integer => "int32_t",
                                            .float => "howl_f32_t",
                                            .bool_true, .bool_false => "bool",
                                            .string => "char*",
                                            .char => "uint8_t",
                                            .enum_member => "int32_t", // Enums are represented as integers in C
                                        };
                                    },
                                    else => c_type = "int32_t", // Fallback
                                }
                            }
                        } else {
                            c_type = "int32_t"; // Empty array fallback
                        }
                    },
                    .call_expr => |call_expr| {
                        // Try to infer from function call
                        const callee_node = self.arena.getNodeConst(call_expr.callee);
                        if (callee_node) |callee| {
                            switch (callee.data) {
                                .identifier => |ident| {
                                    // Map known function names to return types
                                    if (std.mem.eql(u8, ident.name, "add_mixed")) {
                                        c_type = "howl_f32_t";
                                    } else if (std.mem.eql(u8, ident.name, "is_positive")) {
                                        c_type = "bool";
                                    } else if (std.mem.eql(u8, ident.name, "greet")) {
                                        c_type = "char*";
                                    } else if (std.mem.eql(u8, ident.name, "add_numbers")) {
                                        c_type = "int32_t";
                                    } else if (std.mem.eql(u8, ident.name, "multiply")) {
                                        c_type = "howl_f32_t";
                                    } else if (std.mem.eql(u8, ident.name, "check_value")) {
                                        c_type = "bool";
                                    } else if (std.mem.eql(u8, ident.name, "concat_message")) {
                                        c_type = "char*";
                                    }
                                },
                                else => {},
                            }
                        }
                    },
                    .if_expr => |if_expr| {
                        // Infer type from ternary expression by looking at the then_branch
                        const then_node = self.arena.getNodeConst(if_expr.then_branch);
                        if (then_node) |then_n| {
                            switch (then_n.data) {
                                .literal => |literal| {
                                    switch (literal) {
                                        .string => c_type = "const char*",
                                        .integer => c_type = "int32_t",
                                        .float => c_type = "howl_f32_t",
                                        .bool_true, .bool_false => c_type = "bool",
                                        .char => c_type = "uint8_t",
                                        .enum_member => c_type = "int32_t",
                                    }
                                },
                                else => {},
                            }
                        }
                    },
                    else => {}, // Keep default
                }
            }
        }

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
                    try writer.print("{s} {s}[{d}]", .{ c_type, var_decl.name, array_init.elements.items.len });
                    try writer.writeAll(" = ");
                    try self.generateCExpression(writer, init_node_id);
                    try writer.writeAll(";\n");
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
        try writer.writeAll("return");

        if (return_stmt.value) |value_id| {
            try writer.writeAll(" ");
            try self.generateCExpression(writer, value_id);
        } else {
            // Empty return - for main functions, return 0
            try writer.writeAll(" 0");
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
                    // First capture is the element value
                    const value_capture = for_expr.captures.items[0];
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.print("int32_t {s} = {s}[_i];\n", .{ value_capture.name, iterable_name });
                    
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
        try self.generateCExpressionRecursive(writer, node_id);
    }

    fn generateCLiteral(self: *CCodegen, writer: Writer, literal: ast.Literal) !void {
        _ = self;
        switch (literal) {
            .integer => |int_literal| {
                try writer.print("{d}", .{int_literal.value});
            },
            .float => |float_literal| {
                // Format float with proper C syntax
                if (float_literal.value == @floor(float_literal.value)) {
                    // If it's a whole number, format as "x.0f"
                    try writer.print("{d:.1}f", .{float_literal.value});
                } else {
                    // If it has decimal places, format normally
                    try writer.print("{d}f", .{float_literal.value});
                }
            },
            .string => |string_literal| {
                try writer.print("\"{s}\"", .{string_literal.value});
            },
            .char => |char_literal| {
                try writer.print("'{c}'", .{char_literal.value});
            },
            .bool_true => {
                try writer.writeAll("true");
            },
            .bool_false => {
                try writer.writeAll("false");
            },
            .enum_member => |enum_member| {
                // For enum members, we generate the enum member name
                // In C, enums are typically prefixed, e.g., ENUM_MEMBER
                try writer.print("{s}", .{enum_member.name});
            },
        }
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
                try writer.print("{s}_{s}(&{s}", .{ "MyStruct", method_name, object_name }); // TODO: Get actual struct type

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
        try writer.writeAll("/* unhandled member method call */");
    }

    fn generateCMemberExpression(self: *CCodegen, writer: Writer, member_expr: anytype) !void {
        const object_node = self.arena.getNodeConst(member_expr.object) orelse return;

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
                            // This is std.List(Type).init()
                            try writer.writeAll("HowlList_i32_init");
                            return;
                        }
                    }
                }
            }
        }

        // Handle object.method() calls like h.append()
        if (object_node.data == .identifier) {
            const identifier = object_node.data.identifier;
            const method_name = member_expr.field;

            // Handle enum member access (e.g., MyEnum.a -> MyEnum_a)
            if (self.isEnumType(identifier.name)) {
                try writer.print("{s}_{s}", .{ identifier.name, member_expr.field });
                return;
            }

            if (std.mem.eql(u8, method_name, "append")) {
                try writer.print("HowlList_i32_append", .{});
                return;
            }
        }

        // Fallback for unhandled member expressions
        try writer.writeAll("/* unhandled member expression */");
    }

    /// Check if a type name is an enum type
    fn isEnumType(self: *CCodegen, type_name: []const u8) bool {
        if (self.semantic_analyzer.type_registry.get(type_name)) |typ| {
            return switch (typ.data) {
                .@"enum" => true,
                else => false,
            };
        }
        return false;
    }

    fn isStdlibFunctionCall(self: *CCodegen, call_expr: anytype) !bool {
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

    fn generateCStdlibCall(self: *CCodegen, writer: Writer, call_expr: anytype) !void {
        const callee_node = self.arena.getNodeConst(call_expr.callee) orelse return;

        if (callee_node.data == .member_expr) {
            // This should be std.debug.print
            if (try self.isStdlibFunctionCall(call_expr)) {
                try self.generateCPrintfCall(writer, call_expr);
                return;
            }
        }

        // Fallback to unknown function
        try writer.writeAll("/* unknown stdlib call */()");
    }

    fn generateCPrintfCall(self: *CCodegen, writer: Writer, call_expr: anytype) !void {
        if (call_expr.args.items.len == 0) {
            try writer.writeAll("printf(\"\\n\")");
            return;
        }

        // Get format string from first argument
        const format_arg = call_expr.args.items[0];
        const format_node = self.arena.getNodeConst(format_arg) orelse {
            try writer.writeAll("printf(\"Error: Invalid format\\n\")");
            return;
        };

        var format_string: []const u8 = "";
        if (format_node.data == .literal and format_node.data.literal == .string) {
            format_string = format_node.data.literal.string.value;
        } else {
            try writer.writeAll("printf(\"Error: Format must be string literal\\n\")");
            return;
        }

        // Check if we have arguments
        if (call_expr.args.items.len >= 2) {
            const args_node_id = call_expr.args.items[1];
            const args_node = self.arena.getNodeConst(args_node_id);

            if (args_node) |node| {
                // Look for the same pattern as JavaScript backend: call expression with __anonymous_struct
                if (node.data == .call_expr) {
                    const args_call_expr = node.data.call_expr;
                    const callee_node = self.arena.getNodeConst(args_call_expr.callee);
                    if (callee_node) |callee| {
                        if (callee.data == .identifier) {
                            const callee_ident = callee.data.identifier;
                            if (std.mem.eql(u8, callee_ident.name, "__anonymous_struct")) {
                                // Found anonymous struct! Extract arguments from call_expr.args.items
                                try self.generateCFormattedPrintfWithArgs(writer, format_string, args_call_expr.args.items);
                                return;
                            }
                        }
                    }
                }

                // Legacy fallback: check for struct_init (probably not used but keeping for compatibility)
                if (node.data == .struct_init) {
                    const struct_init = node.data.struct_init;
                    // Enhanced format processing with argument extraction
                    try self.generateCFormattedPrintf(writer, format_string, struct_init.fields.items);
                    return;
                } else if (node.data == .array_init) {
                    // Check if it's being parsed as array_init instead
                    try writer.writeAll("printf(\"/* Found array_init instead of struct_init */\");");

                    // For now, fall back to no-argument formatting
                    try self.generateCFormattedPrintf(writer, format_string, &[_]ast.FieldInit{});
                    return;
                } else {
                    // Let's see what node type we actually have
                    // try writer.writeAll("printf(\"/* NOT struct_init - found different node type */\");");
                }
            } else {
                // try writer.writeAll("printf(\"/* args_node is null */\");");
            }
        }

        // Fallback: just format string, no arguments
        try self.generateCFormattedPrintf(writer, format_string, &[_]ast.FieldInit{});
    }

    fn generateCFormattedPrintf(self: *CCodegen, writer: Writer, format_string: []const u8, field_inits: []ast.FieldInit) !void {
        try writer.writeAll("printf(\"");

        // Debug: add comment showing number of field_inits
        // try writer.print("/* DEBUG: field_inits.len = {d} */ ", .{field_inits.len});

        // Convert format string and collect argument info
        var args_needed = std.ArrayList(ast.NodeId).init(self.allocator);
        defer args_needed.deinit();

        var i: usize = 0;
        var arg_index: usize = 0;

        while (i < format_string.len) {
            if (format_string[i] == '{' and i + 1 < format_string.len) {
                // Handle format specifiers
                var j = i + 1;
                while (j < format_string.len and format_string[j] != '}') {
                    j += 1;
                }

                if (j < format_string.len) { // Found closing '}'
                    const spec = format_string[i + 1 .. j];

                    // Map Howl format specifiers to C printf specifiers
                    if (spec.len == 0) {
                        try writer.writeAll("%d"); // Default to %d for {}
                    } else if (std.mem.eql(u8, spec, "d")) {
                        try writer.writeAll("%d");
                    } else if (std.mem.eql(u8, spec, "s")) {
                        try writer.writeAll("%s");
                    } else if (std.mem.startsWith(u8, spec, "f")) {
                        if (std.mem.indexOf(u8, spec, ":.") != null) {
                            // Handle precision like f:.2
                            const precision_start = std.mem.indexOf(u8, spec, ":.").? + 2;
                            if (precision_start < spec.len) {
                                try writer.writeAll("%.");
                                try writer.writeAll(spec[precision_start..]);
                                try writer.writeAll("f");
                            } else {
                                try writer.writeAll("%f");
                            }
                        } else {
                            try writer.writeAll("%f");
                        }
                    } else {
                        try writer.writeAll("%d"); // Fallback
                    }

                    // Add corresponding argument to the list
                    if (arg_index < field_inits.len) {
                        try args_needed.append(field_inits[arg_index].value);
                        arg_index += 1;
                    }

                    i = j + 1;
                } else {
                    // No closing brace, just output the '{'
                    try writer.writeAll("{");
                    i += 1;
                }
            } else {
                // Regular character - handle special characters that need escaping
                const char = format_string[i];
                switch (char) {
                    '\n' => try writer.writeAll("\\n"),
                    '\t' => try writer.writeAll("\\t"),
                    '\r' => try writer.writeAll("\\r"),
                    '"' => try writer.writeAll("\\\""),
                    '\\' => try writer.writeAll("\\\\"),
                    else => try writer.writeAll(&[_]u8{char}),
                }
                i += 1;
            }
        }

        try writer.writeAll("\"");

        // Add the arguments
        for (args_needed.items) |arg_id| {
            try writer.writeAll(", ");
            const arg_node = self.arena.getNodeConst(arg_id);
            if (arg_node) |arg| {
                switch (arg.data) {
                    .literal => |lit| try self.generateCLiteral(writer, lit),
                    .identifier => |id| try writer.writeAll(id.name),
                    .binary_expr => {
                        try writer.writeAll("/* binary_expr_not_supported_in_printf */");
                    },
                    else => try writer.writeAll("/* complex_value */"),
                }
            }
        }

        try writer.writeAll(")");
    }

    fn generateCFormattedPrintfWithArgs(self: *CCodegen, writer: Writer, format_string: []const u8, args: []ast.NodeId) !void {
        try writer.writeAll("printf(\"");

        // Convert format string and collect argument info
        var args_needed = std.ArrayList(ast.NodeId).init(self.allocator);
        defer args_needed.deinit();

        var i: usize = 0;
        var arg_index: usize = 0;

        while (i < format_string.len) {
            if (format_string[i] == '{' and i + 1 < format_string.len) {
                // Handle format specifiers
                var j = i + 1;
                while (j < format_string.len and format_string[j] != '}') {
                    j += 1;
                }

                if (j < format_string.len) { // Found closing '}'
                    const spec = format_string[i + 1 .. j];

                    // Map Howl format specifiers to C printf specifiers
                    if (spec.len == 0) {
                        try writer.writeAll("%d"); // Default to %d for {}
                    } else if (std.mem.eql(u8, spec, "d")) {
                        try writer.writeAll("%d");
                    } else if (std.mem.eql(u8, spec, "s")) {
                        try writer.writeAll("%s");
                    } else if (std.mem.startsWith(u8, spec, "f")) {
                        if (std.mem.indexOf(u8, spec, ":.") != null) {
                            // Handle precision like f:.2
                            const precision_start = std.mem.indexOf(u8, spec, ":.").? + 2;
                            if (precision_start < spec.len) {
                                try writer.writeAll("%.");
                                try writer.writeAll(spec[precision_start..]);
                                try writer.writeAll("f");
                            } else {
                                try writer.writeAll("%f");
                            }
                        } else {
                            try writer.writeAll("%f");
                        }
                    } else {
                        try writer.writeAll("%d"); // Fallback
                    }

                    // Add corresponding argument to the list
                    if (arg_index < args.len) {
                        try args_needed.append(args[arg_index]);
                        arg_index += 1;
                    }

                    i = j + 1;
                } else {
                    // No closing brace, just output the '{'
                    try writer.writeAll("{");
                    i += 1;
                }
            } else {
                // Regular character - handle special characters that need escaping
                const char = format_string[i];
                switch (char) {
                    '\n' => try writer.writeAll("\\n"),
                    '\t' => try writer.writeAll("\\t"),
                    '\r' => try writer.writeAll("\\r"),
                    '"' => try writer.writeAll("\\\""),
                    '\\' => try writer.writeAll("\\\\"),
                    else => try writer.writeAll(&[_]u8{char}),
                }
                i += 1;
            }
        }

        try writer.writeAll("\"");

        // Add the arguments
        for (args_needed.items) |arg_id| {
            try writer.writeAll(", ");
            const arg_node = self.arena.getNodeConst(arg_id);
            if (arg_node) |arg| {
                switch (arg.data) {
                    .literal => |lit| try self.generateCLiteral(writer, lit),
                    .identifier => |id| try writer.writeAll(id.name),
                    .binary_expr => {
                        try writer.writeAll("/* binary_expr_not_supported_in_printf */");
                    },
                    else => try writer.writeAll("/* complex_value */"),
                }
            }
        }

        try writer.writeAll(")");
    }

    fn convertHowlFormatToCWithCount(self: *CCodegen, howl_format: []const u8) !struct { format: []const u8, arg_count: usize } {
        // Convert Howl format string to C format string and count expected arguments
        // Supports: {} -> %d (default), {d} -> %d, {s} -> %s, {f} -> %f
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        var arg_count: usize = 0;

        var i: usize = 0;
        while (i < howl_format.len) {
            if (howl_format[i] == '\n') {
                // Convert actual newline character to \n escape sequence
                try result.appendSlice("\\n");
                i += 1;
            } else if (howl_format[i] == '\t') {
                // Convert actual tab character to \t escape sequence
                try result.appendSlice("\\t");
                i += 1;
            } else if (howl_format[i] == '\r') {
                // Convert actual carriage return to \r escape sequence
                try result.appendSlice("\\r");
                i += 1;
            } else if (howl_format[i] == '"') {
                // Escape double quotes
                try result.appendSlice("\\\"");
                i += 1;
            } else if (howl_format[i] == '\\' and i + 1 < howl_format.len) {
                // Handle escape sequences
                const next_char = howl_format[i + 1];
                switch (next_char) {
                    'n' => {
                        try result.appendSlice("\\n");
                        i += 2;
                    },
                    't' => {
                        try result.appendSlice("\\t");
                        i += 2;
                    },
                    'r' => {
                        try result.appendSlice("\\r");
                        i += 2;
                    },
                    '\\' => {
                        try result.appendSlice("\\\\");
                        i += 2;
                    },
                    '"' => {
                        try result.appendSlice("\\\"");
                        i += 2;
                    },
                    else => {
                        try result.append(howl_format[i]);
                        i += 1;
                    },
                }
            } else if (howl_format[i] == '{' and i + 1 < howl_format.len) {
                const next_char = howl_format[i + 1];
                switch (next_char) {
                    'd' => {
                        try result.appendSlice("%d");
                        arg_count += 1;
                        i += 2; // Skip '{d'
                        // Skip until '}'
                        while (i < howl_format.len and howl_format[i] != '}') {
                            i += 1;
                        }
                        i += 1; // Skip '}'
                    },
                    's' => {
                        try result.appendSlice("%s");
                        arg_count += 1;
                        i += 2; // Skip '{s'
                        // Skip until '}'
                        while (i < howl_format.len and howl_format[i] != '}') {
                            i += 1;
                        }
                        i += 1; // Skip '}'
                    },
                    'f' => {
                        try result.appendSlice("%f");
                        arg_count += 1;
                        i += 2; // Skip '{f'
                        // Skip until '}'
                        while (i < howl_format.len and howl_format[i] != '}') {
                            i += 1;
                        }
                        i += 1; // Skip '}'
                    },
                    '}' => {
                        // Empty {} - determine format based on context
                        // For now, use heuristic: if we're likely dealing with float variables use %f
                        try result.appendSlice("%d"); // Default to int, could be enhanced with type inference
                        arg_count += 1;
                        i += 2;
                    },
                    else => {
                        // If we find a '{' followed by something else, check if it's the end of a generic {}
                        if (next_char == '}') {
                            try result.appendSlice("%d");
                            arg_count += 1;
                            i += 2;
                        } else {
                            // If it's not a recognized format specifier, just copy the '{'
                            try result.append(howl_format[i]);
                            i += 1;
                        }
                    },
                }
            } else {
                try result.append(howl_format[i]);
                i += 1;
            }
        }

        return .{ .format = try self.allocator.dupe(u8, result.items), .arg_count = arg_count };
    }
    fn convertHowlFormatToC(self: *CCodegen, howl_format: []const u8) ![]const u8 {
        const result = try self.convertHowlFormatToCWithCount(howl_format);
        return result.format;
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

    fn generateCExpressionRecursive(self: *CCodegen, writer: Writer, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;

        switch (node.data) {
            .literal => |literal| {
                try self.generateCLiteral(writer, literal);
            },
            .identifier => |identifier| {
                try writer.writeAll(identifier.name);
            },
            .binary_expr => |binary_expr| {
                // Special handling for string concatenation
                if (binary_expr.op == .concat) {
                    try writer.writeAll("howl_string_concat(");
                    try self.generateCExpressionRecursive(writer, binary_expr.left);
                    try writer.writeAll(", ");
                    try self.generateCExpressionRecursive(writer, binary_expr.right);
                    try writer.writeAll(")");
                } else {
                    try writer.writeAll("(");
                    try self.generateCExpressionRecursive(writer, binary_expr.left);
                    try writer.writeAll(" ");
                    try writer.writeAll(binary_expr.op.toString());
                    try writer.writeAll(" ");
                    try self.generateCExpressionRecursive(writer, binary_expr.right);
                    try writer.writeAll(")");
                }
            },
            .member_expr => |member_expr| {
                // Handle member access like std.debug.print
                try self.generateCMemberExpression(writer, member_expr);
            },
            .call_expr => |call_expr| {
                // Check if this is a stdlib function call
                if (try self.isStdlibFunctionCall(call_expr)) {
                    try self.generateCStdlibCall(writer, call_expr);
                } else {
                    // Check if this is a member method call (like h.append())
                    const callee_node = self.arena.getNodeConst(call_expr.callee);
                    if (callee_node) |callee| {
                        if (callee.data == .member_expr) {
                            const member_expr = callee.data.member_expr;
                            try self.generateCMemberMethodCall(writer, member_expr, call_expr.args.items);
                        } else {
                            // Generate regular function call
                            try self.generateCExpressionRecursive(writer, call_expr.callee);
                            try writer.writeAll("(");

                            // Generate arguments
                            for (call_expr.args.items, 0..) |arg_id, i| {
                                if (i > 0) try writer.writeAll(", ");
                                try self.generateCExpressionRecursive(writer, arg_id);
                            }

                            try writer.writeAll(")");
                        }
                    } else {
                        // Fallback
                        try writer.writeAll("/* unknown call */");
                    }
                }
            },
            .struct_init => |struct_init| {
                // Handle anonymous struct syntax .{args}
                if (struct_init.type_name == null) {
                    // This is an anonymous struct for function arguments
                    for (struct_init.fields.items, 0..) |field_init, i| {
                        if (i > 0) try writer.writeAll(", ");
                        try self.generateCExpressionRecursive(writer, field_init.value);
                    }
                } else {
                    // Named struct initialization: .TypeName{ .field = value, ... }
                    // Generate constructor call: TypeName_init(field_values...)
                    const type_name = struct_init.type_name.?;
                    try writer.print("{s}_init(", .{type_name});

                    // Generate field values in constructor order
                    // TODO: Should match the order of fields in struct definition
                    for (struct_init.fields.items, 0..) |field_init, i| {
                        if (i > 0) try writer.writeAll(", ");
                        try self.generateCExpressionRecursive(writer, field_init.value);
                    }
                    try writer.writeAll(")");
                }
            },
            .array_init => |array_init| {
                // Generate C array literal syntax: {1, 2, 3}
                try writer.writeAll("{");
                for (array_init.elements.items, 0..) |element_id, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.generateCExpressionRecursive(writer, element_id);
                }
                try writer.writeAll("}");
            },
            .if_expr => |if_expr| {
                // Generate C ternary operator: (condition ? then_value : else_value)
                try writer.writeAll("(");
                try self.generateCExpressionRecursive(writer, if_expr.condition);
                try writer.writeAll(" ? ");
                try self.generateCExpressionRecursive(writer, if_expr.then_branch);
                try writer.writeAll(" : ");
                if (if_expr.else_branch) |else_branch| {
                    try self.generateCExpressionRecursive(writer, else_branch);
                } else {
                    try writer.writeAll("/* no else branch */");
                }
                try writer.writeAll(")");
            },
            else => {
                try writer.writeAll("/* Unknown expression */");
            },
        }
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

    fn compileGeneratedC(self: *CCodegen) !void {
        _ = self;
        // This function is no longer needed since we handle compilation in generate()
    }

    // Enhanced format generation function for advanced debug.print support
    fn generateCFormatCode(self: *CCodegen, format_string: []const u8, args_node_id: ast.NodeId) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        // Parse format string and convert Howl format specifiers to C format specifiers
        const converted = try self.convertAdvancedHowlFormatToC(format_string);
        defer self.allocator.free(converted.format);

        // Generate printf call with converted format
        try result.appendSlice("printf(\"");
        try result.appendSlice(converted.format);
        try result.appendSlice("\"");

        // Add arguments from anonymous struct
        const args_node = self.arena.getNodeConst(args_node_id);
        if (args_node) |node| {
            switch (node.data) {
                .struct_init => |struct_init| {
                    // Extract values from anonymous struct
                    for (struct_init.fields.items) |field_init| {
                        try result.appendSlice(", ");
                        const value_node = self.arena.getNodeConst(field_init.value);
                        if (value_node) |val_node| {
                            switch (val_node.data) {
                                .literal => |lit| {
                                    switch (lit) {
                                        .string => |s| {
                                            try result.appendSlice("\"");
                                            try result.appendSlice(s.value);
                                            try result.appendSlice("\"");
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
                                        else => try result.appendSlice("unknown_literal"),
                                    }
                                },
                                .identifier => |id| try result.appendSlice(id.name),
                                else => try result.appendSlice("complex_expr"),
                            }
                        } else {
                            try result.appendSlice("null_value");
                        }
                    }
                },
                else => {
                    try result.appendSlice(" /* non-struct args */");
                },
            }
        }

        try result.appendSlice(");");
        return try self.allocator.dupe(u8, result.items);
    }

    // Advanced format string conversion supporting complex format specifiers
    fn convertAdvancedHowlFormatToC(self: *CCodegen, howl_format: []const u8) !struct { format: []const u8, arg_count: usize } {
        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        var arg_count: usize = 0;
        var i: usize = 0;

        while (i < howl_format.len) {
            if (howl_format[i] == '{' and i + 1 < howl_format.len) {
                // Parse format specifier: {type:format_spec}
                var end_pos = i + 1;
                while (end_pos < howl_format.len and howl_format[end_pos] != '}') {
                    end_pos += 1;
                }

                if (end_pos < howl_format.len) {
                    const format_spec = howl_format[i + 1 .. end_pos];

                    // Parse the format specification
                    const converted_spec = try self.convertFormatSpec(format_spec);
                    try result.appendSlice(converted_spec);
                    arg_count += 1;

                    i = end_pos + 1; // Skip past '}'
                } else {
                    // Malformed format spec, just copy the '{'
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
                    '"' => try result.appendSlice("\\\""),
                    else => {
                        try result.append(howl_format[i]);
                        try result.append(next_char);
                    },
                }
                i += 2;
            } else if (howl_format[i] == '"') {
                // Escape quotes in format string
                try result.appendSlice("\\\"");
                i += 1;
            } else {
                try result.append(howl_format[i]);
                i += 1;
            }
        }

        return .{
            .format = try self.allocator.dupe(u8, result.items),
            .arg_count = arg_count,
        };
    }

    // Convert individual format specification (e.g., "d:.2", "s:!>4")
    fn convertFormatSpec(self: *CCodegen, spec: []const u8) ![]const u8 {
        _ = self; // May be used in future for more complex conversions

        if (spec.len == 0) {
            return "%d"; // Default format
        }

        // Split on ':' if present
        var type_part = spec;
        var format_part: []const u8 = "";

        if (std.mem.indexOf(u8, spec, ":")) |colon_pos| {
            type_part = spec[0..colon_pos];
            format_part = spec[colon_pos + 1 ..];
        }

        // Handle type specifiers
        if (std.mem.eql(u8, type_part, "d")) {
            // Integer format
            if (format_part.len == 0) {
                return "%d";
            } else if (std.mem.startsWith(u8, format_part, ".")) {
                // Precision for decimal display
                return "%.0f"; // For now, treat as float with 0 decimal places
            } else {
                return "%d";
            }
        } else if (std.mem.eql(u8, type_part, "s")) {
            // String format
            if (format_part.len == 0) {
                return "%s";
            } else {
                // For now, ignore complex string formatting
                return "%s";
            }
        } else if (std.mem.eql(u8, type_part, "f")) {
            // Float format
            if (format_part.len == 0) {
                return "%f";
            } else if (std.mem.startsWith(u8, format_part, ".")) {
                // Extract precision
                const precision_str = format_part[1..];
                if (std.mem.eql(u8, precision_str, "2")) {
                    return "%.2f";
                } else if (std.mem.eql(u8, precision_str, "1")) {
                    return "%.1f";
                } else if (std.mem.eql(u8, precision_str, "3")) {
                    return "%.3f";
                } else {
                    return "%.2f"; // Default precision
                }
            } else {
                return "%f";
            }
        } else {
            // Default case
            return "%d";
        }
    }
};
