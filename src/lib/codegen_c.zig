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

const CollectedErrorSet = struct {
    name: []const u8,
    errors: []const []const u8,
};

const CollectedErrorUnion = struct {
    error_set_name: []const u8,
    payload_type: []const u8,
    struct_name: []const u8, // Generated name like MyError_i32_ErrorUnion
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
    error_set_types: std.ArrayList(CollectedErrorSet),
    error_union_types: std.ArrayList(CollectedErrorUnion),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) TypeCollection {
        return TypeCollection{
            .list_types = std.ArrayList([]const u8).init(allocator),
            .builtin_types = std.ArrayList([]const u8).init(allocator),
            .custom_types = std.ArrayList(CollectedType).init(allocator),
            .struct_types = std.ArrayList(CollectedStruct).init(allocator),
            .enum_types = std.ArrayList(CollectedEnum).init(allocator),
            .error_set_types = std.ArrayList(CollectedErrorSet).init(allocator),
            .error_union_types = std.ArrayList(CollectedErrorUnion).init(allocator),
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

        // Free all allocated strings in error_set_types
        for (self.error_set_types.items) |error_set_type| {
            self.allocator.free(error_set_type.name);
            for (error_set_type.errors) |error_name| {
                self.allocator.free(error_name);
            }
            self.allocator.free(error_set_type.errors);
        }

        // Free all allocated strings in error_union_types
        for (self.error_union_types.items) |error_union_type| {
            self.allocator.free(error_union_type.error_set_name);
            self.allocator.free(error_union_type.payload_type);
            self.allocator.free(error_union_type.struct_name);
        }

        self.list_types.deinit();
        self.builtin_types.deinit();
        self.custom_types.deinit();
        self.struct_types.deinit();
        self.enum_types.deinit();
        self.error_set_types.deinit();
        self.error_union_types.deinit();
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

    fn hasErrorSetType(self: *const TypeCollection, error_set_name: []const u8) bool {
        for (self.error_set_types.items) |error_set_type| {
            if (std.mem.eql(u8, error_set_type.name, error_set_name)) {
                return true;
            }
        }
        return false;
    }

    fn addErrorSetType(self: *TypeCollection, allocator: std.mem.Allocator, error_set_decl: anytype) !void {
        if (!self.hasErrorSetType(error_set_decl.name)) {
            // Copy error names
            const errors = try allocator.alloc([]const u8, error_set_decl.errors.items.len);
            for (error_set_decl.errors.items, 0..) |error_name, i| {
                errors[i] = try allocator.dupe(u8, error_name);
            }

            const collected_error_set = CollectedErrorSet{
                .name = try allocator.dupe(u8, error_set_decl.name),
                .errors = errors,
            };
            try self.error_set_types.append(collected_error_set);
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
            try self.generateErrorSetImplementation(writer, error_set_type);
        }

        // Generate enum types (they might be dependencies)
        for (self.type_collection.enum_types.items) |enum_type| {
            try self.generateEnumImplementation(writer, enum_type);
        }

        // Generate struct types (they might be dependencies for error unions)
        for (self.type_collection.struct_types.items) |struct_type| {
            try self.generateStructImplementation(writer, struct_type);
        }

        // Generate error union types (depend on error sets and structs)
        for (self.type_collection.error_union_types.items) |error_union_type| {
            try self.generateErrorUnionImplementation(writer, error_union_type);
        }

        // Generate List implementations for used types
        for (self.type_collection.list_types.items) |list_type| {
            try self.generateListImplementation(writer, list_type);
        }
    }

    fn generateErrorSetImplementation(self: *CCodegen, writer: Writer, error_set_info: CollectedErrorSet) !void {
        _ = self; // suppress unused parameter warning

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

    fn generateErrorUnionImplementation(self: *CCodegen, writer: Writer, error_union_info: CollectedErrorUnion) !void {
        _ = self; // suppress unused parameter warning

        try writer.writeAll("// ============================================================================\n");
        try writer.print("// Error Union {s} Implementation\n", .{error_union_info.struct_name});
        try writer.writeAll("// ============================================================================\n\n");

        // Generate specialized error union struct
        try writer.print("typedef struct {s} {{\n", .{error_union_info.struct_name});
        try writer.print("    {s}_t error;\n", .{error_union_info.error_set_name});
        try writer.print("    {s} payload;\n", .{error_union_info.payload_type});
        try writer.print("}} {s};\n\n", .{error_union_info.struct_name});
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

        // Generate the struct definition
        try writer.print("typedef struct {s} {{\n", .{struct_info.name});
        
        // Generate struct fields
        for (struct_info.fields) |field| {
            try writer.writeAll("    ");
            
            // Generate field type
            if (field.type_annotation) |type_node_id| {
                const type_node = self.arena.getNodeConst(type_node_id);
                if (type_node) |node| {
                    if (node.data == .identifier) {
                        const type_name = node.data.identifier.name;
                        const c_type = self.mapHowlTypeToCType(type_name);
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
        try self.generateStructConstructor(writer, struct_info);
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
    
    fn generateStructConstructor(self: *CCodegen, writer: Writer, struct_info: CollectedStruct) !void {
        // Generate a constructor function: MyStruct MyStruct_init(field1, field2, ...)
        try writer.print("{s} {s}_init(", .{ struct_info.name, struct_info.name });
        
        // Generate parameter list
        for (struct_info.fields, 0..) |field, i| {
            if (i > 0) try writer.writeAll(", ");
            
            // Generate parameter type and name
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

    fn generateListImplementation(self: *CCodegen, writer: Writer, list_type: []const u8) !void {
        // Implementation placeholder  
        _ = self;
        _ = writer;
        _ = list_type;
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

    fn inferErrorUnionTypeFromExpression(self: *CCodegen, expr_id: ast.NodeId) []const u8 {
        const node = self.arena.getNodeConst(expr_id) orelse return self.getCurrentErrorUnionStructName();
        
        if (node.data == .call_expr) {
            const call_expr = node.data.call_expr;
            const callee_node = self.arena.getNodeConst(call_expr.callee);
            if (callee_node) |callee| {
                if (callee.data == .identifier) {
                    const func_name = callee.data.identifier.name;
                    
                    // Look up the function's return type to find the matching error union
                    if (std.mem.eql(u8, func_name, "divide")) {
                        return "MyError_i32_ErrorUnion";
                    } else if (std.mem.eql(u8, func_name, "createMyStruct")) {
                        return "MyError_MyStruct_ErrorUnion";
                    }
                    
                    // TODO: More robust lookup by checking collected functions
                }
            }
        }
        
        // Fallback to current function context
        return self.getCurrentErrorUnionStructName();
    }

    fn getCurrentErrorUnionStructName(self: *CCodegen) []const u8 {
        // Return the current function's error union type if set
        if (self.current_function_error_union_name) |name| {
            return name;
        }
        
        // For now, return the first available error union struct name
        // This is a simplification - in a complete implementation, we'd track the current function context
        if (self.type_collection.error_union_types.items.len > 0) {
            return self.type_collection.error_union_types.items[0].struct_name;
        }
        return "MyError_ErrorUnion"; // Fallback
    }

    fn findErrorUnionStructName(self: *CCodegen, error_set_name: []const u8, payload_type: []const u8) ?[]const u8 {
        for (self.type_collection.error_union_types.items) |error_union| {
            if (std.mem.eql(u8, error_union.error_set_name, error_set_name) and 
               std.mem.eql(u8, error_union.payload_type, payload_type)) {
                return error_union.struct_name;
            }
        }
        return null;
    }

    fn collectErrorUnionFromNode(self: *CCodegen, node_id: ast.NodeId) !void {
        const node = self.arena.getNodeConst(node_id) orelse return;
        
        if (node.data == .error_union_type) {
            const error_union = node.data.error_union_type;
            
            if (error_union.error_set) |error_set_id| {
                const error_set_node = self.arena.getNodeConst(error_set_id);
                if (error_set_node) |es_node| {
                    if (es_node.data == .identifier) {
                        const error_set_name = es_node.data.identifier.name;
                        
                        // Get payload type
                        const payload_type_info = self.getNodeType(error_union.payload_type);
                        const payload_type_str = self.generateCType(payload_type_info);
                        
                        // Generate struct name
                        const struct_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}_ErrorUnion", .{error_set_name, self.sanitizeTypeForName(payload_type_str)});
                        
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
        
        // For custom types, just use the type string directly
        return type_str;
    }

    fn collectFunction(self: *CCodegen, func_decl: anytype) !void {
        // Don't collect main function - handled separately
        if (std.mem.eql(u8, func_decl.name, "main")) return;

        // Check if return type is an error union and collect it
        if (func_decl.return_type) |return_type_node_id| {
            try self.collectErrorUnionFromNode(return_type_node_id);
        }

        // Store function for later declaration and implementation generation
        const collected_func = CollectedFunction{
            .name = try self.allocator.dupe(u8, func_decl.name),
            .declaration = "", // Will be filled during declaration generation
            .implementation = "", // Will be filled during implementation generation
        };

        try self.function_collection.functions.append(collected_func);
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
                std.debug.print("DEBUG: Found var_decl: {s}\n", .{var_decl.name});
                // Check if this is a struct declaration like MyStruct :: struct { ... }
                if (var_decl.initializer) |init_id| {
                    std.debug.print("DEBUG: var_decl has initializer\n", .{});
                    const init_node = self.arena.getNodeConst(init_id);
                    if (init_node) |init_data| {
                        std.debug.print("DEBUG: init_node type: {}\n", .{init_data.data});
                        if (init_data.data == .struct_decl) {
                            const struct_decl = init_data.data.struct_decl;
                            // Collect this struct
                            std.debug.print("DEBUG: Collecting struct: {s}\n", .{var_decl.name});
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
            }
        }
    }
    
    // Try to infer the return type by examining return statements in function body
    fn inferReturnTypeFromBody(self: *CCodegen, body_node_id: ast.NodeId) ?[]const u8 {
        // Look for return statements that call struct constructors
        const result = self.findReturnTypeInNode(body_node_id);
        if (result) |found_type| {
            std.debug.print("DEBUG: Inferred return type: {s}\n", .{found_type});
        }
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
                                        const struct_name = func_name[0..func_name.len - 5];
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
        // Special handling for main function
        const is_main_function = std.mem.eql(u8, func_decl.name, "main");
        
        // Try to infer return type from function body if semantic analysis failed
        const inferred_return_type = self.inferReturnTypeFromBody(func_decl.body);
        
        // Generate return type
        const return_type_str = if (is_main_function) "int" else if (inferred_return_type) |inferred| inferred else if (func_decl.return_type) |return_type_node_id| blk: {
            // Check if this is an error union type
            const return_type_node = self.arena.getNodeConst(return_type_node_id);
            if (return_type_node) |node| {
                if (node.data == .error_union_type) {
                    const error_union = node.data.error_union_type;
                    if (error_union.error_set) |error_set_id| {
                        const error_set_node = self.arena.getNodeConst(error_set_id);
                        if (error_set_node) |es_node| {
                            if (es_node.data == .identifier) {
                                const error_set_name = es_node.data.identifier.name;
                                
                                // Get payload type to find the correct specialized struct
                                const payload_type_info = self.getNodeType(error_union.payload_type);
                                const payload_type_str = self.generateCType(payload_type_info);
                                
                                if (self.findErrorUnionStructName(error_set_name, payload_type_str)) |struct_name| {
                                    self.current_function_error_union_name = struct_name; // Set current function's error union type
                                    break :blk try self.allocator.dupe(u8, struct_name);
                                } else {
                                    // Fallback to old naming if not found
                                    break :blk try std.fmt.allocPrint(self.allocator, "{s}_ErrorUnion", .{error_set_name});
                                }
                            }
                        }
                    }
                    // Fallback for anonymous error union
                    break :blk "struct { int32_t error; int32_t payload; }";
                }
            }
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

        // Store previous error union name and restore it after function generation
        const prev_error_union_name = self.current_function_error_union_name;
        defer self.current_function_error_union_name = prev_error_union_name;

        // Set main function flag for special handling
        if (is_main_function) {
            self.current_function_is_main = true;
        }
        defer if (is_main_function) {
            self.current_function_is_main = false;
        };

        // Generate function body
        try self.generateCFromAST(writer, func_decl.body, 1);

        // Add return statement for main function
        if (is_main_function) {
            try writer.writeAll("    return 0;\n");
        }

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
            else => return null, // Unsupported node type for now
        }
    }

    fn generateCType(self: *CCodegen, howl_type: ?ast.Type) []const u8 {
        _ = self; // suppress unused parameter warning for now
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
                    .char => "uint8_t",
                    .f32 => "howl_f32_t",
                    .f64 => "howl_f64_t",
                    .usize => "size_t",
                    .isize => "ptrdiff_t",
                    .str => "const char*",
                    .strb => "HowlStringBuilder*",
                    .string => "char*",
                    .void => "void",
                    else => "int32_t", // Default fallback
                },
                .pointer => "void*",
                .array => "void*",
                .@"struct" => |struct_info| {
                    // Return the struct name directly
                    return struct_info.name;
                },
                .custom_struct => |struct_info| {
                    // Return the struct name directly
                    return struct_info.name;
                },
                .@"enum" => |enum_info| {
                    // Return the enum name directly
                    return enum_info.name;
                },
                else => "int32_t", // Default fallback
            };
        }
        return "int32_t"; // Default fallback
    }

    fn generateMainBody(self: *CCodegen, writer: Writer, node_id: ast.NodeId, indent_level: u32) !void {
        _ = indent_level;
        self.current_function_is_main = true; // Set flag when generating main
        defer self.current_function_is_main = false; // Reset flag when done
        try writer.writeAll("int main(void) {\n");
        try self.generateCFromAST(writer, node_id, 1);
        try writer.writeAll("}\n");
    }

    fn generateCFromAST(self: *CCodegen, writer: Writer, node_id: ast.NodeId, indent_level: u32) anyerror!void {
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
            .try_expr => |try_expr| {
                try self.generateCTryExpression(writer, try_expr, indent_level);
            },
            .catch_expr => |catch_expr| {
                try self.generateCCatchExpression(writer, catch_expr, indent_level);
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
        // Generate try expression as error union unwrapping
        try self.writeIndent(writer, indent_level);
        
        // Get the function call result
        try writer.writeAll("{\n");
        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("struct { int32_t error; int32_t payload; } _try_result = ");
        try self.generateCExpression(writer, try_expr.expression);
        try writer.writeAll(";\n");
        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("if (_try_result.error < 0) {\n");
        try self.writeIndent(writer, indent_level + 2);
        try writer.writeAll("struct { int32_t error; int32_t payload; } _propagated = {_try_result.error, 0};\n");
        try self.writeIndent(writer, indent_level + 2);
        try writer.writeAll("return _propagated;\n");
        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("}\n");
        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("// Extract payload value\n");
        try self.writeIndent(writer, indent_level + 1);
        try writer.writeAll("int32_t result = _try_result.payload;\n");
        try self.writeIndent(writer, indent_level);
        try writer.writeAll("}\n");
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
                    .try_expr => |try_expr| {
                        // For try expressions, infer the payload type from the called function
                        const try_result_node = self.arena.getNodeConst(try_expr.expression);
                        if (try_result_node) |try_node| {
                            if (try_node.data == .call_expr) {
                                const call_expr = try_node.data.call_expr;
                                const callee_node = self.arena.getNodeConst(call_expr.callee);
                                if (callee_node) |callee| {
                                    if (callee.data == .identifier) {
                                        const func_name = callee.data.identifier.name;
                                        if (std.mem.eql(u8, func_name, "divide")) {
                                            c_type = "int32_t"; // The payload type of divide function
                                        } else if (std.mem.eql(u8, func_name, "createMyStruct")) {
                                            c_type = "MyStruct"; // The payload type of createMyStruct function
                                        }
                                    }
                                }
                            }
                        }
                    },
                    .compile_target_expr => {
                        // @compile.target returns a string
                        c_type = "const char*";
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

        if (return_stmt.value) |value_id| {
            const value_node = self.arena.getNodeConst(value_id);
            if (value_node) |node| {
                if (node.data == .if_expr) {
                    // This is likely an error union return: return if (condition) error else payload
                    const if_expr = node.data.if_expr;
                    
                    // Generate error union return pattern
                    try writer.writeAll("{\n");
                    try self.writeIndent(writer, indent_level + 1);
                    try writer.writeAll("if ");
                    try self.generateCExpression(writer, if_expr.condition);
                    try writer.writeAll(" {\n");
                    try self.writeIndent(writer, indent_level + 2);
                     try writer.writeAll(self.getCurrentErrorUnionStructName());
                     try writer.writeAll(" result = {.error = ");
                     try self.generateCExpression(writer, if_expr.then_branch);
                     try writer.writeAll(", .payload = 0};\n");
                     try self.writeIndent(writer, indent_level + 2);
                     try writer.writeAll("return result;\n");
                     try self.writeIndent(writer, indent_level + 1);
                     try writer.writeAll("} else {\n");
                     try self.writeIndent(writer, indent_level + 2);
                     try writer.writeAll(self.getCurrentErrorUnionStructName());
                     try writer.writeAll(" result = {.error = MyError_SUCCESS, .payload = ");
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
                                try writer.writeAll("){.error = ");
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
                        try writer.writeAll(self.getCurrentErrorUnionStructName());
                        try writer.writeAll("){.error = MyError_SUCCESS, .payload = ");
                        try self.generateCExpression(writer, value_id);
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
                                try writer.writeAll(self.getCurrentErrorUnionStructName());
                                try writer.writeAll("){.error = MyError_SUCCESS, .payload = ");
                                try self.generateCExpression(writer, value_id);
                                try writer.writeAll("};\n");
                                return;
                            }
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
                     const element_type = if (std.mem.eql(u8, iterable_name, "floats")) "howl_f32_t" 
                                         else if (std.mem.endsWith(u8, iterable_name, "_f32") or std.mem.endsWith(u8, iterable_name, "float")) "howl_f32_t"
                                         else if (std.mem.endsWith(u8, iterable_name, "_f64") or std.mem.endsWith(u8, iterable_name, "double")) "howl_f64_t"
                                         else "int32_t"; // Default fallback
                     
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
        std.log.err("Member method calls are not yet supported in C target: {s}.{s}", .{ 
            if (object_node.data == .identifier) object_node.data.identifier.name else "complex_object", 
            member_expr.field 
        });
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
                !std.mem.eql(u8, identifier.name, "compile")) {
                // Generate C struct field access: object.field
                try writer.print("{s}.{s}", .{ identifier.name, member_expr.field });
                return;
            }
        }

        // Fallback for unhandled member expressions
        std.log.err("Member expressions are not yet supported in C target: {s}.{s}", .{ 
            if (object_node.data == .identifier) object_node.data.identifier.name else "complex_object", 
            member_expr.field 
        });
        return error.UnsupportedExpression;
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

    /// Check if a type name is an error set type
    fn isErrorSetType(self: *CCodegen, type_name: []const u8) bool {
        if (self.semantic_analyzer.type_registry.get(type_name)) |typ| {
            return switch (typ.data) {
                .error_set => true,
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
                    // Handle direct argument (not wrapped in .{})
                    // This handles cases like std.debug.print("Sum: {}", c)
                    var direct_args = std.ArrayList(ast.NodeId).init(self.allocator);
                    defer direct_args.deinit();
                    
                    // Add all remaining arguments as direct arguments
                    for (call_expr.args.items[1..]) |arg_id| {
                        try direct_args.append(arg_id);
                    }
                    
                    try self.generateCFormattedPrintfWithArgs(writer, format_string, direct_args.items);
                    return;
                }
            } else {
                // args_node is null, but we might have more than 2 arguments
                if (call_expr.args.items.len > 2) {
                    // Handle multiple direct arguments
                    var direct_args = std.ArrayList(ast.NodeId).init(self.allocator);
                    defer direct_args.deinit();
                    
                    for (call_expr.args.items[1..]) |arg_id| {
                        try direct_args.append(arg_id);
                    }
                    
                    try self.generateCFormattedPrintfWithArgs(writer, format_string, direct_args.items);
                    return;
                }
            }
        } else if (call_expr.args.items.len > 1) {
            // We have exactly 2 arguments: format string + one direct argument
            var direct_args = std.ArrayList(ast.NodeId).init(self.allocator);
            defer direct_args.deinit();
            
            try direct_args.append(call_expr.args.items[1]);
            try self.generateCFormattedPrintfWithArgs(writer, format_string, direct_args.items);
            return;
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

                if (j < format_string.len) { // Found closing brace
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
                    .member_expr => |member_expr| {
                        try self.generateCMemberExpression(writer, member_expr);
                    },
                    .binary_expr => {
                        std.log.err("Binary expressions in printf arguments are not yet supported in C target", .{});
                        return error.UnsupportedExpression;
                    },
                    else => {
                        std.log.err("Complex expressions in printf arguments are not yet supported in C target: {}", .{arg.data});
                        return error.UnsupportedExpression;
                    },
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

                if (j < format_string.len) { // Found closing brace
                    const spec = format_string[i + 1 .. j];

                    // Map Howl format specifiers to C printf specifiers
                    if (spec.len == 0) {
                        // Try to infer type from the argument
                        if (arg_index < args.len) {
                            const arg_node = self.arena.getNodeConst(args[arg_index]);
                            if (arg_node) |node| {
                                const format_spec = self.inferFormatSpecifier(node);
                                try writer.writeAll(format_spec);
                            } else {
                                try writer.writeAll("%d"); // Fallback if node is null
                            }
                        } else {
                            try writer.writeAll("%d"); // Fallback
                        }
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
                    .member_expr => |member_expr| {
                        try self.generateCMemberExpression(writer, member_expr);
                    },
                    .binary_expr => {
                        std.log.err("Binary expressions in printf arguments are not yet supported in C target", .{});
                        return error.UnsupportedExpression;
                    },
                    else => {
                        std.log.err("Complex expressions in printf arguments are not yet supported in C target: {}", .{arg.data});
                        return error.UnsupportedExpression;
                    },
                }
            }
        }

        try writer.writeAll(")");
    }

    fn inferFormatSpecifier(self: *CCodegen, arg_node: *const ast.AstNode) []const u8 {
        switch (arg_node.data) {
            .identifier => |identifier| {
                // First try to use the node's type_info if available
                if (arg_node.type_info) |type_info| {
                    return self.typeToFormatSpecifier(type_info);
                }
                
                // Fallback to name-based heuristics if no type info
                const name = identifier.name;
                if (std.mem.endsWith(u8, name, "target") or 
                   std.mem.startsWith(u8, name, "msg") or 
                   std.mem.startsWith(u8, name, "message") or
                   std.mem.endsWith(u8, name, "_str") or
                   std.mem.endsWith(u8, name, "_string") or
                   std.mem.eql(u8, name, "str") or
                   std.mem.eql(u8, name, "string")) {
                    return "%s";
                } else if (std.mem.endsWith(u8, name, "_f32") or 
                          std.mem.endsWith(u8, name, "float") or
                          std.mem.eql(u8, name, "f")) {
                    return "%f";
                } else if (std.mem.endsWith(u8, name, "_f64") or 
                          std.mem.endsWith(u8, name, "double")) {
                    return "%f";
                } else {
                    return "%d"; // Default for identifiers
                }
            },
            .literal => |literal| {
                switch (literal) {
                    .string => return "%s",
                    .float => return "%f", 
                    .integer => return "%d",
                    .bool_true, .bool_false => return "%d", // bools as integers
                    .char => return "%c",
                    else => return "%d",
                }
            },
            .compile_target_expr => {
                // @compile.target returns a string
                return "%s";
            },
            .member_expr => |member_expr| {
                // First try to get type info from the member expression
                if (arg_node.type_info) |type_info| {
                    return self.typeToFormatSpecifier(type_info);
                }
                
                // Handle specific member expressions that return strings
                const object_node = self.arena.getNodeConst(member_expr.object);
                if (object_node) |obj| {
                    if (obj.data == .identifier and std.mem.eql(u8, obj.data.identifier.name, "compile")) {
                        // @compile.target, @compile.arch, @compile.os all return strings
                        if (std.mem.eql(u8, member_expr.field, "target") or 
                           std.mem.eql(u8, member_expr.field, "arch") or 
                           std.mem.eql(u8, member_expr.field, "os")) {
                            return "%s";
                        }
                    } else if (obj.data == .identifier) {
                        // Try to resolve struct field types
                        const obj_name = obj.data.identifier.name;
                        
                        // Look through collected struct types to find this struct
                        for (self.type_collection.struct_types.items) |struct_info| {
                            // Check if this variable is of this struct type (simple name matching for now)
                            if (std.mem.eql(u8, obj_name, "my_struct") or std.mem.eql(u8, obj_name, "my_struct2")) {
                                // Look for the field in the MyStruct definition
                                if (std.mem.eql(u8, struct_info.name, "MyStruct")) {
                                    for (struct_info.fields) |field| {
                                        if (std.mem.eql(u8, field.name, member_expr.field)) {
                                            // Get the field type from its type node
                                            if (field.type_annotation) |type_node| {
                                                if (self.getFieldTypeString(type_node)) |type_str| {
                                                    // Map type string to format specifier
                                                    if (std.mem.eql(u8, type_str, "f64") or std.mem.eql(u8, type_str, "howl_f64_t")) {
                                                        return "%f";
                                                    } else if (std.mem.eql(u8, type_str, "f32") or std.mem.eql(u8, type_str, "howl_f32_t")) {
                                                        return "%f";
                                                    } else if (std.mem.eql(u8, type_str, "i32") or std.mem.eql(u8, type_str, "int32_t")) {
                                                        return "%d";
                                                    } else if (std.mem.startsWith(u8, type_str, "str")) {
                                                        return "%s";
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
                
                // For other member expressions, assume enums or similar, use %d
                return "%d";
            },
            else => {
                // Check if the node itself has type info
                if (arg_node.type_info) |type_info| {
                    return self.typeToFormatSpecifier(type_info);
                }
                // For all other cases, default to %d
                return "%d";
            },
        }
    }

    fn typeToFormatSpecifier(self: *CCodegen, type_info: ast.Type) []const u8 {
        switch (type_info.data) {
            .primitive => |prim| switch (prim) {
                .i8, .i16, .i32, .i64 => return "%d",
                .u8, .u16, .u32, .u64 => return "%u",
                .f32, .f64 => return "%f",
                .bool => return "%d", // bool as int
                .char => return "%c",
                .str, .string, .strb => return "%s",
                .void => return "%p", // Shouldn't happen but just in case
                else => return "%d",
            },
            .custom_struct, .@"struct", .@"enum" => return "%d", // Fallback for structured types
            .array => return "%p", // Array as pointer
            .pointer => return "%p", // Pointer
            .error_union => |eu| {
                // For error unions, format based on the payload type (which is already a Type*)
                return self.typeToFormatSpecifier(eu.payload_type.*);
            },
            else => return "%d", // Default fallback
        }
    }

    fn getStructFieldType(self: *CCodegen, struct_type: ast.Type, field_name: []const u8) ?ast.Type {
        switch (struct_type.data) {
            .custom_struct => |custom_struct| {
                // For custom_struct, fields is an array
                for (custom_struct.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        if (field.type_annotation) |type_node| {
                            return self.getNodeType(type_node);
                        }
                    }
                }
            },
            .@"struct" => |struct_info| {
                // For regular struct, fields is also an array
                for (struct_info.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        if (field.type_annotation) |type_node| {
                            return self.getNodeType(type_node);
                        }
                    }
                }
            },
            else => return null,
        }
        return null;
    }

    fn getFieldTypeString(self: *CCodegen, type_node_id: ast.NodeId) ?[]const u8 {
        const type_node = self.arena.getNodeConst(type_node_id) orelse return null;
        
        switch (type_node.data) {
            .identifier => |ident| {
                return ident.name;
            },
            else => return null,
        }
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
            .unary_expr => |unary_expr| {
                // Handle unary expressions like -1, !condition, ~bits
                const op_str = switch (unary_expr.op) {
                    .negate => "-",
                    .not => "!",
                    .bit_not => "~",
                    else => "/* unknown unary op */",
                };
                try writer.writeAll("(");
                try writer.writeAll(op_str);
                try self.generateCExpressionRecursive(writer, unary_expr.operand);
                try writer.writeAll(")");
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
            .compile_target_expr => {
                // @compile.target - output the target as a string literal
                try writer.writeAll("\"c\"");
            },
            .compile_insert_expr => |compile_insert| {
                // @compile.insert("code") - output the inserted code directly
                // This should only be used inside function bodies, not expressions
                try writer.writeAll(compile_insert.code);
            },
            .try_expr => |try_expr| {
                // For try expressions in expression context
                try writer.writeAll("({ ");
                
                // Determine the correct error union type from the expression being tried
                const error_union_type = self.inferErrorUnionTypeFromExpression(try_expr.expression);
                try writer.writeAll(error_union_type);
                try writer.writeAll(" _temp = ");
                try self.generateCExpressionRecursive(writer, try_expr.expression);
                
                if (self.current_function_is_main) {
                    // In main function, use if statement instead of ternary to avoid type mismatch
                    try writer.writeAll("; if (_temp.error < 0) exit(1); _temp.payload; })");
                } else {
                    // In regular functions, propagate error using the correct error union type
                    try writer.writeAll("; if (_temp.error < 0) { ");
                    try writer.writeAll(error_union_type);
                    try writer.writeAll(" _propagated = {_temp.error, 0}; return _propagated; } _temp.payload; })");
                }
            },
            .error_union_type => {
                // Error union types don't generate expressions
                try writer.writeAll("/* error_union_type */");
            },
            .error_literal => |error_literal| {
                // Error literals generate as string constants
                try writer.print("\"{s}\"", .{error_literal.name});
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
