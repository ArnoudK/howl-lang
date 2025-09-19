const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");
const utils = @import("codegen_c_utils.zig");

const CCodegenError = utils.CCodegenError;
const Writer = utils.Writer;

// Data structures for collecting types and functions during analysis
pub const CollectedType = struct {
    name: []const u8,
    c_definition: []const u8,
    type_kind: TypeKind,

    const TypeKind = enum {
        builtin_list,
        builtin_stringbuilder,
        user_struct,
    };
};

pub const CollectedStruct = struct {
    name: []const u8,
    fields: []const ast.Field,
    methods: std.ArrayList([]const u8), // Method names that belong to this struct
    is_comptime: bool,
};

pub const CollectedEnum = struct {
    name: []const u8,
    members: []const ast.EnumMember,
};

pub const CollectedErrorSet = struct {
    name: []const u8,
    errors: []const []const u8,
};

pub const CollectedErrorUnion = struct {
    error_set_name: []const u8,
    payload_type: []const u8,
    struct_name: []const u8, // Generated name like MyError_i32_ErrorUnion
};

pub const CollectedOptional = struct {
    inner_type: []const u8,
    struct_name: []const u8, // Generated name like Optional_int32_t
};

pub const TypeCollection = struct {
    list_types: std.ArrayList([]const u8),
    builtin_types: std.ArrayList([]const u8),
    custom_types: std.ArrayList(CollectedType),
    struct_types: std.ArrayList(CollectedStruct),
    enum_types: std.ArrayList(CollectedEnum),
    error_set_types: std.ArrayList(CollectedErrorSet),
    error_union_types: std.ArrayList(CollectedErrorUnion),
    optional_types: std.ArrayList(CollectedOptional),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeCollection {
        return TypeCollection{
            .list_types = std.ArrayList([]const u8).init(allocator),
            .builtin_types = std.ArrayList([]const u8).init(allocator),
            .custom_types = std.ArrayList(CollectedType).init(allocator),
            .struct_types = std.ArrayList(CollectedStruct).init(allocator),
            .enum_types = std.ArrayList(CollectedEnum).init(allocator),
            .error_set_types = std.ArrayList(CollectedErrorSet).init(allocator),
            .error_union_types = std.ArrayList(CollectedErrorUnion).init(allocator),
            .optional_types = std.ArrayList(CollectedOptional).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TypeCollection) void {
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

        // Free all allocated strings in optional_types
        for (self.optional_types.items) |optional_type| {
            self.allocator.free(optional_type.inner_type);
            self.allocator.free(optional_type.struct_name);
        }

        self.list_types.deinit();
        self.builtin_types.deinit();
        self.custom_types.deinit();
        self.struct_types.deinit();
        self.enum_types.deinit();
        self.error_set_types.deinit();
        self.error_union_types.deinit();
        self.optional_types.deinit();
    }

    pub fn hasListType(self: *const TypeCollection, type_name: []const u8) bool {
        for (self.list_types.items) |existing| {
            if (std.mem.eql(u8, existing, type_name)) return true;
        }
        return false;
    }

    pub fn hasBuiltinType(self: *const TypeCollection, type_name: []const u8) bool {
        for (self.builtin_types.items) |existing| {
            if (std.mem.eql(u8, existing, type_name)) return true;
        }
        return false;
    }

    pub fn hasStructType(self: *const TypeCollection, type_name: []const u8) bool {
        for (self.struct_types.items) |existing| {
            if (std.mem.eql(u8, existing.name, type_name)) return true;
        }
        return false;
    }

    pub fn addListType(self: *TypeCollection, allocator: std.mem.Allocator, type_name: []const u8) CCodegenError!void {
        if (!self.hasListType(type_name)) {
            const owned_name = try allocator.dupe(u8, type_name);
            try self.list_types.append(owned_name);
        }
    }

    pub fn addBuiltinType(self: *TypeCollection, allocator: std.mem.Allocator, type_name: []const u8) CCodegenError!void {
        if (!self.hasBuiltinType(type_name)) {
            const owned_name = try allocator.dupe(u8, type_name);
            try self.builtin_types.append(owned_name);
        }
    }

    pub fn addStructType(self: *TypeCollection, allocator: std.mem.Allocator, struct_decl: anytype) CCodegenError!void {
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

    pub fn hasEnumType(self: *const TypeCollection, enum_name: []const u8) bool {
        for (self.enum_types.items) |enum_type| {
            if (std.mem.eql(u8, enum_type.name, enum_name)) {
                return true;
            }
        }
        return false;
    }

    pub fn addEnumType(self: *TypeCollection, allocator: std.mem.Allocator, enum_decl: anytype) CCodegenError!void {
        if (!self.hasEnumType(enum_decl.name)) {
            const collected_enum = CollectedEnum{
                .name = try allocator.dupe(u8, enum_decl.name),
                .members = enum_decl.members.items,
            };
            try self.enum_types.append(collected_enum);
        }
    }

    pub fn hasErrorSetType(self: *const TypeCollection, error_set_name: []const u8) bool {
        for (self.error_set_types.items) |error_set_type| {
            if (std.mem.eql(u8, error_set_type.name, error_set_name)) {
                return true;
            }
        }
        return false;
    }

    pub fn addErrorSetType(self: *TypeCollection, allocator: std.mem.Allocator, error_set_decl: anytype) CCodegenError!void {
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

    pub fn hasErrorUnionType(self: *const TypeCollection, error_set_name: []const u8, payload_type: []const u8) bool {
        for (self.error_union_types.items) |error_union| {
            if (std.mem.eql(u8, error_union.error_set_name, error_set_name) and
                std.mem.eql(u8, error_union.payload_type, payload_type))
            {
                return true;
            }
        }
        return false;
    }

    pub fn addErrorUnionType(self: *TypeCollection, allocator: std.mem.Allocator, error_set_name: []const u8, payload_type: []const u8) CCodegenError!void {
        if (!self.hasErrorUnionType(error_set_name, payload_type)) {
            const struct_name = try utils.generateErrorUnionTypeName(allocator, error_set_name, payload_type);
            const collected_error_union = CollectedErrorUnion{
                .error_set_name = try allocator.dupe(u8, error_set_name),
                .payload_type = try allocator.dupe(u8, payload_type),
                .struct_name = struct_name,
            };
            try self.error_union_types.append(collected_error_union);
        }
    }

    pub fn hasOptionalType(self: *const TypeCollection, inner_type: []const u8) bool {
        for (self.optional_types.items) |optional_type| {
            if (std.mem.eql(u8, optional_type.inner_type, inner_type)) {
                return true;
            }
        }
        return false;
    }

    pub fn addOptionalType(self: *TypeCollection, allocator: std.mem.Allocator, inner_type: []const u8) CCodegenError![]const u8 {
        if (!self.hasOptionalType(inner_type)) {
            // Generate struct name like Optional_int32_t
            const struct_name = try std.fmt.allocPrint(allocator, "Optional_{s}_t", .{utils.sanitizeTypeForOptionalName(inner_type)});

            const collected_optional = CollectedOptional{
                .inner_type = try allocator.dupe(u8, inner_type),
                .struct_name = struct_name,
            };
            try self.optional_types.append(collected_optional);
            return struct_name;
        } else {
            // Find existing optional type
            for (self.optional_types.items) |optional_type| {
                if (std.mem.eql(u8, optional_type.inner_type, inner_type)) {
                    return optional_type.struct_name;
                }
            }
        }
        return ""; // Should never reach here
    }

    /// Extract Optional function names from the expected type like "Optional_int32_t"
    pub fn getOptionalFunctionNames(self: *const TypeCollection, optional_type: []const u8) struct { some: []const u8, none: []const u8 } {
        _ = self;

        // Extract the type part from "Optional_TypeName_t" format
        if (std.mem.startsWith(u8, optional_type, "Optional_") and std.mem.endsWith(u8, optional_type, "_t")) {
            // Create some and none function names
            const some_fn = std.fmt.allocPrint(std.heap.page_allocator, "{s}_some", .{optional_type}) catch optional_type;
            const none_fn = std.fmt.allocPrint(std.heap.page_allocator, "{s}_none", .{optional_type}) catch optional_type;
            return .{ .some = some_fn, .none = none_fn };
        }

        // Fallback to int32 if parsing fails
        return .{ .some = "Optional_int32_t_some", .none = "Optional_int32_t_none" };
    }
};

pub fn typeToFormatSpecifier(type_info: ast.Type) []const u8 {
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
            return typeToFormatSpecifier(eu.payload_type.*);
        },
        else => return "%d", // Default fallback
    }
}

/// Generate C type string from Howl type information
pub fn generateCType(_: anytype, howl_type: ?ast.Type) []const u8 {
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
            .optional => |_| {
                // Use a generic pattern for all optional types
                // No hardcoded type name assumptions - let the type system handle this
                return "Optional_Generic_t";
            },
            else => "int32_t", // Default fallback
        };
    }
    return "int32_t"; // Default fallback
}
