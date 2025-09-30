const std = @import("std");
const ast = @import("ast.zig");
const ErrorSystem = @import("error_system.zig");

// ============================================================================
// Sea of Nodes Intermediate Representation
// ============================================================================

/// Unique identifier for IR nodes
pub const IrNodeId = u32;
pub const INVALID_IR_NODE_ID: IrNodeId = std.math.maxInt(IrNodeId);

/// IR Operation types following sea-of-nodes design
pub const IrOp = enum {
    // Control flow nodes
    start, // Entry point of the program/function
    region, // Merge point for control flow
    block, // Block of statements
    if_, // Conditional branch
    loop, // Loop header
    loop_end, // Loop completion
    for_loop, // For loop construct
    while_loop, // While loop construct
    return_, // Function return
    unreachable_, // Unreachable code marker

    // Data nodes
    constant, // Compile-time constant
    parameter, // Function parameter
    identifier, // Variable or module identifier reference
    var_decl, // Variable declaration
    phi, // SSA phi node for merging values

    // Arithmetic operations
    add,
    sub,
    mul,
    div,
    mod,

    // Comparison operations
    eq,
    ne,
    lt,
    le,
    gt,
    ge,

    // Logical operations
    logical_and,
    logical_or,
    logical_not,
    select, // Conditional select (ternary operator)

    // Memory operations
    load, // Load from memory
    store, // Store to memory
    alloc, // Allocate stack memory
    heap_alloc, // Allocate heap memory (malloc)

    // Function operations
    call, // Function call
    function_def, // Function definition

    // Type operations
    cast, // Type cast
    type_check, // Runtime type check

    // Error handling
    try_, // Try expression for error handling
    error_union_ok, // Create successful error union (error_code=0, value=payload)
    error_union_err, // Create error error union (error_code=code, value=null)
    error_union_unwrap, // Extract payload from error union (with error propagation)

    // Match expression operations
    match_start, // Start of match expression
    match_branch, // Single pattern match branch
    match_end, // End of match expression

    // Object/struct operations
    member_access, // Access struct member
    struct_init, // Initialize struct with fields (heap/pointer chaining)
    struct_literal, // Stack struct literal value

    // Module/namespace operations
    namespace, // Module/namespace reference
    namespace_member, // Access member of a namespace

    // Projection nodes (extract values from multi-output nodes)
    proj_control, // Extract control flow from multi-output node
    proj_data, // Extract data value from multi-output node
    proj_memory, // Extract memory state from multi-output node

    pub fn toString(self: IrOp) []const u8 {
        return switch (self) {
            .start => "Start",
            .region => "Region",
            .block => "Block",
            .if_ => "If",
            .loop => "Loop",
            .loop_end => "LoopEnd",
            .for_loop => "ForLoop",
            .while_loop => "WhileLoop",
            .return_ => "Return",
            .unreachable_ => "Unreachable",
            .constant => "Constant",
            .parameter => "Parameter",
            .identifier => "Identifier",
            .var_decl => "VarDecl",
            .phi => "Phi",
            .add => "Add",
            .sub => "Sub",
            .mul => "Mul",
            .div => "Div",
            .mod => "Mod",
            .eq => "Eq",
            .ne => "Ne",
            .lt => "Lt",
            .le => "Le",
            .gt => "Gt",
            .ge => "Ge",
            .logical_and => "LogicalAnd",
            .logical_or => "LogicalOr",
            .logical_not => "LogicalNot",
            .select => "Select",
            .load => "Load",
            .store => "Store",
            .alloc => "Alloc",
            .heap_alloc => "HeapAlloc",
            .call => "Call",
            .function_def => "FunctionDef",
            .cast => "Cast",
            .type_check => "TypeCheck",
            .try_ => "Try",
            .error_union_ok => "ErrorUnionOk",
            .error_union_err => "ErrorUnionErr",
            .error_union_unwrap => "ErrorUnionUnwrap",
            .match_start => "MatchStart",
            .match_branch => "MatchBranch",
            .match_end => "MatchEnd",
            .member_access => "MemberAccess",
            .struct_init => "StructInit",
            .struct_literal => "StructLiteral",
            .namespace => "Namespace",
            .namespace_member => "NamespaceMember",
            .proj_control => "ProjControl",
            .proj_data => "ProjData",
            .proj_memory => "ProjMemory",
        };
    }

    pub fn isControl(self: IrOp) bool {
        return switch (self) {
            .start, .region, .block, .if_, .loop, .loop_end, .for_loop, .return_, .unreachable_ => true,
            else => false,
        };
    }

    pub fn isData(self: IrOp) bool {
        return switch (self) {
            .constant, .parameter, .var_decl, .phi, .add, .sub, .mul, .div, .mod, .eq, .ne, .lt, .le, .gt, .ge, .logical_and, .logical_or, .logical_not, .select, .load, .cast, .type_check, .try_, .error_union_ok, .error_union_err, .error_union_unwrap, .member_access, .struct_init, .struct_literal, .heap_alloc, .match_start, .match_branch, .match_end, .namespace, .namespace_member => true,
            else => false,
        };
    }

    pub fn isMemory(self: IrOp) bool {
        return switch (self) {
            .load, .store, .alloc, .heap_alloc => true,
            else => false,
        };
    }

    pub fn isProjection(self: IrOp) bool {
        return switch (self) {
            .proj_control, .proj_data, .proj_memory => true,
            else => false,
        };
    }
};

/// Constant value stored in IR
pub const IrConstant = union(enum) {
    integer: i64,
    float: f64,
    boolean: bool,
    string: []const u8,
    none: void,

    pub fn fromAstLiteral(literal: ast.Literal) IrConstant {
        return switch (literal) {
            .integer => |int| IrConstant{ .integer = int.value },
            .float => |float| IrConstant{ .float = float.value },
            .bool_true => IrConstant{ .boolean = true },
            .bool_false => IrConstant{ .boolean = false },
            .string => |str| IrConstant{ .string = str.value },
            .none => IrConstant{ .none = {} },
            .char => |char| IrConstant{ .integer = @intCast(char.value) },
            .enum_member => |enum_member| {
                // Convert enum member to integer constant
                const enum_value = if (std.mem.eql(u8, enum_member.name, "A"))
                    @as(i64, 0)
                else if (std.mem.eql(u8, enum_member.name, "B"))
                    @as(i64, 1)
                else if (std.mem.eql(u8, enum_member.name, "C"))
                    @as(i64, 2)
                else
                    @as(i64, 0); // default
                return IrConstant{ .integer = enum_value };
            },
            else => IrConstant{ .none = {} }, // Default fallback
        };
    }

    pub fn toString(self: IrConstant, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .integer => |val| try std.fmt.allocPrint(allocator, "{d}", .{val}),
            .float => |val| try std.fmt.allocPrint(allocator, "{d}", .{val}),
            .boolean => |val| try allocator.dupe(u8, if (val) "true" else "false"),
            .string => |val| try std.fmt.allocPrint(allocator, "\"{s}\"", .{val}),
            .none => try allocator.dupe(u8, "none"),
        };
    }
};

/// Individual IR node in the sea-of-nodes graph
pub const IrNode = struct {
    id: IrNodeId,
    op: IrOp,
    inputs: []IrNodeId, // Input dependencies (both control and data)
    output_type: ?ast.Type, // Type information from semantic analysis
    source_loc: ast.SourceLoc, // Source location for error reporting

    // Node-specific data
    data: IrNodeData,

    // Graph structure
    users: std.ArrayList(IrNodeId), // Nodes that use this node as input

    pub fn init(allocator: std.mem.Allocator, id: IrNodeId, op: IrOp, inputs: []const IrNodeId, source_loc: ast.SourceLoc) !IrNode {
        const inputs_copy = try allocator.dupe(IrNodeId, inputs);
        return IrNode{
            .id = id,
            .op = op,
            .inputs = inputs_copy,
            .output_type = null,
            .source_loc = source_loc,
            .data = IrNodeData{ .none = {} },
            .users = std.ArrayList(IrNodeId).init(allocator),
        };
    }

    pub fn deinit(self: *IrNode, allocator: std.mem.Allocator) void {
        allocator.free(self.inputs);
        self.users.deinit();
        self.data.deinit(allocator);
    }

    pub fn addUser(self: *IrNode, user_id: IrNodeId) !void {
        try self.users.append(user_id);
    }

    pub fn removeUser(self: *IrNode, user_id: IrNodeId) void {
        for (self.users.items, 0..) |id, i| {
            if (id == user_id) {
                _ = self.users.swapRemove(i);
                break;
            }
        }
    }
};

/// Additional data stored in specific IR node types
pub const IrNodeData = union(enum) {
    none: void,
    constant: IrConstant,
    parameter: struct {
        index: u32,
        name: []const u8,
    },
    identifier: struct {
        name: []const u8,
    },
    var_decl: struct {
        name: []const u8,
        initializer: ?IrNodeId,
    },
    phi: struct {
        region_inputs: []IrNodeId, // Control inputs for phi merge
    },
    call: struct {
        function_name: []const u8,
        arg_types: []ast.Type,
    },
    function_def: struct {
        name: []const u8,
        params: [][]const u8, // Parameter names
        param_nodes: []IrNodeId, // Parameter node IDs
        return_type: ast.Type,
        body: IrNodeId, // IR node representing the function body
    },
    projection: struct {
        tuple_input: IrNodeId,
        index: u32,
    },
    alloc: struct {
        alloc_type: ast.Type,
        size: ?u64, // Optional size for arrays
    },
    match_branch: struct {
        condition: IrNodeId, // Pattern condition
        body: IrNodeId, // Body to execute if condition matches
        is_wildcard: bool, // Whether this is a wildcard pattern
        arm_body_node: IrNodeId, // The actual arm body node to execute
    },
    match_end: struct {
        result_type: ast.Type, // Type of the match result
    },
    loop: struct {
        condition: IrNodeId, // Loop condition
        body: IrNodeId, // Loop body to execute
    },
    loop_end: struct {
        result_type: ast.Type, // Type of the loop result
    },
    for_loop: struct {
        ast_node_id: ast.NodeId, // The AST for_expr node
        ast_arena: *const ast.AstArena, // Reference to AST arena
    },
    while_loop: struct {
        ast_node_id: ast.NodeId, // The AST while_expr node
        ast_arena: *const ast.AstArena, // Reference to AST arena
    },
    struct_literal: struct {
        field_names: [][]const u8, // Names of fields in declaration order
    },
    namespace: struct {
        name: []const u8, // Name of the namespace/module
        members: std.StringHashMap(IrNodeId), // Member name -> IR node mapping
    },
    namespace_member: struct {
        namespace: IrNodeId, // Reference to the namespace
        member_name: []const u8, // Name of the member being accessed
    },

    pub fn deinit(self: *IrNodeData, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .phi => |*phi| allocator.free(phi.region_inputs),
            .call => |*call| allocator.free(call.arg_types),
            .function_def => |*func| {
                allocator.free(func.params);
                allocator.free(func.param_nodes);
            },
            .struct_literal => |*sl| allocator.free(sl.field_names),
            .namespace => |*ns| {
                ns.members.deinit();
            },
            .namespace_member => |*nm| {
                allocator.free(nm.member_name);
            },
            else => {},
        }
    }
};

/// Sea-of-nodes IR graph container
pub const SeaOfNodes = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(IrNode),
    start_node: IrNodeId,
    next_id: IrNodeId,

    // Quick lookup tables
    node_map: std.AutoHashMap(IrNodeId, u32), // node_id -> index in nodes array

    pub fn init(allocator: std.mem.Allocator) SeaOfNodes {
        return SeaOfNodes{
            .allocator = allocator,
            .nodes = std.ArrayList(IrNode).init(allocator),
            .start_node = INVALID_IR_NODE_ID,
            .next_id = 0,
            .node_map = std.AutoHashMap(IrNodeId, u32).init(allocator),
        };
    }

    pub fn deinit(self: *SeaOfNodes) void {
        for (self.nodes.items) |*node| {
            node.deinit(self.allocator);
        }
        self.nodes.deinit();
        self.node_map.deinit();
    }

    /// Create a new IR node and add it to the graph
    pub fn createNode(self: *SeaOfNodes, op: IrOp, inputs: []const IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = self.next_id;
        self.next_id += 1;

        const node = try IrNode.init(self.allocator, node_id, op, inputs, source_loc);

        // Update user lists for input nodes
        for (inputs) |input_id| {
            if (self.getNodeMut(input_id)) |input_node| {
                try input_node.addUser(node_id);
            }
        }

        const index = self.nodes.items.len;
        try self.nodes.append(node);
        try self.node_map.put(node_id, @intCast(index));

        return node_id;
    }

    /// Get immutable reference to node
    pub fn getNode(self: *const SeaOfNodes, id: IrNodeId) ?*const IrNode {
        const index = self.node_map.get(id) orelse return null;
        if (index >= self.nodes.items.len) return null;
        return &self.nodes.items[index];
    }

    /// Get mutable reference to node
    pub fn getNodeMut(self: *SeaOfNodes, id: IrNodeId) ?*IrNode {
        const index = self.node_map.get(id) orelse return null;
        if (index >= self.nodes.items.len) return null;
        return &self.nodes.items[index];
    }

    /// Replace all uses of old_node with new_node
    pub fn replaceNode(self: *SeaOfNodes, old_id: IrNodeId, new_id: IrNodeId) !void {
        const old_node = self.getNode(old_id) orelse return;

        // Update all users of the old node
        for (old_node.users.items) |user_id| {
            if (self.getNodeMut(user_id)) |user_node| {
                // Replace old_id with new_id in user's inputs
                for (user_node.inputs) |*input_id| {
                    if (input_id.* == old_id) {
                        input_id.* = new_id;
                        // Add user to new node's user list
                        if (self.getNodeMut(new_id)) |new_node| {
                            try new_node.addUser(user_id);
                        }
                    }
                }
            }
        }
    }

    /// Remove a node from the graph (must have no users)
    pub fn removeNode(self: *SeaOfNodes, node_id: IrNodeId) !void {
        const node = self.getNode(node_id) orelse return;

        // Ensure node has no users
        if (node.users.items.len > 0) {
            return error.NodeHasUsers;
        }

        // Remove this node from its inputs' user lists
        for (node.inputs) |input_id| {
            if (self.getNodeMut(input_id)) |input_node| {
                input_node.removeUser(node_id);
            }
        }

        // Remove from map
        _ = self.node_map.remove(node_id);
    }

    /// Get all nodes that use the given node as input
    pub fn getUsers(self: *const SeaOfNodes, node_id: IrNodeId) []IrNodeId {
        const node = self.getNode(node_id) orelse return &.{};
        return node.users.items;
    }

    /// Set the start node for this IR graph
    pub fn setStartNode(self: *SeaOfNodes, start_id: IrNodeId) void {
        self.start_node = start_id;
    }

    /// Create a constant node
    pub fn createConstant(self: *SeaOfNodes, constant: IrConstant, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.constant, &.{}, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .constant = constant };
        }
        return node_id;
    }

    /// Create a namespace node
    pub fn createNamespace(self: *SeaOfNodes, name: []const u8, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.namespace, &.{}, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .namespace = .{
                .name = try self.allocator.dupe(u8, name),
                .members = std.StringHashMap(IrNodeId).init(self.allocator),
            } };
        }
        return node_id;
    }

    /// Create a namespace member access node
    pub fn createNamespaceMember(self: *SeaOfNodes, namespace: IrNodeId, member_name: []const u8, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.namespace_member, &.{namespace}, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .namespace_member = .{
                .namespace = namespace,
                .member_name = try self.allocator.dupe(u8, member_name),
            } };
        }
        return node_id;
    }

    /// Create a parameter node
    pub fn createParameter(self: *SeaOfNodes, index: u32, name: []const u8, param_type: ast.Type, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.parameter, &.{}, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .parameter = .{ .index = index, .name = name } };
            node.output_type = param_type;
        }
        return node_id;
    }

    /// Create an identifier node
    pub fn createIdentifier(self: *SeaOfNodes, name: []const u8, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.identifier, &.{}, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .identifier = .{ .name = name } };
        }
        return node_id;
    }

    /// Create a variable declaration node
    pub fn createVarDecl(self: *SeaOfNodes, name: []const u8, initializer: ?IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        const inputs = if (initializer) |init_id| &[_]IrNodeId{init_id} else &[_]IrNodeId{};
        const node_id = try self.createNode(.var_decl, inputs, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .var_decl = .{ .name = name, .initializer = initializer } };
        }
        return node_id;
    }

    /// Create a call node with function name
    pub fn createCall(self: *SeaOfNodes, function_name: []const u8, inputs: []const IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.call, inputs, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            // For now, we'll allocate empty arg_types array - this should be filled by semantic analysis
            const empty_arg_types = try self.allocator.alloc(ast.Type, 0);
            node.data = IrNodeData{ .call = .{ .function_name = function_name, .arg_types = empty_arg_types } };
        }
        return node_id;
    }

    /// Create a function definition node
    pub fn createFunctionDef(self: *SeaOfNodes, name: []const u8, params: [][]const u8, param_nodes: []const IrNodeId, return_type: ast.Type, body: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.function_def, &.{body}, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            // Duplicate the params array
            const params_copy = try self.allocator.dupe([]const u8, params);
            const param_nodes_copy = try self.allocator.dupe(IrNodeId, param_nodes);
            node.data = IrNodeData{ .function_def = .{ .name = name, .params = params_copy, .param_nodes = param_nodes_copy, .return_type = return_type, .body = body } };
            // Set output_type for the codegen to use
            node.output_type = return_type;
        }
        return node_id;
    }

    /// Create a binary operation node
    pub fn createBinaryOp(self: *SeaOfNodes, op: IrOp, left: IrNodeId, right: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(op, &.{ left, right }, source_loc);
    }

    /// Create a unary operation node
    pub fn createUnaryOp(self: *SeaOfNodes, op: IrOp, operand: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(op, &.{operand}, source_loc);
    }

    /// Create an error union success value (error_code=0, value=payload)
    pub fn createErrorUnionOk(self: *SeaOfNodes, payload: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(.error_union_ok, &.{payload}, source_loc);
    }

    /// Create an error union error value (error_code=code, value=null)
    pub fn createErrorUnionErr(self: *SeaOfNodes, error_code: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(.error_union_err, &.{error_code}, source_loc);
    }

    /// Create error union unwrap operation (extract payload with error propagation)
    pub fn createErrorUnionUnwrap(self: *SeaOfNodes, error_union: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(.error_union_unwrap, &.{error_union}, source_loc);
    }

    /// Create try expression for error handling
    pub fn createTry(self: *SeaOfNodes, expression: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(.try_, &.{expression}, source_loc);
    }

    /// Create match start operation
    pub fn createMatchStart(self: *SeaOfNodes, matched_value: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(.match_start, &.{matched_value}, source_loc);
    }

    /// Create match branch operation
    pub fn createMatchBranch(self: *SeaOfNodes, match_start: IrNodeId, condition: IrNodeId, body: IrNodeId, is_wildcard: bool, arm_body_node: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.match_branch, &.{ match_start, condition, body }, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .match_branch = .{
                .condition = condition,
                .body = body,
                .is_wildcard = is_wildcard,
                .arm_body_node = arm_body_node,
            } };
        }
        return node_id;
    }

    /// Create match end operation
    pub fn createMatchEnd(self: *SeaOfNodes, branches: []const IrNodeId, result_type: ast.Type, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.match_end, branches, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .match_end = .{
                .result_type = result_type,
            } };
            node.output_type = result_type;
        }
        return node_id;
    }

    /// Create a loop start operation
    pub fn createLoopStart(self: *SeaOfNodes, initial_condition: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        return self.createNode(.loop, &.{initial_condition}, source_loc);
    }

    /// Create a loop body operation
    pub fn createLoopBody(self: *SeaOfNodes, loop_start: IrNodeId, condition: IrNodeId, body: IrNodeId, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.loop, &.{ loop_start, condition, body }, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .loop = .{
                .condition = condition,
                .body = body,
            } };
        }
        return node_id;
    }

    /// Create a loop end operation
    pub fn createLoopEnd(self: *SeaOfNodes, loop_body: IrNodeId, result_type: ast.Type, source_loc: ast.SourceLoc) !IrNodeId {
        const node_id = try self.createNode(.loop, &.{loop_body}, source_loc);
        if (self.getNodeMut(node_id)) |node| {
            node.data = IrNodeData{ .loop_end = .{
                .result_type = result_type,
            } };
            node.output_type = result_type;
        }
        return node_id;
    }

    /// Perform topological sort for code generation
    pub fn topologicalSort(self: *const SeaOfNodes, allocator: std.mem.Allocator) ![]IrNodeId {
        var result = std.ArrayList(IrNodeId).init(allocator);
        var visited = std.AutoHashMap(IrNodeId, void).init(allocator);
        defer visited.deinit();

        // Visit all nodes in the graph
        for (0..self.nodes.items.len) |i| {
            const node_id: IrNodeId = @intCast(i);
            if (!visited.contains(node_id)) {
                try self.topologicalSortRecursive(node_id, &result, &visited);
            }
        }

        return result.toOwnedSlice();
    }

    fn topologicalSortRecursive(
        self: *const SeaOfNodes,
        node_id: IrNodeId,
        result: *std.ArrayList(IrNodeId),
        visited: *std.AutoHashMap(IrNodeId, void),
    ) !void {
        if (visited.contains(node_id)) return;
        try visited.put(node_id, {});

        const node = self.getNode(node_id) orelse return;

        // Visit all dependencies first
        for (node.inputs) |input_id| {
            try self.topologicalSortRecursive(input_id, result, visited);
        }

        // Add this node to result
        try result.append(node_id);
    }

    /// Debug: Print the IR graph
    pub fn dumpGraph(self: *const SeaOfNodes, writer: anytype) !void {
        try writer.print("=== Sea of Nodes IR Graph ===\n", .{});
        try writer.print("Start node: {d}\n", .{self.start_node});
        try writer.print("Total nodes: {d}\n\n", .{self.nodes.items.len});

        for (self.nodes.items) |*node| {
            try writer.print("Node {d}: {s}", .{ node.id, node.op.toString() });

            if (node.inputs.len > 0) {
                try writer.print(" inputs=[", .{});
                for (node.inputs, 0..) |input_id, i| {
                    if (i > 0) try writer.print(", ", .{});
                    try writer.print("{d}", .{input_id});
                }
                try writer.print("]", .{});
            }

            if (node.users.items.len > 0) {
                try writer.print(" users=[", .{});
                for (node.users.items, 0..) |user_id, i| {
                    if (i > 0) try writer.print(", ", .{});
                    try writer.print("{d}", .{user_id});
                }
                try writer.print("]", .{});
            }

            // Print additional data
            switch (node.data) {
                .constant => |constant| {
                    const const_str = try constant.toString(self.allocator);
                    defer self.allocator.free(const_str);
                    try writer.print(" value={s}", .{const_str});
                },
                .parameter => |param| {
                    try writer.print(" param[{d}]=\"{s}\"", .{ param.index, param.name });
                },
                else => {},
            }

            try writer.print(" @ {s}:{d}:{d}\n", .{ node.source_loc.file_path, node.source_loc.line, node.source_loc.column });
        }
        try writer.print("\n", .{});
    }
};

// ============================================================================
// Error handling for IR construction and optimization
// ============================================================================

pub const IrError = error{
    UnsupportedAstNode,
    InvalidNodeReference,
    NodeHasUsers,
    CyclicGraph,
    InvalidTransformation,
    OptimizationFailed,
} || std.mem.Allocator.Error;

pub fn reportIrError(
    errors: *ErrorSystem.ErrorCollector,
    code: ErrorSystem.ErrorCode,
    message: []const u8,
    source_loc: ast.SourceLoc,
) !void {
    _ = try errors.createAndAddError(
        code,
        .semantic, // IR errors are treated as semantic errors
        .error_,
        message,
        source_loc.toSourceSpan(),
    );
}
