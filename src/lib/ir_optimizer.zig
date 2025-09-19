const std = @import("std");
const SeaOfNodes = @import("sea_of_nodes_ir.zig").SeaOfNodes;
const IrNodeId = @import("sea_of_nodes_ir.zig").IrNodeId;
const IrOp = @import("sea_of_nodes_ir.zig").IrOp;
const IrConstant = @import("sea_of_nodes_ir.zig").IrConstant;
const IrError = @import("sea_of_nodes_ir.zig").IrError;
const reportIrError = @import("sea_of_nodes_ir.zig").reportIrError;
const INVALID_IR_NODE_ID = @import("sea_of_nodes_ir.zig").INVALID_IR_NODE_ID;
const ErrorSystem = @import("error_system.zig");
const ast = @import("ast.zig");

// ============================================================================
// IR Optimization Pass Framework
// ============================================================================

/// Signature for optimization pass functions
pub const PassFunction = *const fn (*SeaOfNodes, *ErrorSystem.ErrorCollector) anyerror!bool;

/// Individual optimization pass
pub const OptimizationPass = struct {
    name: []const u8,
    description: []const u8,
    run: PassFunction,

    pub fn init(name: []const u8, description: []const u8, run_fn: PassFunction) OptimizationPass {
        return OptimizationPass{
            .name = name,
            .description = description,
            .run = run_fn,
        };
    }
};

/// Pass manager for running optimization passes
pub const PassManager = struct {
    allocator: std.mem.Allocator,
    passes: std.ArrayList(OptimizationPass),
    debug_output: bool,
    max_iterations: u32,

    pub fn init(allocator: std.mem.Allocator) PassManager {
        return PassManager{
            .allocator = allocator,
            .passes = std.ArrayList(OptimizationPass).init(allocator),
            .debug_output = false,
            .max_iterations = 10, // Prevent infinite optimization loops
        };
    }

    pub fn deinit(self: *PassManager) void {
        self.passes.deinit();
    }

    /// Add an optimization pass to the manager
    pub fn addPass(self: *PassManager, pass: OptimizationPass) !void {
        try self.passes.append(pass);
    }

    /// Run all registered passes on the IR
    pub fn runAllPasses(self: *PassManager, ir: *SeaOfNodes, errors: *ErrorSystem.ErrorCollector) !void {
        var iteration: u32 = 0;
        var made_changes = true;

        while (made_changes and iteration < self.max_iterations) {
            made_changes = false;
            iteration += 1;

            if (self.debug_output) {
                std.debug.print("=== Optimization iteration {d} ===\n", .{iteration});
            }

            for (self.passes.items) |*pass| {
                if (self.debug_output) {
                    std.debug.print("Running pass: {s}\n", .{pass.name});
                }

                const pass_made_changes = try pass.run(ir, errors);
                made_changes = made_changes or pass_made_changes;

                // Stop if any errors occurred
                if (errors.hasErrors()) {
                    return;
                }
            }
        }

        if (iteration >= self.max_iterations and made_changes) {
            try reportIrError(
                errors,
                .optimization_failed,
                "Optimization passes did not converge within maximum iterations",
                ast.SourceLoc.invalid(),
            );
        }
    }

    /// Create a standard set of optimization passes
    pub fn createStandardPasses(allocator: std.mem.Allocator) !PassManager {
        var manager = PassManager.init(allocator);

        // Add passes in order of execution
        try manager.addPass(OptimizationPass.init("constant-folding", "Fold compile-time constants", constantFoldingPass));
        try manager.addPass(OptimizationPass.init("dead-code-elimination", "Remove unreachable code", deadCodeEliminationPass));
        try manager.addPass(OptimizationPass.init("common-subexpr-elimination", "Eliminate common subexpressions", commonSubexpressionEliminationPass));
        try manager.addPass(OptimizationPass.init("copy-propagation", "Propagate simple copies", copyPropagationPass));

        return manager;
    }
};

// ============================================================================
// Individual Optimization Passes
// ============================================================================

/// Constant folding pass - evaluate compile-time constants
fn constantFoldingPass(ir: *SeaOfNodes, errors: *ErrorSystem.ErrorCollector) !bool {
    _ = errors; // TODO: Use for error reporting
    var made_changes = false;

    // We need to create a list of nodes to avoid modifying the list while iterating
    var nodes_to_process = try std.ArrayList(IrNodeId).initCapacity(ir.allocator, ir.nodes.items.len);
    defer nodes_to_process.deinit();

    for (ir.nodes.items) |*node| {
        try nodes_to_process.append(node.id);
    }

    for (nodes_to_process.items) |node_id| {
        const node = ir.getNode(node_id) orelse continue;

        // Try to fold binary operations with constant operands
        if (node.inputs.len == 2) {
            const left = ir.getNode(node.inputs[0]);
            const right = ir.getNode(node.inputs[1]);

            if (left != null and right != null and
                left.?.op == .constant and right.?.op == .constant)
            {
                const folded_result = foldBinaryOperation(node.op, left.?.data.constant, right.?.data.constant);
                if (folded_result) |result| {
                    // Create a new constant node
                    const new_constant = try ir.createConstant(result, node.source_loc);

                    // Replace all uses of the old node with the new constant
                    try ir.replaceNode(node_id, new_constant);
                    made_changes = true;
                }
            }
        }

        // Try to fold unary operations with constant operands
        if (node.inputs.len == 1) {
            const operand = ir.getNode(node.inputs[0]);

            if (operand != null and operand.?.op == .constant) {
                const folded_result = foldUnaryOperation(node.op, operand.?.data.constant);
                if (folded_result) |result| {
                    const new_constant = try ir.createConstant(result, node.source_loc);
                    try ir.replaceNode(node_id, new_constant);
                    made_changes = true;
                }
            }
        }
    }

    return made_changes;
}

/// Dead code elimination pass - remove unreachable nodes
fn deadCodeEliminationPass(ir: *SeaOfNodes, errors: *ErrorSystem.ErrorCollector) !bool {
    _ = errors; // TODO: Use for error reporting
    var made_changes = false;

    // Mark all reachable nodes starting from the start node
    var reachable = std.AutoHashMap(IrNodeId, void).init(ir.allocator);
    defer reachable.deinit();

    if (ir.start_node != INVALID_IR_NODE_ID) {
        try markReachable(ir, ir.start_node, &reachable);
    }

    // Collect unreachable nodes
    var unreachable_nodes = std.ArrayList(IrNodeId).init(ir.allocator);
    defer unreachable_nodes.deinit();

    for (ir.nodes.items) |*node| {
        if (!reachable.contains(node.id)) {
            try unreachable_nodes.append(node.id);
        }
    }

    // Remove unreachable nodes
    for (unreachable_nodes.items) |node_id| {
        // Only remove if the node has no users (to be safe)
        const node = ir.getNode(node_id);
        if (node != null and node.?.users.items.len == 0) {
            try ir.removeNode(node_id);
            made_changes = true;
        }
    }

    return made_changes;
}

/// Common subexpression elimination pass
fn commonSubexpressionEliminationPass(ir: *SeaOfNodes, errors: *ErrorSystem.ErrorCollector) !bool {
    _ = errors; // TODO: Use for error reporting
    var made_changes = false;

    // Map from (op, inputs) to node_id for finding duplicates
    var expression_map = std.HashMap(ExpressionKey, IrNodeId, ExpressionContext, std.hash_map.default_max_load_percentage).init(ir.allocator);
    defer {
        // Clean up allocated keys
        var iterator = expression_map.iterator();
        while (iterator.next()) |entry| {
            ir.allocator.free(entry.key_ptr.inputs);
        }
        expression_map.deinit();
    }

    for (ir.nodes.items) |*node| {
        // Only consider pure data operations
        if (node.op.isData() and node.inputs.len > 0) {
            const key = ExpressionKey{
                .op = node.op,
                .inputs = try ir.allocator.dupe(IrNodeId, node.inputs),
            };

            if (expression_map.get(key)) |existing_node_id| {
                // Found a duplicate expression, replace this node with the existing one
                try ir.replaceNode(node.id, existing_node_id);
                made_changes = true;
                // Clean up the key we just allocated
                ir.allocator.free(key.inputs);
            } else {
                try expression_map.put(key, node.id);
            }
        }
    }

    return made_changes;
}

/// Copy propagation pass - replace loads of stored constants
fn copyPropagationPass(ir: *SeaOfNodes, errors: *ErrorSystem.ErrorCollector) !bool {
    _ = errors; // TODO: Use for error reporting
    var made_changes = false;

    // Track store operations: alloc_node -> stored_value
    var stored_values = std.AutoHashMap(IrNodeId, IrNodeId).init(ir.allocator);
    defer stored_values.deinit();

    // First pass: find all store operations
    for (ir.nodes.items) |*node| {
        if (node.op == .store and node.inputs.len >= 2) {
            const alloc_node = node.inputs[0];
            const stored_value = node.inputs[1];
            try stored_values.put(alloc_node, stored_value);
        }
    }

    // Second pass: replace loads with stored values
    for (ir.nodes.items) |*node| {
        if (node.op == .load and node.inputs.len >= 1) {
            const alloc_node = node.inputs[0];
            if (stored_values.get(alloc_node)) |stored_value| {
                // Check if the stored value is a constant
                const value_node = ir.getNode(stored_value);
                if (value_node != null and value_node.?.op == .constant) {
                    try ir.replaceNode(node.id, stored_value);
                    made_changes = true;
                }
            }
        }
    }

    return made_changes;
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Recursively mark nodes as reachable
fn markReachable(
    ir: *SeaOfNodes,
    node_id: IrNodeId,
    reachable: *std.AutoHashMap(IrNodeId, void),
) !void {
    if (reachable.contains(node_id)) return; // Already visited

    try reachable.put(node_id, {});

    const node = ir.getNode(node_id) orelse return;

    // Mark all input nodes as reachable
    for (node.inputs) |input_id| {
        try markReachable(ir, input_id, reachable);
    }

    // Mark all users as reachable
    for (node.users.items) |user_id| {
        try markReachable(ir, user_id, reachable);
    }
}

/// Fold binary operations with constant operands
fn foldBinaryOperation(op: IrOp, left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (op) {
        .add => foldAdd(left, right),
        .sub => foldSub(left, right),
        .mul => foldMul(left, right),
        .div => foldDiv(left, right),
        .mod => foldMod(left, right),
        .eq => foldEq(left, right),
        .ne => foldNe(left, right),
        .lt => foldLt(left, right),
        .le => foldLe(left, right),
        .gt => foldGt(left, right),
        .ge => foldGe(left, right),
        .logical_and => foldLogicalAnd(left, right),
        .logical_or => foldLogicalOr(left, right),
        else => null,
    };
}

/// Fold unary operations with constant operands
fn foldUnaryOperation(op: IrOp, operand: IrConstant) ?IrConstant {
    return switch (op) {
        .logical_not => switch (operand) {
            .boolean => |val| IrConstant{ .boolean = !val },
            else => null,
        },
        else => null,
    };
}

// Arithmetic folding functions
fn foldAdd(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .integer = l + r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .float = l + r },
            else => null,
        },
        else => null,
    };
}

fn foldSub(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .integer = l - r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .float = l - r },
            else => null,
        },
        else => null,
    };
}

fn foldMul(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .integer = l * r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .float = l * r },
            else => null,
        },
        else => null,
    };
}

fn foldDiv(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| if (r != 0) IrConstant{ .integer = @divTrunc(l, r) } else null,
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| if (r != 0.0) IrConstant{ .float = l / r } else null,
            else => null,
        },
        else => null,
    };
}

fn foldMod(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| if (r != 0) IrConstant{ .integer = @mod(l, r) } else null,
            else => null,
        },
        else => null,
    };
}

// Comparison folding functions
fn foldEq(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .boolean = l == r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .boolean = l == r },
            else => null,
        },
        .boolean => |l| switch (right) {
            .boolean => |r| IrConstant{ .boolean = l == r },
            else => null,
        },
        else => null,
    };
}

fn foldNe(left: IrConstant, right: IrConstant) ?IrConstant {
    const eq_result = foldEq(left, right);
    if (eq_result) |result| {
        return switch (result) {
            .boolean => |val| IrConstant{ .boolean = !val },
            else => null,
        };
    }
    return null;
}

fn foldLt(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .boolean = l < r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .boolean = l < r },
            else => null,
        },
        else => null,
    };
}

fn foldLe(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .boolean = l <= r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .boolean = l <= r },
            else => null,
        },
        else => null,
    };
}

fn foldGt(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .boolean = l > r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .boolean = l > r },
            else => null,
        },
        else => null,
    };
}

fn foldGe(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .integer => |l| switch (right) {
            .integer => |r| IrConstant{ .boolean = l >= r },
            else => null,
        },
        .float => |l| switch (right) {
            .float => |r| IrConstant{ .boolean = l >= r },
            else => null,
        },
        else => null,
    };
}

// Logical folding functions
fn foldLogicalAnd(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .boolean => |l| switch (right) {
            .boolean => |r| IrConstant{ .boolean = l and r },
            else => null,
        },
        else => null,
    };
}

fn foldLogicalOr(left: IrConstant, right: IrConstant) ?IrConstant {
    return switch (left) {
        .boolean => |l| switch (right) {
            .boolean => |r| IrConstant{ .boolean = l or r },
            else => null,
        },
        else => null,
    };
}

// ============================================================================
// Expression Key for Common Subexpression Elimination
// ============================================================================

const ExpressionKey = struct {
    op: IrOp,
    inputs: []IrNodeId,

    pub fn eql(self: ExpressionKey, other: ExpressionKey) bool {
        if (self.op != other.op) return false;
        if (self.inputs.len != other.inputs.len) return false;

        for (self.inputs, other.inputs) |a, b| {
            if (a != b) return false;
        }
        return true;
    }

    pub fn hash(self: ExpressionKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&self.op));
        for (self.inputs) |input| {
            hasher.update(std.mem.asBytes(&input));
        }
        return hasher.final();
    }
};

const ExpressionContext = struct {
    pub fn hash(self: @This(), key: ExpressionKey) u64 {
        _ = self;
        return key.hash();
    }

    pub fn eql(self: @This(), a: ExpressionKey, b: ExpressionKey) bool {
        _ = self;
        return a.eql(b);
    }
};
