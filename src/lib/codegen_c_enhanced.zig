const std = @import("std");
const ast = @import("ast.zig");
const SemanticAnalyzer = @import("semantic_analyzer.zig");
const ErrorSystem = @import("error_system.zig");
const ErrorCollector = ErrorSystem.ErrorCollector;

