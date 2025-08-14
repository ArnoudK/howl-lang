const std = @import("std");
const ErrorSystem = @import("./error_system.zig");
const AST = @import("./ast.zig");

const CompilerError = ErrorSystem.CompilerError;
const ErrorCollector = ErrorSystem.ErrorCollector;
const ErrorCode = ErrorSystem.ErrorCode;
const ErrorCategory = ErrorSystem.ErrorCategory;
const ErrorSeverity = ErrorSystem.ErrorSeverity;
const SourceSpan = ErrorSystem.SourceSpan;

/// Helper functions for creating semantic errors with intelligent suggestions
pub const SemanticErrorHelpers = struct {
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) SemanticErrorHelpers {
        return SemanticErrorHelpers{
            .allocator = allocator,
        };
    }
    
    /// Report undefined variable error with suggestion
    pub fn undefinedVariable(
        self: *const SemanticErrorHelpers,
        collector: *ErrorCollector,
        variable_name: []const u8,
        span: SourceSpan,
        available_variables: []const []const u8,
    ) !*CompilerError {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "undefined variable '{s}'",
            .{variable_name}
        );
        defer self.allocator.free(message);
        
        const err = try collector.createAndAddError(
            .undefined_variable,
            .semantic,
            .error_,
            message,
            span,
        );
        
        // Find similar variable names for suggestions
        var suggestions = std.ArrayList([]const u8).init(self.allocator);
        defer suggestions.deinit();
        
        for (available_variables) |var_name| {
            if (self.calculateLevenshteinDistance(variable_name, var_name) <= 2) {
                try suggestions.append(try std.fmt.allocPrint(
                    self.allocator,
                    "did you mean '{s}'?",
                    .{var_name}
                ));
            }
        }
        
        if (suggestions.items.len > 0) {
            try err.withSuggestions(self.allocator, suggestions.items);
        } else {
            const generic_suggestions = [_][]const u8{
                "check the variable name spelling",
                "ensure the variable is declared before use",
            };
            try err.withSuggestions(self.allocator, &generic_suggestions);
        }
        
        return err;
    }
    
    /// Report type mismatch error with detailed information
    pub fn typeMismatch(
        self: *const SemanticErrorHelpers,
        collector: *ErrorCollector,
        expected_type: []const u8,
        actual_type: []const u8,
        span: SourceSpan,
        context: []const u8,
    ) !*CompilerError {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "type mismatch in {s}: expected '{s}', found '{s}'",
            .{ context, expected_type, actual_type }
        );
        defer self.allocator.free(message);
        
        const err = try collector.createAndAddError(
            .type_mismatch,
            .semantic,
            .error_,
            message,
            span,
        );
        
        // Generate type-specific suggestions
        var suggestions = std.ArrayList([]const u8).init(self.allocator);
        defer suggestions.deinit();
        
        // Common type conversion suggestions
        if (std.mem.eql(u8, expected_type, "i32") and std.mem.eql(u8, actual_type, "str")) {
            try suggestions.append("use std.parse.parseInt() to convert string to integer");
        } else if (std.mem.eql(u8, expected_type, "str") and std.mem.eql(u8, actual_type, "i32")) {
            try suggestions.append("use std.fmt.print() to convert integer to string");
        } else if (std.mem.eql(u8, expected_type, "bool") and (std.mem.eql(u8, actual_type, "i32") or std.mem.eql(u8, actual_type, "i64"))) {
            try suggestions.append("use '!= 0' to convert integer to boolean");
        } else {
            try suggestions.append(try std.fmt.allocPrint(
                self.allocator,
                "cast the value to '{s}' if the conversion is intentional",
                .{expected_type}
            ));
        }
        
        try err.withSuggestions(self.allocator, suggestions.items);
        return err;
    }
    
    /// Report duplicate declaration error
    pub fn duplicateDeclaration(
        self: *const SemanticErrorHelpers,
        collector: *ErrorCollector,
        identifier_name: []const u8,
        current_span: SourceSpan,
        previous_span: SourceSpan,
    ) !*CompilerError {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "duplicate declaration of '{s}'",
            .{identifier_name}
        );
        defer self.allocator.free(message);
        
        const err = try collector.createAndAddError(
            .duplicate_declaration,
            .semantic,
            .error_,
            message,
            current_span,
        );
        
        // Create a related error for the previous declaration
        const related_message = try std.fmt.allocPrint(
            self.allocator,
            "'{s}' was previously declared here",
            .{identifier_name}
        );
        defer self.allocator.free(related_message);
        
        const related_err = try CompilerError.create(
            self.allocator,
            .duplicate_declaration,
            .semantic,
            .note,
            related_message,
            previous_span,
        );
        
        // Link the errors
        const related_errors = [_]*CompilerError{related_err};
        err.related_errors = try self.allocator.dupe(*CompilerError, &related_errors);
        
        const suggestions = [_][]const u8{
            "use a different name for this declaration",
            "remove one of the duplicate declarations",
        };
        try err.withSuggestions(self.allocator, &suggestions);
        
        return err;
    }
    
    /// Report invalid member access error
    pub fn invalidMemberAccess(
        self: *const SemanticErrorHelpers,
        collector: *ErrorCollector,
        base_type: []const u8,
        member_name: []const u8,
        span: SourceSpan,
        available_members: []const []const u8,
    ) !*CompilerError {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "type '{s}' has no member '{s}'",
            .{ base_type, member_name }
        );
        defer self.allocator.free(message);
        
        const err = try collector.createAndAddError(
            .invalid_member_access,
            .semantic,
            .error_,
            message,
            span,
        );
        
        var suggestions = std.ArrayList([]const u8).init(self.allocator);
        defer suggestions.deinit();
        
        // Find similar member names
        for (available_members) |member| {
            if (self.calculateLevenshteinDistance(member_name, member) <= 2) {
                try suggestions.append(try std.fmt.allocPrint(
                    self.allocator,
                    "did you mean '{s}'?",
                    .{member}
                ));
            }
        }
        
        // If no similar members, list available ones
        if (suggestions.items.len == 0 and available_members.len > 0) {
            var members_list = std.ArrayList(u8).init(self.allocator);
            defer members_list.deinit();
            
            try members_list.appendSlice("available members: ");
            for (available_members, 0..) |member, i| {
                if (i > 0) try members_list.appendSlice(", ");
                try members_list.appendSlice(member);
            }
            
            try suggestions.append(try members_list.toOwnedSlice());
        }
        
        if (suggestions.items.len > 0) {
            try err.withSuggestions(self.allocator, suggestions.items);
        }
        
        return err;
    }
    
    /// Report function call errors
    pub fn invalidFunctionCall(
        self: *const SemanticErrorHelpers,
        collector: *ErrorCollector,
        function_name: []const u8,
        span: SourceSpan,
        error_kind: FunctionCallErrorKind,
    ) !*CompilerError {
        const message = switch (error_kind) {
            .not_a_function => try std.fmt.allocPrint(
                self.allocator,
                "'{s}' is not a function",
                .{function_name}
            ),
            .wrong_argument_count => |counts| try std.fmt.allocPrint(
                self.allocator,
                "function '{s}' expects {d} argument{s}, but {d} {s} provided",
                .{ 
                    function_name, 
                    counts.expected, 
                    if (counts.expected == 1) "" else "s",
                    counts.actual,
                    if (counts.actual == 1) "was" else "were"
                }
            ),
            .argument_type_mismatch => |mismatch| try std.fmt.allocPrint(
                self.allocator,
                "argument {d} of function '{s}': expected '{s}', found '{s}'",
                .{ mismatch.argument_index + 1, function_name, mismatch.expected_type, mismatch.actual_type }
            ),
        };
        defer self.allocator.free(message);
        
        const code = switch (error_kind) {
            .not_a_function => ErrorCode.invalid_function_call,
            .wrong_argument_count => ErrorCode.wrong_argument_count,
            .argument_type_mismatch => ErrorCode.type_mismatch,
        };
        
        const err = try collector.createAndAddError(
            code,
            .semantic,
            .error_,
            message,
            span,
        );
        
        const suggestions = switch (error_kind) {
            .not_a_function => [_][]const u8{
                "check if this is the correct identifier",
                "ensure the function is declared before use",
            },
            .wrong_argument_count => [_][]const u8{
                "check the function signature",
                "add or remove arguments to match the expected count",
            },
            .argument_type_mismatch => [_][]const u8{
                "check the argument type",
                "cast the argument to the expected type if appropriate",
            },
        };
        
        try err.withSuggestions(self.allocator, &suggestions);
        return err;
    }
    
    /// Report binary operation type errors
    pub fn binaryOperationTypeError(
        self: *const SemanticErrorHelpers,
        collector: *ErrorCollector,
        operator: []const u8,
        left_type: []const u8,
        right_type: []const u8,
        span: SourceSpan,
    ) !*CompilerError {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "binary operation '{s}' is not supported between '{s}' and '{s}'",
            .{ operator, left_type, right_type }
        );
        defer self.allocator.free(message);
        
        const err = try collector.createAndAddError(
            .binary_operation_type_error,
            .semantic,
            .error_,
            message,
            span,
        );
        
        var suggestions = std.ArrayList([]const u8).init(self.allocator);
        defer suggestions.deinit();
        
        // Operator-specific suggestions
        if (std.mem.eql(u8, operator, "+")) {
            if (std.mem.eql(u8, left_type, "str") or std.mem.eql(u8, right_type, "str")) {
                try suggestions.append("use string concatenation function for string operations");
            } else {
                try suggestions.append("ensure both operands are numeric types");
            }
        } else if (std.mem.eql(u8, operator, "and") or std.mem.eql(u8, operator, "or")) {
            try suggestions.append("ensure both operands are boolean types");
        } else {
            try suggestions.append("check that both operands have compatible types");
            try suggestions.append("cast one operand to match the other's type if appropriate");
        }
        
        try err.withSuggestions(self.allocator, suggestions.items);
        return err;
    }
    
    /// Report return type mismatch
    pub fn returnTypeMismatch(
        self: *const SemanticErrorHelpers,
        collector: *ErrorCollector,
        function_name: []const u8,
        expected_type: []const u8,
        actual_type: []const u8,
        span: SourceSpan,
    ) !*CompilerError {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "function '{s}' expects return type '{s}', but '{s}' was returned",
            .{ function_name, expected_type, actual_type }
        );
        defer self.allocator.free(message);
        
        const err = try collector.createAndAddError(
            .return_type_mismatch,
            .semantic,
            .error_,
            message,
            span,
        );
        
        const suggestions = [_][]const u8{
            "check the function's return type declaration",
            "cast the return value to the expected type",
            "change the function's return type if this is intended",
        };
        try err.withSuggestions(self.allocator, &suggestions);
        
        return err;
    }
    
    /// Calculate Levenshtein distance for suggestion matching
    fn calculateLevenshteinDistance(self: *const SemanticErrorHelpers, str1: []const u8, str2: []const u8) usize {
        _ = self;
        const len1 = str1.len;
        const len2 = str2.len;
        
        if (len1 == 0) return len2;
        if (len2 == 0) return len1;
        
        // Simple implementation for small strings
        if (len1 > 20 or len2 > 20) return 99; // Too different
        
        var matrix: [21][21]usize = undefined;
        
        // Initialize first row and column
        for (0..len1 + 1) |i| {
            matrix[i][0] = i;
        }
        for (0..len2 + 1) |j| {
            matrix[0][j] = j;
        }
        
        // Fill the matrix
        for (1..len1 + 1) |i| {
            for (1..len2 + 1) |j| {
                const cost = if (str1[i - 1] == str2[j - 1]) @as(usize, 0) else @as(usize, 1);
                matrix[i][j] = @min(
                    @min(matrix[i - 1][j] + 1, matrix[i][j - 1] + 1),
                    matrix[i - 1][j - 1] + cost
                );
            }
        }
        
        return matrix[len1][len2];
    }
};

pub const FunctionCallErrorKind = union(enum) {
    not_a_function,
    wrong_argument_count: struct {
        expected: usize,
        actual: usize,
    },
    argument_type_mismatch: struct {
        argument_index: usize,
        expected_type: []const u8,
        actual_type: []const u8,
    },
};

/// Context for tracking semantic analysis state
pub const SemanticContext = struct {
    allocator: std.mem.Allocator,
    current_function: ?[]const u8,
    current_scope_depth: usize,
    symbol_tables: std.ArrayList(std.StringHashMap(SymbolInfo)),
    
    pub fn init(allocator: std.mem.Allocator) SemanticContext {
        return SemanticContext{
            .allocator = allocator,
            .current_function = null,
            .current_scope_depth = 0,
            .symbol_tables = std.ArrayList(std.StringHashMap(SymbolInfo)).init(allocator),
        };
    }
    
    pub fn deinit(self: *SemanticContext) void {
        for (self.symbol_tables.items) |*table| {
            table.deinit();
        }
        self.symbol_tables.deinit();
    }
    
    pub fn pushScope(self: *SemanticContext) !void {
        try self.symbol_tables.append(std.StringHashMap(SymbolInfo).init(self.allocator));
        self.current_scope_depth += 1;
    }
    
    pub fn popScope(self: *SemanticContext) void {
        if (self.symbol_tables.items.len > 0) {
            var table = self.symbol_tables.pop();
            table.deinit();
            self.current_scope_depth -= 1;
        }
    }
    
    pub fn declareSymbol(self: *SemanticContext, name: []const u8, symbol_info: SymbolInfo) !void {
        if (self.symbol_tables.items.len == 0) return error.NoScopeAvailable;
        
        const current_scope = &self.symbol_tables.items[self.symbol_tables.items.len - 1];
        try current_scope.put(try self.allocator.dupe(u8, name), symbol_info);
    }
    
    pub fn lookupSymbol(self: *const SemanticContext, name: []const u8) ?SymbolInfo {
        // Search from innermost to outermost scope
        var i = self.symbol_tables.items.len;
        while (i > 0) {
            i -= 1;
            if (self.symbol_tables.items[i].get(name)) |symbol| {
                return symbol;
            }
        }
        return null;
    }
    
    pub fn getAvailableSymbols(self: *const SemanticContext, allocator: std.mem.Allocator) ![][]const u8 {
        var symbols = std.ArrayList([]const u8).init(allocator);
        
        for (self.symbol_tables.items) |table| {
            var iterator = table.iterator();
            while (iterator.next()) |entry| {
                try symbols.append(try allocator.dupe(u8, entry.key_ptr.*));
            }
        }
        
        return symbols.toOwnedSlice();
    }
};

pub const SymbolKind = enum {
    variable,
    function,
    type_,
    constant,
};

pub const SymbolInfo = struct {
    kind: SymbolKind,
    type_name: []const u8,
    span: SourceSpan,
    is_mutable: bool,
    
    pub fn variable(type_name: []const u8, span: SourceSpan, is_mutable: bool) SymbolInfo {
        return SymbolInfo{
            .kind = .variable,
            .type_name = type_name,
            .span = span,
            .is_mutable = is_mutable,
        };
    }
    
    pub fn function_(return_type: []const u8, span: SourceSpan) SymbolInfo {
        return SymbolInfo{
            .kind = .function,
            .type_name = return_type,
            .span = span,
            .is_mutable = false,
        };
    }
};