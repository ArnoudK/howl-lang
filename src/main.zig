//! Howl Language Compiler - Main Entry Point

const std = @import("std");
const root = @import("root.zig");

// Use the library exports directly
const Compiler = root.Compiler;
const CompileOptions = root.CompileOptions;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <file.howl> [options]\n", .{args[0]});
        std.debug.print("   or: {s} [options] <file.howl>\n", .{args[0]});
        std.debug.print("\nTarget Selection:\n", .{});
        std.debug.print("  -tjs                 Generate JavaScript code (default)\n", .{});
        std.debug.print("  -t64                 Generate 64-bit native binary\n", .{});
        std.debug.print("  -t32                 Generate 32-bit native binary\n", .{});
        std.debug.print("  -tobj                Generate object file\n", .{});
        std.debug.print("  -tllvm               Generate LLVM IR\n", .{});
        std.debug.print("\nOther Options:\n", .{});
        std.debug.print("  --format=<format>    Output format: colored, plain, json, summary (default: colored)\n", .{});
        std.debug.print("  --target=<target>    Legacy target syntax (use -t flags instead)\n", .{});
        std.debug.print("  --max-errors=<n>     Maximum number of errors to show (default: 10)\n", .{});
        std.debug.print("  --no-warnings        Disable warnings\n", .{});
        std.debug.print("  --stop-on-error      Stop compilation on first error\n", .{});
        std.debug.print("  --dump-ast           Dump AST if compilation succeeds\n", .{});
        std.debug.print("  --dump-zir           Dump ZIR if ZIR generation succeeds\n", .{});
        std.debug.print("  --stats              Show compilation statistics\n", .{});
        std.debug.print("\nExamples:\n", .{});
        std.debug.print("  {s} example/simple-program.howl -tjs\n", .{args[0]});
        std.debug.print("  {s} -t64 example/simple-program.howl\n", .{args[0]});
        std.debug.print("  {s} example/simple-program.howl -t64 --dump-zir\n", .{args[0]});
        std.debug.print("  {s} -tobj --stats example/simple-program.howl\n", .{args[0]});
        return;
    }

    // Find the file path - it's the first argument that doesn't start with '-'
    var file_path: []const u8 = undefined;
    var file_path_found = false;
    for (args[1..]) |arg| {
        if (!std.mem.startsWith(u8, arg, "-")) {
            file_path = arg;
            file_path_found = true;
            break;
        }
    }
    
    if (!file_path_found) {
        std.debug.print("Error: No input file specified\n", .{});
        std.debug.print("Usage: {s} [options] <file.howl>\n", .{args[0]});
        return;
    }

    // Parse options
    var output_format = CompileOptions.OutputFormat.colored_text;
    var target: root.CompileTarget = .javascript;
    var max_errors: u32 = 10;
    var enable_warnings = true;
    var stop_on_first_error = false;
    var show_stats = false;

    for (args[1..]) |arg| {
        // Skip the file path
        if (std.mem.eql(u8, arg, file_path)) {
            continue;
        }
        
        // New short flag format for targets
        if (std.mem.eql(u8, arg, "-tjs")) {
            target = .javascript;
        } else if (std.mem.eql(u8, arg, "-t64")) {
            target = .zig_binary;
        } else if (std.mem.eql(u8, arg, "-t32")) {
            target = .zig_binary; // For now, treat 32-bit same as 64-bit
        } else if (std.mem.eql(u8, arg, "-tobj")) {
            target = .zig_object;
        } else if (std.mem.eql(u8, arg, "-tllvm")) {
            target = .llvm_ir;
        // Legacy format support
        } else if (std.mem.startsWith(u8, arg, "--format=")) {
            const format_str = arg[9..];
            if (std.mem.eql(u8, format_str, "colored")) {
                output_format = CompileOptions.OutputFormat.colored_text;
            } else if (std.mem.eql(u8, format_str, "plain")) {
                output_format = CompileOptions.OutputFormat.plain_text;
            } else if (std.mem.eql(u8, format_str, "json")) {
                output_format = CompileOptions.OutputFormat.json;
            } else if (std.mem.eql(u8, format_str, "summary")) {
                output_format = CompileOptions.OutputFormat.summary_only;
            } else {
                std.debug.print("Unknown format: {s}\n", .{format_str});
                return;
            }
        } else if (std.mem.startsWith(u8, arg, "--target=")) {
            const target_str = arg[9..];
            std.debug.print("Warning: --target= syntax is deprecated. Use -t flags instead (e.g., -tjs, -t64)\n", .{});
            if (std.mem.eql(u8, target_str, "js")) {
                target = .javascript;
            } else if (std.mem.eql(u8, target_str, "zig-bin")) {
                target = .zig_binary;
            } else if (std.mem.eql(u8, target_str, "zig-obj")) {
                target = .zig_object;
            } else if (std.mem.eql(u8, target_str, "llvm")) {
                target = .llvm_ir;
            } else {
                std.debug.print("Unknown target: {s}\n", .{target_str});
                return;
            }
        } else if (std.mem.startsWith(u8, arg, "--max-errors=")) {
            max_errors = std.fmt.parseInt(u32, arg[13..], 10) catch {
                std.debug.print("Invalid max-errors value: {s}\n", .{arg[13..]});
                return;
            };
        } else if (std.mem.eql(u8, arg, "--no-warnings")) {
            enable_warnings = false;
        } else if (std.mem.eql(u8, arg, "--stop-on-error")) {
            stop_on_first_error = true;
        } else if (std.mem.eql(u8, arg, "--stats")) {
            show_stats = true;
        } else {
            std.debug.print("Unknown option: {s}\n", .{arg});
            std.debug.print("Use -tjs, -t64, -tobj, -tllvm for target selection\n", .{});
            return;
        }
    }

    // Read source file
    const source_content = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ file_path, err });
        return;
    };
    defer allocator.free(source_content);

    // Create compile options
    const options = CompileOptions{
        .file_path = file_path,
        .source_content = source_content,
        .stop_on_first_error = stop_on_first_error,
        .max_errors = max_errors,
        .enable_warnings = enable_warnings,
        .output_format = output_format,
        .target = target,
    };

    // Initialize compiler
    var compiler = Compiler.init(allocator, options) catch |err| {
        std.debug.print("Failed to initialize compiler: {}\n", .{err});
        return;
    };
    defer compiler.deinit();

    // Compile
    var result = compiler.compile() catch |err| {
        std.debug.print("Compilation failed with error: {}\n", .{err});
        return;
    };
    defer result.deinit(allocator);

    // Print errors if any (but not for successful x64 compilations)
    if (result.errors.errors.items.len > 0 and !(result.success and target == .zig_binary)) {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();
        try compiler.formatErrors(&result, buffer.writer());
        std.debug.print("{s}", .{buffer.items});
    }

    // Print generated code if successful
    if (result.success and result.generated_code != null) {
        switch (result.target) {
            .javascript => {
                std.debug.print("\nGenerated JavaScript:\n", .{});
                std.debug.print("==========================================\n", .{});
                std.debug.print("{s}\n", .{result.generated_code.?});
                std.debug.print("==========================================\n", .{});
            },
            .zig_binary, .zig_object, .llvm_ir => {
                std.debug.print("\nZIR backend result:\n", .{});
                std.debug.print("==========================================\n", .{});
                std.debug.print("{s}\n", .{result.generated_code.?});
                std.debug.print("==========================================\n", .{});
            },
        }
    }

    // Print compilation statistics if requested
    if (show_stats) {
        const stats = compiler.getCompilationStats(&result);
        std.debug.print("\nCompilation Statistics:\n", .{});
        std.debug.print("  Total AST nodes: {d}\n", .{stats.total_nodes});
        std.debug.print("  Phase completed: {s}\n", .{@tagName(stats.phase_completed)});
        std.debug.print("  Success: {}\n", .{stats.success});
    }

    // Exit with appropriate code
    if (!result.success) {
        std.process.exit(1);
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "use other module" {
    try std.testing.expectEqual(@as(i32, 150), root.add(100, 50));
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
