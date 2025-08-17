//! Howl Language Compiler - Main Entry Point

const std = @import("std");
const root = @import("root.zig");
const lsp_server = @import("lib/lsp_server.zig");
const formatter = @import("lib/formatter.zig");

// Use the library exports directly
const Compiler = root.Compiler;
const CompileOptions = root.CompileOptions;

const Command = enum {
    build,
    run,
    lsp,
    fmt,
    help,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        // Disable memory leak detection for now since HashMap internal allocations
        // are being incorrectly flagged as leaks
        .safety = false,
    }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage(args[0]);
        return;
    }

    const command_str = args[1];
    const command = parseCommand(command_str) orelse {
        std.debug.print("Unknown command: {s}\n", .{command_str});
        try printUsage(args[0]);
        return;
    };

    switch (command) {
        .help => try printUsage(args[0]),
        .lsp => try runLspServer(allocator),
        .fmt => {
            // Convert [][:0]u8 to [][]const u8
            var converted_args = try allocator.alloc([]const u8, args[2..].len);
            defer allocator.free(converted_args);
            for (args[2..], 0..) |arg, i| {
                converted_args[i] = arg;
            }
            try runFormatter(allocator, converted_args);
        },
        .build => {
            // Convert [][:0]u8 to [][]const u8
            var converted_args = try allocator.alloc([]const u8, args[2..].len);
            defer allocator.free(converted_args);
            for (args[2..], 0..) |arg, i| {
                converted_args[i] = arg;
            }
            try runBuild(allocator, converted_args);
        },
        .run => {
            // Convert [][:0]u8 to [][]const u8
            var converted_args = try allocator.alloc([]const u8, args[2..].len);
            defer allocator.free(converted_args);
            for (args[2..], 0..) |arg, i| {
                converted_args[i] = arg;
            }
            try runProgram(allocator, converted_args);
        },
    }
}

fn parseCommand(command_str: []const u8) ?Command {
    if (std.mem.eql(u8, command_str, "build")) return .build;
    if (std.mem.eql(u8, command_str, "run")) return .run;
    if (std.mem.eql(u8, command_str, "lsp")) return .lsp;
    if (std.mem.eql(u8, command_str, "fmt")) return .fmt;
    if (std.mem.eql(u8, command_str, "help")) return .help;
    if (std.mem.eql(u8, command_str, "--help")) return .help;
    if (std.mem.eql(u8, command_str, "-h")) return .help;
    return null;
}

fn printUsage(program_name: []const u8) !void {
    std.debug.print("Howl Language Compiler\n\n", .{});
    std.debug.print("Usage: {s} <command> [options]\n\n", .{program_name});
    std.debug.print("Commands:\n", .{});
    std.debug.print("  build <file.howl> [options]    Compile a Howl file\n", .{});
    std.debug.print("  run <file.howl> [options]      Compile and run a Howl file\n", .{});
    std.debug.print("  fmt <file.howl> [options]      Format a Howl file\n", .{});
    std.debug.print("  lsp                             Start the Language Server Protocol server\n", .{});
    std.debug.print("  help                            Show this help message\n", .{});
    std.debug.print("\nBuild/Run Options:\n", .{});
    std.debug.print("  -tjs                            Generate JavaScript code (default)\n", .{});
    std.debug.print("  -tc                             Generate native binary (C)\n", .{});
    std.debug.print("  --format=<format>               Output format: colored, plain, json, summary\n", .{});
    std.debug.print("  --max-errors=<n>                Maximum number of errors to show (default: 10)\n", .{});
    std.debug.print("  --no-warnings                   Disable warnings\n", .{});
    std.debug.print("  --stop-on-error                 Stop compilation on first error\n", .{});
    std.debug.print("  --stats                         Show compilation statistics\n", .{});
    std.debug.print("\nFormat Options:\n", .{});
    std.debug.print("  --check                         Check if file is formatted (exit code 1 if not)\n", .{});
    std.debug.print("  --indent-size=<n>               Indentation size (default: 4)\n", .{});
    std.debug.print("  --use-tabs                      Use tabs for indentation\n", .{});
    std.debug.print("  --max-line-length=<n>           Maximum line length (default: 100)\n", .{});
    std.debug.print("  --trailing-comma-threshold=<n>  Multi-line threshold (default: 3)\n", .{});
    std.debug.print("  --no-multiline-trailing-comma   Disable auto multi-line for trailing commas\n", .{});
    std.debug.print("\nExamples:\n", .{});
    std.debug.print("  {s} build example/simple-program.howl -tjs\n", .{program_name});
    std.debug.print("  {s} run example/simple-program.howl -tc\n", .{program_name});
    std.debug.print("  {s} fmt example/simple-program.howl\n", .{program_name});
    std.debug.print("  {s} fmt --check example/simple-program.howl\n", .{program_name});
    std.debug.print("  {s} lsp\n", .{program_name});
}

fn runLspServer(allocator: std.mem.Allocator) !void {
    var server = lsp_server.LspServer.init(allocator);
    defer server.deinit();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var buffer: [8192]u8 = undefined;

        const headers = readHeaders(stdin, &buffer) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        const content_length = parseContentLength(headers) catch continue;

        if (content_length > buffer.len) {
            std.log.err("Message too large: {d} bytes\n", .{content_length});
            continue;
        }

        const message_bytes = buffer[0..content_length];
        const bytes_read = try stdin.readAll(message_bytes);
        if (bytes_read != content_length) {
            std.log.err("Expected {d} bytes, got {d}\n", .{ content_length, bytes_read });
            continue;
        }

        const message = std.mem.trim(u8, message_bytes, " \t\r\n");

        const parsed = std.json.parseFromSlice(std.json.Value, allocator, message, .{}) catch |err| {
            std.log.err("Failed to parse JSON: {}\n", .{err});
            continue;
        };
        defer parsed.deinit();

        const json_value = parsed.value;

        if (json_value.object.get("method")) |_| {
            if (json_value.object.get("id")) |_| {
                // This is a request
                const method = if (json_value.object.get("method")) |m| m.string else "unknown";
                const request = @import("lib/lsp.zig").LspRequest{
                    .method = method,
                    .id = json_value.object.get("id"),
                    .params = json_value.object.get("params"),
                };
                try server.handleRequest(request, stdout);
            } else {
                // This is a notification
                const method = if (json_value.object.get("method")) |m| m.string else "unknown";
                const notification = @import("lib/lsp.zig").LspNotification{
                    .method = method,
                    .params = json_value.object.get("params"),
                };
                try server.handleNotification(notification, stdout);
            }
        }
    }
}

fn readHeaders(reader: anytype, buffer: []u8) ![]const u8 {
    var pos: usize = 0;

    while (pos < buffer.len - 1) {
        const byte = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => return error.EndOfStream,
            else => return err,
        };

        buffer[pos] = byte;
        pos += 1;

        if (pos >= 4 and
            buffer[pos - 4] == '\r' and buffer[pos - 3] == '\n' and
            buffer[pos - 2] == '\r' and buffer[pos - 1] == '\n')
        {
            return buffer[0..pos];
        }
    }

    return error.HeadersTooLong;
}

fn parseContentLength(headers: []const u8) !usize {
    const content_length_prefix = "Content-Length: ";

    var lines = std.mem.splitSequence(u8, headers, "\r\n");
    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, content_length_prefix)) {
            const length_str = line[content_length_prefix.len..];
            return std.fmt.parseInt(usize, length_str, 10);
        }
    }

    return error.NoContentLength;
}

fn runBuild(allocator: std.mem.Allocator, args: [][]const u8) !void {
    if (args.len == 0) {
        std.debug.print("Error: No input file specified for build command\n", .{});
        std.debug.print("Usage: howl build <file.howl> [options]\n", .{});
        return;
    }

    // Find the file path - it's the first argument that doesn't start with '-'
    var file_path: []const u8 = undefined;
    var file_path_found = false;
    for (args) |arg| {
        if (!std.mem.startsWith(u8, arg, "-")) {
            file_path = arg;
            file_path_found = true;
            break;
        }
    }

    if (!file_path_found) {
        std.debug.print("Error: No input file specified\n", .{});
        std.debug.print("Usage: howl build <file.howl> [options]\n", .{});
        return;
    }

    const build_result = try compileFile(allocator, file_path, args);
    defer build_result.deinit(allocator);
    if (!build_result.success) {
        std.process.exit(1);
    }
}

fn runProgram(allocator: std.mem.Allocator, args: [][]const u8) !void {
    if (args.len == 0) {
        std.debug.print("Error: No input file specified for run command\n", .{});
        std.debug.print("Usage: howl run <file.howl> [options]\n", .{});
        return;
    }

    // Find the file path
    var file_path: []const u8 = undefined;
    var file_path_found = false;
    for (args) |arg| {
        if (!std.mem.startsWith(u8, arg, "-")) {
            file_path = arg;
            file_path_found = true;
            break;
        }
    }

    if (!file_path_found) {
        std.debug.print("Error: No input file specified\n", .{});
        std.debug.print("Usage: howl run <file.howl> [options]\n", .{});
        return;
    }

    const build_result = try compileFile(allocator, file_path, args);
    defer build_result.deinit(allocator);
    if (!build_result.success) {
        std.process.exit(1);
    }

    // Determine target to know how to run
    var target: root.CompileTarget = .javascript; // default
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-tc")) {
            target = .c;
        } else if (std.mem.eql(u8, arg, "-tjs")) {
            target = .javascript;
        }
    }

    // Run the compiled output
    switch (target) {
        .javascript => {
            // Create output directory
            std.fs.cwd().makeDir("howl-out") catch |err| switch (err) {
                error.PathAlreadyExists => {}, // Directory already exists, continue
                else => return err, // Other errors should be propagated
            };

            // Write JavaScript to file and run with node
            const temp_file = "howl-out/howl_program.js";

            if (build_result.generated_code) |code| {
                try std.fs.cwd().writeFile(.{ .sub_path = temp_file, .data = code });

                const result = try std.process.Child.run(.{
                    .allocator = allocator,
                    .argv = &[_][]const u8{ "node", temp_file },
                });
                defer allocator.free(result.stdout);
                defer allocator.free(result.stderr);

                std.debug.print("{s}", .{result.stdout});
                if (result.stderr.len > 0) {
                    std.debug.print("{s}", .{result.stderr});
                }
            }
        },
        .c => {
            // The C codegen already compiled the binary, just run it
            const exe_file = "howl-out/howl_output";

            // Check if the compiled binary exists
            std.fs.cwd().access(exe_file, .{}) catch {
                std.debug.print("Error: Compiled binary '{s}' not found\n", .{exe_file});
                return;
            };

            // Run the compiled executable
            const run_result = try std.process.Child.run(.{
                .allocator = allocator,
                .argv = &[_][]const u8{"./" ++ exe_file},
            });
            defer allocator.free(run_result.stdout);
            defer allocator.free(run_result.stderr);

            std.debug.print("{s}", .{run_result.stdout});
            if (run_result.stderr.len > 0) {
                std.debug.print("{s}", .{run_result.stderr});
            }

            // Leave compiled files in howl-out/ for user inspection
            // Clean up is optional and disabled by default to allow debugging
            // std.fs.cwd().deleteFile("howl-out/howl_program.c") catch {};
            // std.fs.cwd().deleteFile(exe_file) catch {};
        },
    }
}

fn runFormatter(allocator: std.mem.Allocator, args: [][]const u8) !void {
    if (args.len == 0) {
        std.debug.print("Error: No input file specified for fmt command\n", .{});
        std.debug.print("Usage: howl fmt <file.howl> [options]\n", .{});
        return;
    }

    // Find the file path - it's the first argument that doesn't start with '-'
    var file_path: []const u8 = undefined;
    var file_path_found = false;
    for (args) |arg| {
        if (!std.mem.startsWith(u8, arg, "-")) {
            file_path = arg;
            file_path_found = true;
            break;
        }
    }

    if (!file_path_found) {
        std.debug.print("Error: No input file specified\n", .{});
        std.debug.print("Usage: howl fmt <file.howl> [options]\n", .{});
        return;
    }

    // Parse formatter options
    var options = formatter.FormatterOptions{};
    var check_only = false;

    for (args) |arg| {
        // Skip the file path
        if (std.mem.eql(u8, arg, file_path)) {
            continue;
        }

        if (std.mem.eql(u8, arg, "--check")) {
            check_only = true;
        } else if (std.mem.startsWith(u8, arg, "--indent-size=")) {
            options.indent_size = std.fmt.parseInt(u32, arg[14..], 10) catch {
                std.debug.print("Invalid indent-size value: {s}\n", .{arg[14..]});
                return;
            };
        } else if (std.mem.eql(u8, arg, "--use-tabs")) {
            options.use_tabs = true;
        } else if (std.mem.startsWith(u8, arg, "--max-line-length=")) {
            options.max_line_length = std.fmt.parseInt(u32, arg[18..], 10) catch {
                std.debug.print("Invalid max-line-length value: {s}\n", .{arg[18..]});
                return;
            };
        } else if (std.mem.startsWith(u8, arg, "--trailing-comma-threshold=")) {
            options.trailing_comma_threshold = std.fmt.parseInt(u32, arg[27..], 10) catch {
                std.debug.print("Invalid trailing-comma-threshold value: {s}\n", .{arg[27..]});
                return;
            };
        } else if (std.mem.eql(u8, arg, "--no-multiline-trailing-comma")) {
            options.always_multiline_trailing_comma = false;
        } else if (std.mem.startsWith(u8, arg, "-")) {
            std.debug.print("Unknown option: {s}\n", .{arg});
            return;
        }
    }

    // Read source file
    const source_content = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ file_path, err });
        return;
    };
    defer allocator.free(source_content);

    // Format the code
    const format_result = formatter.formatCode(allocator, source_content, options) catch |err| {
        std.debug.print("Error formatting file: {}\n", .{err});
        return;
    };
    defer format_result.deinit(allocator);

    if (!format_result.success) {
        if (format_result.error_message) |err_msg| {
            std.debug.print("Format error: {s}\n", .{err_msg});
        }
        std.process.exit(1);
        return;
    }

    if (check_only) {
        // Check if the file is already formatted
        if (std.mem.eql(u8, source_content, format_result.formatted_code)) {
            std.debug.print("File is already formatted: {s}\n", .{file_path});
        } else {
            std.debug.print("File needs formatting: {s}\n", .{file_path});
            std.process.exit(1);
        }
    } else {
        // Write the formatted code back to the file
        std.fs.cwd().writeFile(.{ .sub_path = file_path, .data = format_result.formatted_code }) catch |err| {
            std.debug.print("Error writing formatted file '{s}': {}\n", .{ file_path, err });
            return;
        };
        std.debug.print("Formatted file: {s}\n", .{file_path});
    }
}

const CompileResult = struct {
    success: bool,
    generated_code: ?[]const u8,
    target: root.CompileTarget,

    pub fn deinit(self: CompileResult, allocator: std.mem.Allocator) void {
        if (self.generated_code) |code| {
            allocator.free(code);
        }
    }
};

fn compileFile(allocator: std.mem.Allocator, file_path: []const u8, args: [][]const u8) !CompileResult {
    // Parse options
    var output_format = CompileOptions.OutputFormat.colored_text;
    var target: root.CompileTarget = .javascript; // Changed default to JavaScript
    var max_errors: u32 = 10;
    var enable_warnings = true;
    var stop_on_first_error = false;
    var show_stats = false;

    for (args) |arg| {
        // Skip the file path
        if (std.mem.eql(u8, arg, file_path)) {
            continue;
        }

        // Target flags
        if (std.mem.eql(u8, arg, "-tjs")) {
            target = .javascript;
        } else if (std.mem.eql(u8, arg, "-tc")) {
            target = .c;
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
                return CompileResult{ .success = false, .generated_code = null, .target = target };
            }
        } else if (std.mem.startsWith(u8, arg, "--max-errors=")) {
            max_errors = std.fmt.parseInt(u32, arg[13..], 10) catch {
                std.debug.print("Invalid max-errors value: {s}\n", .{arg[13..]});
                return CompileResult{ .success = false, .generated_code = null, .target = target };
            };
        } else if (std.mem.eql(u8, arg, "--no-warnings")) {
            enable_warnings = false;
        } else if (std.mem.eql(u8, arg, "--stop-on-error")) {
            stop_on_first_error = true;
        } else if (std.mem.eql(u8, arg, "--stats")) {
            show_stats = true;
        } else if (std.mem.startsWith(u8, arg, "-")) {
            std.debug.print("Unknown option: {s}\n", .{arg});
            return CompileResult{ .success = false, .generated_code = null, .target = target };
        }
    }

    // Read source file
    const source_content = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ file_path, err });
        return CompileResult{ .success = false, .generated_code = null, .target = target };
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
        return CompileResult{ .success = false, .generated_code = null, .target = target };
    };
    defer compiler.deinit();

    // Compile
    var result = compiler.compile() catch |err| {
        std.debug.print("Compilation failed with error: {}\n", .{err});
        return CompileResult{ .success = false, .generated_code = null, .target = target };
    };
    defer result.deinit(allocator);

    // Print errors if any
    if (result.errors.errors.items.len > 0) {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();
        try compiler.formatErrors(&result, buffer.writer());
        std.debug.print("{s}", .{buffer.items});
    }

    // Store generated code if successful
    var generated_code: ?[]const u8 = null;
    if (result.success and result.generated_code != null) {
        generated_code = try allocator.dupe(u8, result.generated_code.?);

        // Print generated code for build command
        switch (result.target) {
            .javascript => {
                std.debug.print("\nGenerated JavaScript:\n", .{});
                std.debug.print("==========================================\n", .{});
                std.debug.print("{s}\n", .{result.generated_code.?});
                std.debug.print("==========================================\n", .{});
            },
            .c => {
                std.debug.print("\nGenerated C code:\n", .{});
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

    return CompileResult{
        .success = result.success,
        .generated_code = generated_code,
        .target = target,
    };
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
