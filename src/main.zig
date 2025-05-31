const std = @import("std");
const root = @import("./root.zig");

const stdout_file = std.io.getStdOut().writer();
const stderr_file = std.io.getStdErr().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
        .retain_metadata = true,
        .enable_memory_limit = false,
    }){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            std.log.err("Memory leak detected!", .{});
        }
    }
    const allocator = gpa.allocator();

    // Get command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        stdout_file.print(HelpMessage, .{}) catch {};
        return;
    }

    // Parse command line arguments manually
    var show_tokens = false;
    var show_syntax = false;
    var show_semantic = false;
    var log_level = LogLevel.Default;
    var subcommand: ?SubCommand = null;
    var input_files = std.ArrayList([]const u8).init(allocator);
    defer input_files.deinit();

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--tokens")) {
            show_tokens = true;
        } else if (std.mem.eql(u8, arg, "--syntax")) {
            show_syntax = true;
        } else if (std.mem.eql(u8, arg, "--semantic")) {
            show_semantic = true;
        } else if (std.mem.eql(u8, arg, "--log")) {
            if (i + 1 < args.len) {
                i += 1;
                const log_arg = args[i];
                if (std.mem.eql(u8, log_arg, "Silent")) {
                    log_level = .Silent;
                } else if (std.mem.eql(u8, log_arg, "Verbose")) {
                    log_level = .Verbose;
                } else if (std.mem.eql(u8, log_arg, "Debug")) {
                    log_level = .Debug;
                } else if (std.mem.eql(u8, log_arg, "All")) {
                    log_level = .All;
                } else {
                    log_level = .Default;
                }
            }
        } else if (std.mem.eql(u8, arg, "help")) {
            subcommand = .help;
        } else if (std.mem.eql(u8, arg, "version")) {
            subcommand = .version;
        } else if (std.mem.eql(u8, arg, "build")) {
            subcommand = .build;
        } else if (std.mem.eql(u8, arg, "run")) {
            subcommand = .run;
        } else if (std.mem.eql(u8, arg, "debug")) {
            subcommand = .debug;
        } else if (std.mem.eql(u8, arg, "lsp")) {
            subcommand = .lsp;
        } else if (!std.mem.startsWith(u8, arg, "--")) {
            // This is an input file
            try input_files.append(arg);
        }
    }

    // Default to debug if no subcommand provided
    if (subcommand == null) {
        subcommand = .debug;
    }

    if (log_level != .Silent) {
        stdout_file.print("[LOG LEVEL] set to '{s}'\n", .{@tagName(log_level)}) catch {};
    }

    switch (subcommand.?) {
        .help => {
            stdout_file.print(HelpMessage, .{}) catch {};
            return;
        },
        .version => {
            stdout_file.print("Howl Programming Language v0.0.1\n", .{}) catch {};
        },
        .build => {
            if (input_files.items.len == 0) {
                stderr_file.print("Error: No input files specified for build\n", .{}) catch {};
                return;
            }
            try buildFiles(allocator, input_files.items, show_tokens, show_syntax, show_semantic, log_level);
        },
        .run => {
            if (input_files.items.len == 0) {
                stderr_file.print("Error: No input files specified for run\n", .{}) catch {};
                return;
            }
            try runFiles(allocator, input_files.items, show_tokens, show_syntax, show_semantic, log_level);
        },
        .lsp => {
            stdout_file.print("LSP mode not yet implemented\n", .{}) catch {};
        },
        .debug => {
            if (input_files.items.len == 0) {
                stderr_file.print("Error: No input files specified for debug\n", .{}) catch {};
                return;
            }
            try debugFiles(allocator, input_files.items, show_tokens, show_syntax, show_semantic, log_level);
        },
    }
}

// File processing functions
fn buildFiles(allocator: std.mem.Allocator, files: []const []const u8, show_tokens: bool, show_syntax: bool, show_semantic: bool, log_level: LogLevel) !void {
    if (log_level != .Silent) {
        stdout_file.print("Building {d} file(s)...\n", .{files.len}) catch {};
    }

    for (files) |file_path| {
        try processFile(allocator, file_path, show_tokens, show_syntax, show_semantic, log_level, .build);
    }
}

fn runFiles(allocator: std.mem.Allocator, files: []const []const u8, show_tokens: bool, show_syntax: bool, show_semantic: bool, log_level: LogLevel) !void {
    if (log_level != .Silent) {
        stdout_file.print("Running {d} file(s)...\n", .{files.len}) catch {};
    }

    for (files) |file_path| {
        try processFile(allocator, file_path, show_tokens, show_syntax, show_semantic, log_level, .run);
    }
}

fn debugFiles(allocator: std.mem.Allocator, files: []const []const u8, show_tokens: bool, show_syntax: bool, show_semantic: bool, log_level: LogLevel) !void {
    stdout_file.print("Debug mode: Processing {d} file(s)...\n", .{files.len}) catch {};

    for (files) |file_path| {
        try processFile(allocator, file_path, show_tokens, show_syntax, show_semantic, log_level, .debug);
    }
}

const ProcessMode = enum { build, run, debug };

fn processFile(allocator: std.mem.Allocator, file_path: []const u8, show_tokens: bool, show_syntax: bool, show_semantic: bool, log_level: LogLevel, mode: ProcessMode) !void {
    // Read the file
    const file_contents = std.fs.cwd().readFileAlloc(allocator, file_path, std.math.maxInt(usize)) catch |err| {
        stderr_file.print("Error reading file '{s}': {}\n", .{ file_path, err }) catch {};
        return;
    };
    defer allocator.free(file_contents);

    // Create a copy of the file path for the lexer (it takes ownership)
    const file_name = try allocator.dupe(u8, file_path);
    const file_content_copy = try allocator.dupe(u8, file_contents);

    if (log_level == .Verbose or log_level == .Debug or log_level == .All) {
        stdout_file.print("\n=== Processing file: {s} ===\n", .{file_path}) catch {};
    }

    // Initialize lexer

    var lexer = root.Lexing.Lexer.init(allocator);
    defer lexer.deinit();

    // Add file and tokenize
    lexer.addFile(file_name, file_content_copy);

    // Check for lexer errors
    if (lexer.files.items.len > 0 and lexer.files.items[0].hasErrorTokens) {
        stderr_file.print("Lexer errors found in file: {s}\n", .{file_path}) catch {};

        // Print error tokens
        for (lexer.files.items[0].tokens.items) |token| {
            if (token.kind == .ErrorToken) {
                const error_msg = lexer.files.items[0].fmtLexorErrorToken(token, allocator) catch "Error formatting error message";
                defer allocator.free(error_msg);
                stderr_file.print("{s}\n", .{error_msg}) catch {};
            }
        }
        return;
    }

    // Show tokens if requested or in debug mode
    if (show_tokens or mode == .debug) {
        stdout_file.print("\n--- TOKENS ---\n", .{}) catch {};
        if (lexer.files.items.len > 0) {
            lexer.files.items[0].printTokens();
        }
    }

    // Add syntax analysis using our new simplified AST
    if (show_syntax or mode == .debug) {
        stdout_file.print("\n--- SYNTAX TREE ---\n", .{}) catch {};

        if (lexer.files.items.len > 0) {
            const tokens = lexer.files.items[0].tokens.items;

            // Create AST parser
            var parser = root.Ast.Parser.init(allocator, tokens);
            defer parser.deinit(); // Properly clean up all allocated AST nodes

            // Try parsing with error recovery for better error reporting
            const ast = if (mode == .debug)
                parser.parseWithRecovery()
            else
                parser.parse() catch |err| {
                    stderr_file.print("Parse error: {}\n", .{err}) catch {};

                    // Print detailed error information if available
                    if (parser.hasErrors()) {
                        stderr_file.print("\n--- DETAILED PARSE ERRORS ---\n", .{}) catch {};
                        parser.printErrors();
                    }
                    return;
                };

            // Always check for and display parsing errors
            if (parser.hasErrors()) {
                stderr_file.print("\n--- PARSE ERRORS DETECTED ---\n", .{}) catch {};
                parser.printErrors();

                if (mode != .debug) {
                    stderr_file.print("Use 'debug' mode for error recovery and continued parsing.\n", .{}) catch {};
                    return;
                }
            }

            // Print the AST
            stdout_file.print("AST parsed with {} statements\n", .{ast.block.statements.len}) catch {};
            if (mode == .debug or !parser.hasErrors()) {
                root.Ast.Parser.printAst(ast, 0);
            }
        } else {
            stdout_file.print("No tokens available for parsing\n", .{}) catch {};
        }
    }

    // TODO: Add semantic analysis here when implemented
    if (show_semantic or mode == .debug) {
        stdout_file.print("\n--- SEMANTIC ANALYSIS (TODO) ---\n", .{}) catch {};
        stdout_file.print("Semantic analysis not yet implemented\n", .{}) catch {};
    }

    // Process based on mode
    switch (mode) {
        .build => {
            if (log_level == .Verbose or log_level == .Debug or log_level == .All) {
                stdout_file.print("Build completed successfully for: {s}\n", .{file_path}) catch {};
            }
        },
        .run => {
            if (log_level == .Verbose or log_level == .Debug or log_level == .All) {
                stdout_file.print("Run completed for: {s}\n", .{file_path}) catch {};
            }
            // TODO: Add actual execution logic here
            stdout_file.print("Execution not yet implemented\n", .{}) catch {};
        },
        .debug => {
            stdout_file.print("Debug analysis completed for: {s}\n", .{file_path}) catch {};
        },
    }
}

const HelpMessage =
    \\Usage: howl <command> [options] [files...] 
    \\
    \\Commands:
    \\  help          Show this help message
    \\  version       Show the version number 
    \\              
    \\  build <files> Build the specified .howl files
    \\  run <files>   Build and run the specified .howl files
    \\  debug <files> Run the howl-compiler in debug mode on specified files
    \\                  this is very verbose and meant for debugging the compiler
    \\
    \\  lsp           Start the language server (not yet implemented)
    \\  fmt           Format the project (not yet implemented)
    \\
    \\Examples:
    \\  howl build main.howl
    \\  howl run --tokens main.howl
    \\  howl debug --tokens --syntax main.howl
    \\                
    \\Debug flags:
    \\  --tokens      Print the tokenizer output
    \\  --syntax      Print syntax tree (not yet implemented)
    \\  --semantic    Print semantic analysis output (not yet implemented)
    \\
    \\General flags:
    \\  --log <level> Set the log level
    \\                Values: Default, Silent, Verbose, Debug, All 
;
const SubCommand = enum {
    help,
    version,
    build,
    run,
    lsp,
    debug,
};

const LogLevel = enum {
    Default,
    Silent,
    Verbose,
    Debug,
    All,
};
