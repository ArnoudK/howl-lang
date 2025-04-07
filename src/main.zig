const std = @import("std");

const lib = @import("howl_lang_lib");
const clap = @import("clap");

const stdout_file = std.io.getStdOut().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    // We can use `parseParamsComptime` to parse a string into an array of `Param(Help)`.
    const params = comptime clap.parseParamsComptime(
        \\--tokens     Flag for debugging the tokenizer.
        \\--syntax     Flag for debugging the syntax tree.
        \\--semantic   Flag for debugging the semantic analysis.
        \\--log <logLevel> Configure how much is logged.     
        \\<subcommand>   
    );

    // Declare our own parsers which are used to map the argument strings to other
    // types.
    const parsers = comptime .{
        .subcommand = clap.parsers.enumeration(SubCommand),
        .logLevel = clap.parsers.enumeration(LogLevel),
    };

    var diag = clap.Diagnostic{};

    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
        // The assignment separator can be configured. `--number=1` and `--number:1` is now
        // allowed.
        .assignment_separators = "=:",
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        var argsI = std.process.argsWithAllocator(gpa.allocator()) catch {};
        defer argsI.deinit();
        _ = argsI.next();

        stdout_file.print("Unknown argument found?\n", .{}) catch {};
        while (argsI.next()) |arg| {
            stdout_file.print("  {s}", .{arg}) catch {};
        }
        stdout_file.print("\n", .{}) catch {};
        return;
    };
    defer res.deinit();

    // get args
    const loglvl = res.args.log orelse LogLevel.Default;
    stdout_file.print("[LOG LEVEL] set to '{s}'\n", .{@tagName(loglvl)}) catch {};

    const subCommand = res.positionals[0] orelse {
        // print help because no subcommand was provided
        stdout_file.print("No subcommand provided.\n", .{}) catch {};
        return;
    };
    switch (subCommand) {
        .help => {
            // Print help message
            stdout_file.print(HelpMessage, .{}) catch {};
            return;
        },
        .version => {
            stdout_file.print("0.0.0\n", .{}) catch {};
        },
        .build => {
            stdout_file.print("Starting build\n", .{}) catch {};
        },
        .run => {
            stdout_file.print("RUN\n", .{}) catch {};
        },
        .lsp => {
            stdout_file.print("LSP\n", .{}) catch {};
        },
        .debug => {
            stdout_file.print("DEBUG\n", .{}) catch {};
        },
    }
}

const HelpMessage =
    \\Usage: howl <command> [options] [args] 
    \\
    \\Commands:
    \\  help        Show this help message
    \\  version     Show the version number 
    \\  env         Show the environment variables
    \\              
    \\  build       Build the project
    \\  run         Build than run the project
    \\                you can pass args with `--` like this:
    \\                      howl run -- argForMyProgram
    \\
    \\  lsp         Start the language server
    \\  fmt         Format the project (todo)
    \\
    \\  debug       Run the howl-compiler in debug mode
    \\                this is very verbose and has and meant for
    \\                debugging the compiler itself
    \\                
    \\              flags for debug:
    \\                --tokens     Print the tokinze output.
    \\                --syntax     Print syntax tokens found.
    \\                --semantic   Print semantic analasis output.
    \\Flags:
    \\  --log <level>   Set the log level.
    \\                  Values: Default, Silent, Verbose, Debug, All 
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
