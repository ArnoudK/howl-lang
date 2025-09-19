const std = @import("std");

pub const LogLevel = enum {
    Debug,
    Info,
    Warn,
    Error,

    pub fn toString(self: LogLevel) []const u8 {
        return switch (self) {
            .Debug => "DEBUG",
            .Info => "INFO",
            .Warn => "WARN",
            .Error => "ERROR",
        };
    }
};

pub const LspLogger = struct {
    allocator: std.mem.Allocator,
    file: ?std.fs.File,
    enabled: bool,
    min_level: LogLevel,

    pub fn init(allocator: std.mem.Allocator, log_file_path: ?[]const u8, min_level: LogLevel) LspLogger {
        var file: ?std.fs.File = null;

        if (log_file_path) |path| {
            // Try to create/open the log file
            file = std.fs.cwd().createFile(path, .{ .truncate = false }) catch |create_err| {
                std.debug.print("Failed to create LSP log file '{s}': {}\n", .{ path, create_err });
                null;
            };

            // If we successfully opened the file, seek to the end for appending
            if (file) |f| {
                f.seekFromEnd(0) catch {
                    // If seeking fails, still try to use the file
                };
            }
        }

        return LspLogger{
            .allocator = allocator,
            .file = file,
            .enabled = true,
            .min_level = min_level,
        };
    }

    pub fn deinit(self: *LspLogger) void {
        if (self.file) |file| {
            file.close();
        }
    }

    pub fn log(self: *LspLogger, level: LogLevel, comptime fmt: []const u8, args: anytype) void {
        if (!self.enabled or @intFromEnum(level) < @intFromEnum(self.min_level)) {
            return;
        }

        // Get current timestamp
        const timestamp = std.time.milliTimestamp();
        const dt = std.time.epoch.EpochSeconds{ .secs = @intCast(@divTrunc(timestamp, 1000)) };
        const year_day = dt.getEpochDay().calculateYearDay();
        const month_day = year_day.calculateMonthDay();
        const day_seconds = dt.getDaySeconds();

        // Format the log message
        var buffer: [2048]u8 = undefined;
        const formatted_msg = std.fmt.bufPrint(&buffer, fmt, args) catch "LOG_FORMAT_ERROR";

        // Create the full log entry
        var log_buffer: [4096]u8 = undefined;
        const log_entry = std.fmt.bufPrint(&log_buffer, "[{d:0>4}-{d:0>2}-{d:0>2} {d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}] [{}] {s}\n", .{
            year_day.year,
            month_day.month.numeric(),
            month_day.day_index + 1,
            day_seconds.getHoursIntoDay(),
            day_seconds.getMinutesIntoHour(),
            day_seconds.getSecondsIntoMinute(),
            @mod(timestamp, 1000),
            level.toString(),
            formatted_msg,
        }) catch "LOG_TIMESTAMP_ERROR\n";

        // Write to file if available, otherwise to stderr
        if (self.file) |file| {
            _ = file.writeAll(log_entry) catch {};
            // Force flush for important logs
            if (@intFromEnum(level) >= @intFromEnum(LogLevel.Warn)) {
                file.sync() catch {};
            }
        } else {
            // Fallback to stderr for debugging
            std.debug.print("{s}", .{log_entry});
        }
    }

    pub fn debug(self: *LspLogger, comptime fmt: []const u8, args: anytype) void {
        self.log(.Debug, fmt, args);
    }

    pub fn info(self: *LspLogger, comptime fmt: []const u8, args: anytype) void {
        self.log(.Info, fmt, args);
    }

    pub fn warn(self: *LspLogger, comptime fmt: []const u8, args: anytype) void {
        self.log(.Warn, fmt, args);
    }

    pub fn err(self: *LspLogger, comptime fmt: []const u8, args: anytype) void {
        self.log(.Error, fmt, args);
    }

    pub fn logError(self: *LspLogger, error_value: anyerror, context: []const u8) void {
        self.err("Error in {s}: {}", .{ context, error_value });
    }

    pub fn logRequest(self: *LspLogger, method: []const u8, id: ?std.json.Value) void {
        if (id) |request_id| {
            self.debug("Handling request: {} (id: {})", .{ method, request_id });
        } else {
            self.debug("Handling notification: {s}", .{method});
        }
    }

    pub fn logResponse(self: *LspLogger, method: []const u8, success: bool) void {
        if (success) {
            self.debug("Successfully handled: {s}", .{method});
        } else {
            self.warn("Failed to handle: {s}", .{method});
        }
    }
};

// Global logger instance for convenience
var global_logger: ?LspLogger = null;

pub fn initGlobalLogger(allocator: std.mem.Allocator, log_file_path: ?[]const u8, min_level: LogLevel) void {
    if (global_logger) |*logger| {
        logger.deinit();
    }
    global_logger = LspLogger.init(allocator, log_file_path, min_level);
}

pub fn deinitGlobalLogger() void {
    if (global_logger) |*logger| {
        logger.deinit();
        global_logger = null;
    }
}

pub fn getLogger() ?*LspLogger {
    return if (global_logger) |*logger| logger else null;
}

// Convenience functions for global logger
pub fn debug(comptime fmt: []const u8, args: anytype) void {
    if (getLogger()) |logger| logger.debug(fmt, args);
}

pub fn info(comptime fmt: []const u8, args: anytype) void {
    if (getLogger()) |logger| logger.info(fmt, args);
}

pub fn warn(comptime fmt: []const u8, args: anytype) void {
    if (getLogger()) |logger| logger.warn(fmt, args);
}

pub fn err(comptime fmt: []const u8, args: anytype) void {
    if (getLogger()) |logger| logger.err(fmt, args);
}

pub fn logError(error_value: anyerror, context: []const u8) void {
    if (getLogger()) |logger| logger.logError(error_value, context);
}
