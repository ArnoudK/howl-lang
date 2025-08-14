const std = @import("std");
const lsp = @import("lib/lsp.zig");
const lsp_server = @import("lib/lsp_server.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
                // This is a request - create a simple request structure
                const method = if (json_value.object.get("method")) |m| m.string else "unknown";
                const request = lsp.LspRequest{
                    .method = method,
                    .id = json_value.object.get("id"),
                    .params = json_value.object.get("params"),
                };
                try server.handleRequest(request, stdout);
            } else {
                // This is a notification - create a simple notification structure  
                const method = if (json_value.object.get("method")) |m| m.string else "unknown";
                const notification = lsp.LspNotification{
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
            buffer[pos-4] == '\r' and buffer[pos-3] == '\n' and
            buffer[pos-2] == '\r' and buffer[pos-1] == '\n') {
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

