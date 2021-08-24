const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const fs = std.fs;
const bufSize = 128;

pub fn main() !void {
    // We need a buffer that can stay in scope longer than the result of getLine
    var buffer: [bufSize]u8 = undefined;

    try stdout.print("Enter your name: ", .{});
    var line = try getLine(&buffer);

    while (!isQuitWord(line)) {
        try stdout.print("Greetings {s}.\n", .{line});
        try stdout.print("Enter your name: ", .{});
        line = try getLine(&buffer);
    }

    try stdout.print("Bye bye now.\n", .{});
}

fn writeFile(filename: []const u8, contents: []const u8) anyerror!void {
    const file = try fs.cwd().createFile(
        filename,
        .{ .read = true },
    );
    defer file.close();

    try file.writeAll(contents);
}

fn getLine(buffer: []u8) ![]const u8 {
    const line = try stdin.readUntilDelimiterOrEof(
        buffer,
        '\n',
    );
    return line orelse "";
}

fn isQuitWord(word: []const u8) bool {
    return inline for (.{ "q", "quit", "exit", "bye" }) |quit| {
        if (std.mem.eql(u8, word, quit)) {
            break true;
        }
    } else false;
}
