const std = @import("std");
const stdin = std.io.getStdIn();
const stdout = std.io.getStdOut();
const fs = std.fs;
const bufSize = 128;

pub fn main() !void {
    try stdout.writeAll("Enter your name: ");

    // We need a buffer that can stay in scope longer than the result of getLine
    var buffer: [bufSize]u8 = undefined;
    const line = getLine(&buffer);

    try stdout.writer().print("Greetings {s}.\n", .{line});

    //try writeFile("temp1.txt", "testing");
    //try writeFile("temp2.txt", "testing\n");
    //try writeFile("temp3.txt", "testing\n\n");
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
    const line = try stdin.reader().readUntilDelimiterOrEof(
        buffer,
        '\n',
    );
    return line orelse "";
}
