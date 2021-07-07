const std = @import("std");
const fs = std.fs;

pub fn main() anyerror!void {
    try writeFile("temp1.txt", "testing");
    try writeFile("temp2.txt", "testing\n");
    try writeFile("temp3.txt", "testing\n\n");
}

fn writeFile(filename: []const u8, contents: []const u8) anyerror!void {
    const file = try fs.cwd().createFile(
        filename,
        .{ .read = true },
    );
    defer file.close();

    try file.writeAll(contents);
}
