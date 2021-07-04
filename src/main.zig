const std = @import("std");
const fs = std.fs;

pub fn main() anyerror!void {
    const file = try fs.cwd().createFile(
        "temp.txt",
        .{ .read = true },
    );
    defer file.close();

    try file.writeAll("All your codebase are belong to us.\n\n");
}
