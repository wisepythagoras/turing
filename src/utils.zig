const std = @import("std");

/// Read a source file.
pub fn readFile(fName: []const u8) ![]u8 {
    var turFile = try std.fs.cwd().openFile(fName, .{});
    defer turFile.close();

    var buf_reader = std.io.bufferedReader(turFile.reader());
    var in_stream = buf_reader.reader();
    const allocator = std.heap.page_allocator;

    if (turFile.getEndPos()) |size| {
        if (allocator.alloc(u8, size)) |buf| {
            if (in_stream.read(buf)) |_| {
                return buf;
            } else |err| {
                return err;
            }
        } else |err| {
            return err;
        }
    } else |err| {
        return err;
    }
}

pub fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}
