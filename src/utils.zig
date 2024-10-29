const std = @import("std");
const core = @import("core.zig");
const object = @import("object.zig");

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

/// Checks if a character is a digit. This is either a 0-9 char or a decimal.
pub fn isDigit(char: u8, withDecimal: bool) bool {
    return char >= '0' and char <= '9' or (withDecimal and char == '.');
}

/// Checks if a character is an alpha or an underscore.
pub fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or
        (char >= 'A' and char <= 'Z') or
        char == '_';
}

/// Returns whether a character is alphanumeric. This excludes dots (decimal point).
pub fn isAlphaNum(char: u8) bool {
    return isAlpha(char) or isDigit(char, false);
}

/// Compares two strings. Shorthand for `std.mem.eql`.
pub fn strcomp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

/// Convert a u8 array to an object value.
pub fn strToObject(str: []const u8) !core.Value() {
    const strObj = object.String().init(str);

    if (object.Object().init(strObj)) |obj| {
        const memory = std.heap.page_allocator;
        const o = try memory.create(object.Object());
        o.* = obj;

        return core.Value().initObj(o);
    }

    return core.CompilerError.RuntimeError;
}

/// Concatenate two strings (`[]const u8`).
pub fn concatStrs(a: []const u8, b: []const u8) []const u8 {
    const memory = std.heap.page_allocator;
    var result = memory.alloc(u8, a.len + b.len) catch unreachable;

    std.mem.copyForwards(u8, result[0..a.len], a);
    std.mem.copyForwards(u8, result[a.len..], b);

    return result;
}
