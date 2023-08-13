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
