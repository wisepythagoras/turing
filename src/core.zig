const std = @import("std");

pub const OpCode = enum(u8) {
    RETURN,
    CONSTANT,
};

pub fn returnInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
