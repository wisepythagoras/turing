const std = @import("std");

pub const OpCode = enum(u8) {
    OP_RETURN,
};

pub fn opInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
