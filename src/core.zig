const std = @import("std");
const chunk = @import("chunk.zig");

pub const OpCode = enum(u8) {
    RETURN,
    CONSTANT,
};

pub fn returnInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) usize {
    const idx: u8 = @intFromEnum(c.code.items[offset + 1]);
    const constant: f64 = c.values.items[idx];
    std.debug.print("{s} = 0x{x} ({d})\n", .{ name, idx, constant });

    return offset + 2;
}
