const std = @import("std");
const chunk = @import("chunk.zig");

pub const OpCode = enum(u8) {
    RETURN,
    CONSTANT,
    CONSTANT_16,
};

pub const InterpretResults = enum(u8) {
    OK,
    CONTINUE,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const ValueT = f64;
pub const CompilerError = error{ CompileError, RuntimeError };

pub fn readConstant(c: *chunk.Chunk(), offset: usize) f64 {
    const idx: u8 = @intFromEnum(c.code.items[offset + 1][0]);
    const constant: f64 = c.values.items[idx];

    return constant;
}

pub fn returnInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) usize {
    const idx: u8 = @intFromEnum(c.code.items[offset + 1][0]);
    const constant: f64 = c.values.items[idx];
    std.debug.print("{s} = 0x{x} ({d})\n", .{ name, idx, constant });

    return offset + 2;
}

pub fn constant16Instruction(name: []const u8, c: *chunk.Chunk(), offset: usize) usize {
    const idxB: u16 = @intFromEnum(c.code.items[offset + 1][0]);
    const idxA: u16 = @intFromEnum(c.code.items[offset + 2][0]);
    const idx: u16 = (idxB << 8) | idxA;

    const constant: f64 = c.values.items[idx];
    std.debug.print("{s} = 0x{x} ({d})\n", .{ name, idx, constant });

    return offset + 2;
}
