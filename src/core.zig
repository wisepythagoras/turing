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

pub const Value = struct {
    const Self = @This();

    number: f64,
    isNumber: bool,

    pub fn print(self: Self) void {
        if (self.isNumber) {
            std.debug.print("{d:.6}\n", .{self.number});
        }
    }
};

pub const CompilerError = error{ CompileError, RuntimeError, InvalidMemoryLookup };

pub fn readConstant(c: *chunk.Chunk(), offset: usize) CompilerError!Value {
    if (offset + 1 > c.code.items.len) {
        return CompilerError.InvalidMemoryLookup;
    }

    const idx: u8 = @intFromEnum(c.code.items[offset + 1][0]);
    return c.values.items[idx];
}

pub fn returnInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) usize {
    const idx: u8 = @intFromEnum(c.code.items[offset + 1][0]);
    const constant: Value = c.values.items[idx];

    if (constant.isNumber) {
        std.debug.print("{s} = 0x{x} ({d})\n", .{ name, idx, constant.number });
    }

    return offset + 2;
}

pub fn constant16Instruction(name: []const u8, c: *chunk.Chunk(), offset: usize) usize {
    const idxB: u16 = @intFromEnum(c.code.items[offset + 1][0]);
    const idxA: u16 = @intFromEnum(c.code.items[offset + 2][0]);
    const idx: u16 = (idxB << 8) | idxA;

    const constant: Value = c.values.items[idx];

    if (constant.isNumber) {
        std.debug.print("{s} = 0x{x} ({d})\n", .{ name, idx, constant.number });
    }

    return offset + 2;
}
