const std = @import("std");
const core = @import("core.zig");

/// Creates a new chunk, which essentially represents a single bytecode instruction
/// group.
/// Remember to `chunk.destroy()` when you're done using. You can even `defer` it.
pub fn Chunk() type {
    return struct {
        const Self = @This();

        code: std.ArrayList(core.OpCode),

        pub fn init(allocator: std.mem.Allocator) Self {
            var code = std.ArrayList(core.OpCode).init(allocator);
            return Self{
                .code = code,
            };
        }

        pub fn destroy(self: Self) void {
            self.code.deinit();
        }

        pub fn writeOpCode(self: *Self, opCode: core.OpCode) !void {
            try self.code.append(opCode);
        }

        pub fn writeByte(self: *Self, byte: u8) !void {
            const opCode = @as(core.OpCode, @enumFromInt(byte));
            try self.code.append(opCode);
        }

        pub fn print(self: Self) void {
            for (self.code.items) |item| {
                std.debug.print(" -> {any} {any} ({any})\n", .{
                    @TypeOf(item),
                    item,
                    @intFromEnum(item),
                });
            }
        }

        pub fn disassemble(self: *Self) void {
            var offset: usize = 0;
            const testf = "dsadsa";
            _ = testf;

            while (offset < self.code.items.len) {
                offset = disassembleInstruction(self, offset);
            }

            // for (self.code.items) |opCode| {
            //     _ = opCode;
            //     //
            // }
        }
    };
}

pub fn disassembleInstruction(chunk: *Chunk(), offset: usize) usize {
    const instruction = chunk.code.items[offset];
    return switch (instruction) {
        .OP_RETURN => core.opInstruction("OP_RETURN", offset),
    };
}
