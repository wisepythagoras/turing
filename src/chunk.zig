const std = @import("std");
const core = @import("core.zig");

/// Creates a new chunk, which essentially represents a single bytecode instruction
/// group.
/// Remember to `chunk.destroy()` when you're done using. You can even `defer` it.
pub fn Chunk() type {
    return struct {
        const Self = @This();

        code: std.ArrayList(core.OpCode),
        values: std.ArrayList(f64),

        /// Initialize the new Chunk.
        pub fn init(allocator: std.mem.Allocator) Self {
            var code = std.ArrayList(core.OpCode).init(allocator);
            var values = std.ArrayList(f64).init(allocator);

            return Self{
                .code = code,
                .values = values,
            };
        }

        /// This will destroy the instance and free any used memory.
        pub fn destroy(self: Self) void {
            self.code.deinit();
            self.values.deinit();
        }

        /// Writes a single opcode to the chunk.
        pub fn writeOpCode(self: *Self, opCode: core.OpCode) !void {
            try self.code.append(opCode);
        }

        /// Converts a byte to an opcode and writes it to the chunk.
        pub fn writeByte(self: *Self, byte: u8) !void {
            const opCode = @as(core.OpCode, @enumFromInt(byte));
            try self.writeOpCode(opCode);
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

        /// Disassembles the chunk byte-by-byte.
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

fn disassembleInstruction(chunk: *Chunk(), offset: usize) usize {
    const instruction = chunk.code.items[offset];
    return switch (instruction) {
        .RETURN => core.returnInstruction("RETURN", offset),
        .CONSTANT => offset + 1,
    };
}
