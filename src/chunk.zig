const std = @import("std");
const core = @import("core.zig");

const CodeTuple = std.meta.Tuple(&.{ core.OpCode, usize });

/// Creates a new chunk, which essentially represents a single bytecode instruction
/// group.
/// Remember to `chunk.destroy()` when you're done using. You can even `defer` it.
pub fn Chunk() type {
    return struct {
        const Self = @This();

        code: std.ArrayList(CodeTuple),
        values: std.ArrayList(core.ValueT),

        /// Initialize the new Chunk.
        pub fn init(allocator: std.mem.Allocator) Self {
            var code = std.ArrayList(CodeTuple).init(allocator);
            var values = std.ArrayList(core.ValueT).init(allocator);

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
        pub fn writeOpCode(self: *Self, opCode: core.OpCode, line: usize) !void {
            if (opCode == core.OpCode.CONSTANT) {
                const pos: u16 = @as(u16, @intCast(self.values.items.len));

                if (pos >= 255) {
                    return self.code.append(.{ core.OpCode.CONSTANT_16, line });
                }
            }

            return self.code.append(.{ opCode, line });
        }

        /// Converts a byte to an opcode and writes it to the chunk.
        pub fn writeByte(self: *Self, byte: u8, line: usize) !void {
            const opCode = @as(core.OpCode, @enumFromInt(byte));
            try self.writeOpCode(opCode, line);
        }

        pub fn print(self: Self) void {
            for (self.code.items) |item| {
                std.debug.print(" -> {any} {any} ({any})\n", .{
                    @TypeOf(item[0]),
                    item[0],
                    @intFromEnum(item[0]),
                });
            }
        }

        pub fn addConstant(self: *Self, constant: f64) !void {
            if (self.values.append(constant)) {
                const pos: u16 = @as(u16, @intCast(self.values.items.len)) - 1;

                if (pos <= 255) {
                    return self.writeByte(@as(u8, @intCast(pos)), 0);
                }

                const posA: u8 = @as(u8, @intCast((pos >> 0) & 0xff));
                const posB: u8 = @as(u8, @intCast((pos >> 8) & 0xff));

                if (self.writeByte(posB, 0)) {
                    return self.writeByte(posA, 0);
                } else |err| {
                    return err;
                }
            } else |err| {
                return err;
            }
        }

        /// Disassembles the chunk byte-by-byte.
        pub fn disassemble(self: *Self) void {
            var offset: usize = 0;

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

    if (offset == 0 or (instruction[1] != 0 and instruction[1] != chunk.code.items[offset - 1][1])) {
        // TODO: Formatting based on how many lines there are.
        std.debug.print("{d:4}: ", .{instruction[1]});
    } else {
        std.debug.print("    : ", .{});
    }

    return switch (instruction[0]) {
        .RETURN => core.returnInstruction("OP_RETURN", offset),
        .CONSTANT => core.constantInstruction("OP_CONSTANT", chunk, offset),
        .CONSTANT_16 => core.constant16Instruction("OP_CONSTANT_16", chunk, offset),
    };
}
