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
        values: std.ArrayList(core.Value()),

        // TODO: Keep a linked list of all objects for the purposes of garbage collection.
        // https://pedropark99.github.io/zig-book/Chapters/09-data-structures.html#linked-lists

        /// Initialize the new Chunk.
        pub fn init(allocator: std.mem.Allocator) Self {
            const code = std.ArrayList(CodeTuple).init(allocator);
            const values = std.ArrayList(core.Value()).init(allocator);

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

        pub fn addConstant(self: *Self, constant: core.Value()) !usize {
            if (self.values.append(constant)) {
                return self.values.items.len;
            } else |err| {
                return err;
            }
        }

        // pub fn makeConstant(self: *Self, constant: core.Value()) !usize {
        //     if (self.addConstant(constant)) |constant| {
        //         ???
        //     } else |err| {
        //         return err;
        //     }
        // }

        pub fn emitConstant(self: *Self, constant: core.Value()) !void {
            if (self.addConstant(constant)) |_| {
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

        pub fn replaceConstant(self: *Self, constant: core.Value(), idx: usize) core.CompilerError!void {
            if (idx >= self.values.items.len) {
                return core.CompilerError.InvalidMemoryLookup;
            }

            self.values.items[idx] = constant;
        }

        pub fn toBytes(self: *Self) core.CompilerError![]const u8 {
            var offsetVal: usize = 0;
            const offset = &offsetVal;
            const memory = std.heap.page_allocator;
            const valuesBytes = try core.valuesToBytes(self);
            var res = memory.alloc(u8, valuesBytes.len + 1) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.MemoryError;
            };

            var i: usize = 0;

            for (valuesBytes) |b| {
                res[i] = b;
                i += 1;
            }

            res[i] = 99;
            i += 1;

            var len: usize = res.len;

            while (offset.* < self.code.items.len) {
                if (instructionToBytes(self, offset)) |bytes| {
                    // std.debug.print("{any}\n", .{bytes});
                    len = len + bytes.len;
                    res = memory.realloc(res, len) catch |err| {
                        std.debug.print("ERROR: {?}\n", .{err});
                        return core.CompilerError.MemoryError;
                    };

                    for (bytes) |b| {
                        res[i] = b;
                        i += 1;
                    }
                } else |err| {
                    return err;
                }
            }
            std.debug.print("{any}\n", .{res});

            return res;
        }

        /// Disassembles the chunk byte-by-byte.
        pub fn disassemble(self: *Self) core.CompilerError!void {
            var offset: usize = 0;

            while (offset < self.code.items.len) {
                if (disassembleInstruction(self, offset)) |newOffset| {
                    offset = newOffset;
                } else |err| {
                    return err;
                }
            }

            // for (self.code.items) |opCode| {
            //     _ = opCode;
            //     //
            // }
        }
    };
}

fn instructionToBytes(chunk: *Chunk(), offset: *usize) core.CompilerError![]const u8 {
    const instruction = chunk.code.items[offset.*];
    const opCode = instruction[0];

    // TODO: chunk.values needs to be converted to bytes, instead of reading the value and
    // putting it in place. This way, when run, the code could access those constants that
    // are on the stack.

    return switch (opCode) {
        .RETURN => {
            const retRes = opCode.toBytes() catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.MemoryError;
            };
            offset.* += 1;
            return retRes;
        },
        .CONSTANT, .DEFG, .GETG => {
            return core.constToBytes(chunk, opCode, offset);
        },
        .CONSTANT_16 => {
            offset.* += 2;
            return "";
        },
        .NEG, .ADD, .MUL, .DIV, .SUB, .XOR, .MOD, .POW, .AND, .NOT, .EQ, .NE, .GT, .GE, .LT, .LE, .FALSE, .TRUE, .NIL, .OUT, .POP => {
            const opRes = opCode.toBytes() catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.MemoryError;
            };
            offset.* += 1;
            return opRes;
        },
    };
}

fn disassembleInstruction(chunk: *Chunk(), offset: usize) core.CompilerError!usize {
    const instruction = chunk.code.items[offset];

    if (offset == 0 or (instruction[1] != 0 and instruction[1] != chunk.code.items[offset - 1][1])) {
        // TODO: Formatting based on how many lines there are.
        std.debug.print("{d:4}: ", .{instruction[1]});
    } else {
        std.debug.print("    : ", .{});
    }

    const opCode = instruction[0];
    const opCodeStr = opCode.toString();

    return switch (opCode) {
        .RETURN => core.returnInstruction(opCodeStr, offset),
        .CONSTANT, .DEFG, .GETG => {
            return core.constantInstruction(opCodeStr, chunk, offset);
        },
        .CONSTANT_16 => core.constant16Instruction(opCodeStr, chunk, offset),
        .NEG, .ADD, .MUL, .DIV, .SUB, .XOR, .MOD, .POW, .AND, .NOT, .EQ, .NE, .GT, .GE, .LT, .LE, .OUT, .POP => {
            return core.simpleInstruction(opCodeStr, offset);
        },
        .FALSE, .TRUE => {
            return core.simpleInstruction(opCodeStr, offset);
        },
        .NIL => {
            return core.simpleInstruction(opCodeStr, offset);
        },
    };
}
