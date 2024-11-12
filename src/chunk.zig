const std = @import("std");
const core = @import("core.zig");
const object = @import("object.zig");
const opcode = @import("opcode.zig");
const utils = @import("utils.zig");

pub const CodeTuple = std.meta.Tuple(&.{ u8, usize });

/// Creates a new chunk, which essentially represents a single bytecode instruction
/// group.
/// Remember to `chunk.destroy()` when you're done using. You can even `defer` it.
pub fn Chunk() type {
    return struct {
        const Self = @This();

        code: std.ArrayList(CodeTuple),
        values: std.ArrayList(core.Value()),
        verbose: bool,

        // TODO: Keep a linked list of all objects for the purposes of garbage collection.
        // https://pedropark99.github.io/zig-book/Chapters/09-data-structures.html#linked-lists

        /// Initialize the new Chunk.
        pub fn init(allocator: std.mem.Allocator, verbose: bool) Self {
            const code = std.ArrayList(CodeTuple).init(allocator);
            const values = std.ArrayList(core.Value()).init(allocator);

            return Self{
                .code = code,
                .values = values,
                .verbose = verbose,
            };
        }

        /// This will destroy the instance and free any used memory.
        pub fn destroy(self: Self) void {
            self.code.deinit();
            self.values.deinit();
        }

        /// Writes a single opcode to the chunk.
        pub fn writeOpCode(self: *Self, opCode: opcode.OpCode, line: usize) !void {
            if (opCode == opcode.OpCode.CONSTANT) {
                const pos: u16 = @as(u16, @intCast(self.values.items.len));

                if (pos > 255) {
                    return self.code.append(.{ opcode.OpCode.CONSTANT_16.toU8(), line });
                }
            }

            return self.code.append(.{ opCode.toU8(), line });
        }

        /// Converts a byte to an opcode and writes it to the chunk.
        pub fn writeByte(self: *Self, byte: u8, line: usize) !void {
            // const opCode = @as(opcode.OpCode, @enumFromInt(byte));
            // try self.writeOpCode(byte, line);
            return self.code.append(.{ byte, line });
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

        /// Adds a constant in the values array and then the position onto the stack.
        pub fn emitConstant(self: *Self, constant: core.Value()) !void {
            if (self.addConstant(constant)) |newLen| {
                const pos = newLen - 1;

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
                std.debug.print("ERROR: {?} (alloc)\n", .{err});
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
                if (instructionToBytes(self, offset, self.verbose)) |bytes| {
                    // std.debug.print("{any}\n", .{bytes});
                    len = len + bytes.len;
                    res = memory.realloc(res, len) catch |err| {
                        std.debug.print("ERROR: {?} (realloc)\n", .{err});
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

            if (self.verbose) {
                std.debug.print("{any}\n", .{res});
            }

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
        }

        /// Load raw bytes as bytecode into the current chunk. The bytecode that's loaded can then be
        /// run in the VM.
        ///
        /// TODO: Add specs for this.
        /// TODO: This does not always work for 16 bit constants.
        pub fn loadBytecode(self: *Self, bytes: []const u8) !void {
            var isCode = false;
            var i: usize = 0;

            if (self.verbose) {
                std.debug.print("{any}\n", .{bytes});
            }

            const memory = std.heap.page_allocator;

            while (i < bytes.len) {
                const b = bytes[i];

                if (!isCode) {
                    switch (b) {
                        core.ValueType.NIL.toU8() => blk: {
                            std.debug.print("NILL NOT YET IMPLEMENTED\n", .{});
                            break :blk;
                        },
                        core.ValueType.BOOL.toU8() => blk: {
                            std.debug.print("BOOL NOT YET IMPLEMENTED\n", .{});
                            break :blk;
                        },
                        core.ValueType.NUMBER.toU8() => blk: {
                            if (self.verbose) {
                                std.debug.print("Parsing number\n", .{});
                            }

                            var j: usize = i + 1;
                            var result: [8]u8 = undefined;

                            while (j <= i + 8) {
                                result[j - i - 1] = bytes[j];
                                j += 1;
                            }

                            const num: f64 = @as(f64, @bitCast(result));

                            if (bytes[j] != 0) {
                                std.debug.print("Unexpected byte \"{d}\"\n", .{b});
                                // TODO: Handle errors.
                                return;
                            }

                            i = j;

                            _ = try self.addConstant(core.Value().initNumber(num));

                            if (self.verbose) {
                                std.debug.print("  Added number {d} (next byte {d})\n", .{ num, bytes[i + 1] });
                            }

                            break :blk;
                        },
                        core.ValueType.OBJECT.toU8() => blk: {
                            if (self.verbose) {
                                std.debug.print("Parsing object\n", .{});
                            }

                            const objType = try object.ObjectType.fromU8(bytes[i + 1]);
                            var j = i + 2;

                            if (objType == object.ObjectType.STRING) {
                                var str = try memory.alloc(u8, 0);
                                var len: usize = 0;

                                while (j < bytes.len) {
                                    const strByte = bytes[j];

                                    if (strByte == 0) {
                                        // This is the end of the string.
                                        break;
                                    } else {
                                        len += 1;
                                        str = try memory.realloc(str, len);
                                        str[len - 1] = strByte;
                                    }

                                    j += 1;
                                }

                                i = j;

                                const val = try utils.strToObject(str);
                                _ = try self.addConstant(val);

                                if (self.verbose) {
                                    std.debug.print("  Added string \"{s}\" (next byte {d})\n", .{ str, bytes[i + 1] });
                                }
                            } else {
                                std.debug.print("Unexpected object type \"{any}\"\n", .{objType});
                                // TODO: Handle errors.
                                return;
                            }

                            break :blk;
                        },
                        99 => blk: {
                            // This is the byte that signifies that the constants section
                            // has been traversed and the next section contains the code.
                            isCode = true;

                            if (self.verbose) {
                                std.debug.print("Reading code block\n", .{});
                            }

                            break :blk;
                        },
                        else => {
                            std.debug.print("ERROR: Unrecognized byte \"{d}\"\n", .{b});
                            // TODO: Handle errors.
                        },
                    }
                } else {
                    if (self.verbose) {
                        std.debug.print("Byte {d} (i = {d})\n", .{ b, i });
                    }

                    switch (b) {
                        opcode.OpCode.CONSTANT.toU8() => blk: {
                            // TODO: We should somehow handle the size of the values array. If there are more than 255
                            // constants we want to allocate two bytes (or more) for the constants.

                            if (self.verbose) {
                                std.debug.print("CONSTANT {d}\n", .{bytes[i + 1]});
                            }

                            try self.writeOpCode(opcode.OpCode.CONSTANT, 0);
                            try self.writeByte(bytes[i + 1], 0);

                            i += 1;

                            break :blk;
                        },
                        opcode.OpCode.CONSTANT_16.toU8() => blk: {
                            if (self.verbose) {
                                const idxA: u32 = bytes[i + 1];
                                const idxB: u32 = bytes[i + 2];
                                const num: usize = (idxA << 8) | idxB;
                                std.debug.print("CONSTANT_16 {d}\n", .{num});
                            }

                            try self.writeOpCode(opcode.OpCode.CONSTANT_16, 0);
                            try self.writeByte(bytes[i + 1], 0);
                            try self.writeByte(bytes[i + 2], 0);

                            i += 2;

                            break :blk;
                        },
                        opcode.OpCode.DEFG.toU8(), opcode.OpCode.GETG.toU8(), opcode.OpCode.SETG.toU8() => blk: {
                            const opCode = try opcode.OpCode.fromU8(b);

                            if (self.verbose) {
                                std.debug.print("{any} {d}\n", .{ opCode, bytes[i + 1] });
                            }

                            try self.writeOpCode(opCode, 0);

                            break :blk;
                        },
                        else => blk: {
                            const opCode = opcode.OpCode.fromU8(b) catch |err| {
                                std.debug.print("ERROR: Unrecognized opcode \"{d}\". {?}\n", .{ b, err });
                                break :blk;
                            };

                            if (self.verbose) {
                                std.debug.print("{any}\n", .{opCode});
                            }

                            try self.writeOpCode(opCode, 0);

                            // TODO: Handle errors.
                        },
                    }
                }

                i += 1;
            }
        }
    };
}

/// TODO: This needs to be rebuilt, since the commands/arch has changed.
fn instructionToBytes(chunk: *Chunk(), offset: *usize, verbose: bool) core.CompilerError![]const u8 {
    const instruction = chunk.code.items[offset.*];
    const opCode = @as(opcode.OpCode, @enumFromInt(instruction[0]));

    if (verbose) {
        std.debug.print("OPCODE: {?}\n", .{opCode});
    }

    return switch (opCode) {
        .RETURN => {
            const retRes = opCode.toBytes() catch |err| {
                std.debug.print("ERROR: {?} (toBytes)\n", .{err});
                return core.CompilerError.MemoryError;
            };
            offset.* += 1;
            return retRes;
        },
        .CONSTANT, .CONSTANT_16 => {
            return core.constToBytes(chunk, opCode, offset);
        },
        // .DEFG, .GETG, .SETG, .GETL, .SETL => {
        //     return core.constToBytes(chunk, opCode, offset);
        // },
        // .CONSTANT_16 => {
        //     offset.* += 3;
        //     return [3]u8{opCode.toBytes()};
        // },
        .NEG, .ADD, .MUL, .DIV, .SUB, .XOR, .MOD, .POW, .AND, .NOT, .EQ, .NE, .GT, .GE, .LT, .LE, .FALSE, .TRUE, .NIL, .OUT, .POP, .DEFG, .GETG, .SETG, .GETL, .SETL => {
            const opRes = opCode.toBytes() catch |err| {
                std.debug.print("ERROR: {?} (toBytes)\n", .{err});
                return core.CompilerError.MemoryError;
            };
            offset.* += 1;
            return opRes;
        },
        else => {
            std.debug.print("ERROR: ? {?}\n", .{opCode});
            return core.CompilerError.InvalidOperation;
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

    const opCode = @as(opcode.OpCode, @enumFromInt(instruction[0]));
    const opCodeStr = opCode.toString();

    return switch (opCode) {
        .RETURN => core.returnInstruction(opCodeStr, offset),
        .CONSTANT => {
            return core.constantInstruction(opCodeStr, chunk, offset);
        },
        .DEFG, .GETG, .SETG, .GETL, .SETL => {
            return core.varInstruction(opCodeStr, chunk, offset);
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
        .JWF, .JMP, .LOOP => {
            return core.branchInstruction(opCodeStr, chunk, offset);
        },
    };
}
