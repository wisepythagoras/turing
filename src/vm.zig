const std = @import("std");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const opcode = @import("opcode.zig");

pub fn VM() type {
    return struct {
        const Self = @This();

        chunk: *chunk.Chunk(),
        stack: std.ArrayList(core.Value()),
        verbose: bool,
        globals: std.StringHashMap(core.Value()),

        /// Creates a new VM instance. The `destroy` function should be run in order to free
        /// up memory. `defer myVm.destroy()` is possible.
        pub fn init(verbose: bool) !Self {
            // TODO: To be used to print the stack trace and disassemble as we run through bytecode.
            const allocator = std.heap.page_allocator;

            if (allocator.create(chunk.Chunk())) |memory| {
                const newChunk = chunk.Chunk().init(allocator, verbose);
                memory.* = newChunk;
                const stack = std.ArrayList(core.Value()).init(
                    allocator,
                );

                return Self{
                    .chunk = memory,
                    .stack = stack,
                    .verbose = verbose,
                    .globals = std.StringHashMap(core.Value()).init(allocator),
                };
            } else |err| {
                return err;
            }
        }

        /// Add a value onto the stack.
        pub fn push(self: *Self, constant: core.Value()) !void {
            try self.stack.append(constant);
        }

        /// Get and remove the last value on the stack.
        pub fn pop(self: *Self) ?core.Value() {
            return self.stack.pop();
        }

        /// Get the last value on the stack, without removing it from it.
        pub fn peek(self: *Self) ?core.Value() {
            if (self.stack.items.len == 0) {
                return null;
            }

            return self.stack.items[self.stack.items.len - 1];
        }

        /// Runs the bytecode in the chunk.
        pub fn run(self: *Self) core.CompilerError!void {
            var offset: usize = 0;

            while (offset < self.chunk.code.items.len) {
                const byte = self.chunk.code.items[offset][0];

                const result = switch (byte) {
                    opcode.OpCode.CONSTANT.toU8(), opcode.OpCode.CONSTANT_16.toU8() => blk: {
                        var targetGetter: core.GetterFn = core.readValue;

                        if (byte == opcode.OpCode.CONSTANT_16.toU8()) {
                            targetGetter = core.readValue16;
                        }

                        if (targetGetter(self.chunk, offset)) |constant| {
                            if (byte == opcode.OpCode.CONSTANT_16.toU8()) {
                                offset += 3;
                                // offset += 2;
                            } else {
                                offset += 2;
                            }

                            if (self.verbose) {
                                constant.print();
                            }

                            self.push(constant) catch |err| {
                                std.debug.print("ERROR: {?}\n", .{err});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            };

                            break :blk core.InterpretResults.CONTINUE;
                        } else |err| {
                            std.debug.print("ERROR: {?}\n", .{err});
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        }
                    },
                    // opcode.OpCode.CONSTANT_16.toU8() => blk: {
                    //     // TODO: Implement!
                    //     offset += 3;
                    //     break :blk core.InterpretResults.CONTINUE;
                    // },
                    opcode.OpCode.DEFG.toU8() => blk: {
                        const size = self.chunk.code.items[offset + 1][0];
                        var targetGetter: core.GetterFn = core.readValue;

                        if (size == opcode.OpCode.CONSTANT_16.toU8()) {
                            targetGetter = core.readValue16;
                        }

                        if (targetGetter(self.chunk, offset + 1)) |varName| {
                            offset += 3;

                            if (!varName.isString()) {
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            if (self.pop()) |val| {
                                self.globals.put(varName.toString(), val) catch |err| {
                                    std.debug.print("ERROR: {?}\n", .{err});
                                    break :blk core.InterpretResults.COMPILE_ERROR;
                                };
                            } else {
                                break :blk core.InterpretResults.COMPILE_ERROR;
                            }

                            break :blk core.InterpretResults.CONTINUE;
                        } else |err| {
                            std.debug.print("ERROR: {?}\n", .{err});
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        }
                    },
                    opcode.OpCode.GETG.toU8() => blk: {
                        const size = self.chunk.code.items[offset + 1][0];
                        var targetGetter: core.GetterFn = core.readValue;

                        if (size == opcode.OpCode.CONSTANT_16.toU8()) {
                            targetGetter = core.readValue16;
                        }

                        if (targetGetter(self.chunk, offset + 1)) |varName| {
                            offset += 3;

                            // std.debug.print("{any} {s}\n", .{ size, varName.val.object.toString() });

                            if (!varName.isString()) {
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            if (self.globals.get(varName.toString())) |value| {
                                self.push(value) catch |err| {
                                    std.debug.print("ERROR: {?}\n", .{err});
                                    break :blk core.InterpretResults.RUNTIME_ERROR;
                                };
                            } else {
                                std.debug.print("ERROR: \"{s}\" is not defined\n", .{varName.toString()});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            break :blk core.InterpretResults.CONTINUE;
                        } else |err| {
                            std.debug.print("ERROR: {?}\n", .{err});
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        }
                    },
                    opcode.OpCode.SETG.toU8() => blk: {
                        const size = self.chunk.code.items[offset + 1][0];
                        var targetGetter: core.GetterFn = core.readValue;

                        if (size == opcode.OpCode.CONSTANT_16.toU8()) {
                            targetGetter = core.readValue16;
                        }

                        if (targetGetter(self.chunk, offset + 1)) |varName| {
                            offset += 3;

                            if (!varName.isString()) {
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            if (self.globals.get(varName.toString())) |value| {
                                _ = value; // TODO: Check here if it's a constant in the future.

                                if (self.peek()) |val| {
                                    self.globals.put(varName.toString(), val) catch |err| {
                                        std.debug.print("ERROR: {?}\n", .{err});
                                        break :blk core.InterpretResults.COMPILE_ERROR;
                                    };
                                } else {
                                    break :blk core.InterpretResults.COMPILE_ERROR;
                                }
                            } else {
                                std.debug.print("ERROR: \"{s}\" is not defined\n", .{varName.toString()});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            break :blk core.InterpretResults.CONTINUE;
                        } else |err| {
                            std.debug.print("ERROR: {?}\n", .{err});
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        }
                    },
                    opcode.OpCode.GETL.toU8() => blk: {
                        const size = self.chunk.code.items[offset + 1][0];
                        var targetGetter: core.GetterFn = core.readValue;

                        if (size == opcode.OpCode.CONSTANT_16.toU8()) {
                            targetGetter = core.readValue16;
                        }

                        if (targetGetter(self.chunk, offset + 1)) |slot| {
                            offset += 3;

                            if (!slot.isNumber()) {
                                std.debug.print("ERROR: Memory corruption\n", .{});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            self.push(self.stack.items[@as(usize, @intFromFloat(slot.val.number))]) catch |err| {
                                std.debug.print("ERROR: Memory corruption: {?}\n", .{err});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            };
                        } else |err| {
                            std.debug.print("ERROR: unspecified variable error. {?}\n", .{err});
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        }

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.SETL.toU8() => blk: {
                        const size = self.chunk.code.items[offset + 1][0];
                        var targetGetter: core.GetterFn = core.readValue;

                        if (size == opcode.OpCode.CONSTANT_16.toU8()) {
                            targetGetter = core.readValue16;
                        }

                        if (targetGetter(self.chunk, offset + 1)) |slot| {
                            offset += 3;

                            if (!slot.isNumber()) {
                                std.debug.print("ERROR: Memory corruption\n", .{});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            if (self.peek()) |val| {
                                self.stack.items[@as(usize, @intFromFloat(slot.val.number))] = val;
                            } else {
                                std.debug.print("ERROR: Memory corruption. No value\n", .{});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }
                        } else |err| {
                            std.debug.print("ERROR: unspecified variable error. {?}\n", .{err});
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        }

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.NEG.toU8() => blk: {
                        const optionalConstant = self.pop();

                        if (optionalConstant) |eConstant| {
                            var constant = eConstant;

                            if (constant.vType != core.ValueType.NUMBER) {
                                // Otherwise RUNTIME_ERROR.
                                break :blk core.InterpretResults.UNEXPECTED_VALUE;
                            }

                            constant.val.number *= -1;

                            self.push(constant) catch |err| {
                                std.debug.print("ERROR: {?}\n", .{err});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            };

                            offset += 1;

                            break :blk core.InterpretResults.CONTINUE;
                        }

                        break :blk core.InterpretResults.RUNTIME_ERROR;
                    },
                    opcode.OpCode.NOT.toU8() => blk: {
                        const optionalConstant = self.pop();

                        if (optionalConstant) |constant| {
                            const newRawVal = core.isFalsey(constant);
                            const newVal = core.Value().initBool(newRawVal);

                            self.push(newVal) catch |err| {
                                std.debug.print("ERROR: {?}\n", .{err});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            };

                            offset += 1;

                            break :blk core.InterpretResults.CONTINUE;
                        }

                        break :blk core.InterpretResults.RUNTIME_ERROR;
                    },
                    opcode.OpCode.ADD.toU8() => blk: {
                        const res = self.operation(core.addOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.SUB.toU8() => blk: {
                        const res = self.operation(core.subOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.MUL.toU8() => blk: {
                        const res = self.operation(core.mulOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.DIV.toU8() => blk: {
                        const res = self.operation(core.divOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.MOD.toU8() => blk: {
                        const res = self.operation(core.divOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.XOR.toU8() => blk: {
                        const res = self.operation(core.xorOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.POW.toU8() => blk: {
                        const res = self.operation(core.powOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.EQ.toU8() => blk: {
                        const opCode = @as(opcode.OpCode, @enumFromInt(byte));
                        const res = self.operation(core.eqOp, opCode);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.NE.toU8() => blk: {
                        const opCode = @as(opcode.OpCode, @enumFromInt(byte));
                        const res = self.operation(core.eqOp, opCode);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.GT.toU8() => blk: {
                        const opCode = @as(opcode.OpCode, @enumFromInt(byte));
                        const res = self.operation(core.eqOp, opCode);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.GE.toU8() => blk: {
                        const opCode = @as(opcode.OpCode, @enumFromInt(byte));
                        const res = self.operation(core.eqOp, opCode);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.LT.toU8() => blk: {
                        const opCode = @as(opcode.OpCode, @enumFromInt(byte));
                        const res = self.operation(core.eqOp, opCode);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.LE.toU8() => blk: {
                        const opCode = @as(opcode.OpCode, @enumFromInt(byte));
                        const res = self.operation(core.eqOp, opCode);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.AND.toU8() => blk: {
                        const opCode = @as(opcode.OpCode, @enumFromInt(byte));
                        const res = self.operation(core.eqOp, opCode);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    opcode.OpCode.FALSE.toU8() => blk: {
                        self.push(core.Value().initBool(false)) catch {
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        };

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.TRUE.toU8() => blk: {
                        self.push(core.Value().initBool(true)) catch {
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        };

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.NIL.toU8() => blk: {
                        self.push(core.Value().initNil()) catch {
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        };

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.OUT.toU8() => blk: {
                        if (self.pop()) |val| {
                            val.print();
                        }

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.POP.toU8() => blk: {
                        _ = self.pop();
                        offset += 1;
                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.JWF.toU8() => blk: {
                        const newOffset = try core.readRaw32(self.chunk, offset);

                        if (self.peek()) |val| {
                            if (core.isFalsey(val)) {
                                offset += newOffset;
                            }
                        }

                        offset += 5;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.JMP.toU8() => blk: {
                        const newOffset = try core.readRaw32(self.chunk, offset);
                        offset += newOffset + 5;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    opcode.OpCode.RETURN.toU8() => core.InterpretResults.OK,
                    else => blk: {
                        break :blk core.InterpretResults.COMPILE_ERROR;
                    },
                };

                if (result == core.InterpretResults.OK) {
                    break;
                } else if (result == core.InterpretResults.RUNTIME_ERROR) {
                    return core.CompilerError.RuntimeError;
                }
            }
        }

        pub fn operation(self: *Self, op: core.OperationFn, ins: ?opcode.OpCode) core.InterpretResults {
            const bOptional = self.pop();

            if (bOptional) |b| {
                const aOptional = self.pop();

                if (aOptional) |a| {
                    const newValue = op(a, b, ins) catch |err| {
                        if (err == core.CompilerError.RuntimeError) {
                            return core.InterpretResults.RUNTIME_ERROR;
                        }

                        return core.InterpretResults.COMPILE_ERROR;
                    };
                    self.push(newValue) catch |err| {
                        std.debug.print("ERROR: {?}\n", .{err});
                        return core.InterpretResults.RUNTIME_ERROR;
                    };

                    if (self.verbose) {
                        newValue.print();
                    }

                    return core.InterpretResults.CONTINUE;
                }
            }

            return core.InterpretResults.RUNTIME_ERROR;
        }

        pub fn resetStack(self: *Self) void {
            self.stack = std.ArrayList(core.Value()).init(
                std.heap.page_allocator,
            );
        }

        pub fn destroy(self: *Self) void {
            self.chunk.destroy();
            self.globals.deinit();
            self.stack.deinit();
            std.heap.page_allocator.destroy(self.chunk);
        }
    };
}
