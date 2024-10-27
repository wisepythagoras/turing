const std = @import("std");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

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
                const newChunk = chunk.Chunk().init(allocator);
                memory.* = newChunk;
                const stack = std.ArrayList(core.Value()).init(
                    allocator,
                );
                // defer stack.deinit();

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

        pub fn push(self: *Self, constant: core.Value()) !void {
            try self.stack.append(constant);
        }

        pub fn pop(self: *Self) ?core.Value() {
            const node = self.stack.pop();

            // if (node == null) {
            //     return null;
            // }

            return node;
        }

        /// Runs the bytecode in the chunk.
        pub fn run(self: *Self) core.CompilerError!void {
            var offset: usize = 0;

            while (offset < self.chunk.code.items.len) {
                const byte = self.chunk.code.items[offset][0];

                const result = switch (byte) {
                    .CONSTANT => blk: {
                        if (core.readValue(self.chunk, offset)) |constant| {
                            offset += 2;

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
                    .CONSTANT_16 => blk: {
                        // TODO: Implement!
                        offset += 1;
                        break :blk core.InterpretResults.CONTINUE;
                    },
                    .DEFG => blk: {
                        if (core.readValue(self.chunk, offset)) |varName| {
                            offset += 2;

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
                    .GETG => blk: {
                        if (core.readValue(self.chunk, offset)) |varName| {
                            offset += 2;

                            if (!varName.isString()) {
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            if (self.globals.get(varName.toString())) |value| {
                                self.push(value) catch |err| {
                                    std.debug.print("ERROR: {?}\n", .{err});
                                    break :blk core.InterpretResults.RUNTIME_ERROR;
                                };
                            } else {
                                std.debug.print("ERROR: {s} not defined\n", .{varName.toString()});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            break :blk core.InterpretResults.CONTINUE;
                        } else |err| {
                            std.debug.print("ERROR: {?}\n", .{err});
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        }
                    },
                    .NEG => blk: {
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
                    .NOT => blk: {
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
                    .ADD => blk: {
                        const res = self.operation(core.addOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .SUB => blk: {
                        const res = self.operation(core.subOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .MUL => blk: {
                        const res = self.operation(core.mulOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .DIV => blk: {
                        const res = self.operation(core.divOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .MOD => blk: {
                        const res = self.operation(core.divOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .XOR => blk: {
                        const res = self.operation(core.xorOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .POW => blk: {
                        const res = self.operation(core.powOp, null);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .EQ => blk: {
                        const res = self.operation(core.eqOp, byte);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .NE => blk: {
                        const res = self.operation(core.eqOp, byte);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .GT => blk: {
                        const res = self.operation(core.eqOp, byte);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .GE => blk: {
                        const res = self.operation(core.eqOp, byte);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .LT => blk: {
                        const res = self.operation(core.eqOp, byte);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .LE => blk: {
                        const res = self.operation(core.eqOp, byte);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .AND => blk: {
                        const res = self.operation(core.andOp, byte);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .FALSE => blk: {
                        self.push(core.Value().initBool(false)) catch {
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        };

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    .TRUE => blk: {
                        self.push(core.Value().initBool(true)) catch {
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        };

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    .NIL => blk: {
                        self.push(core.Value().initNil()) catch {
                            break :blk core.InterpretResults.RUNTIME_ERROR;
                        };

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    .OUT => blk: {
                        if (self.pop()) |val| {
                            val.print();
                        }

                        offset += 1;

                        break :blk core.InterpretResults.CONTINUE;
                    },
                    .POP => blk: {
                        _ = self.pop();
                        offset += 1;
                        break :blk core.InterpretResults.CONTINUE;
                    },
                    .RETURN => core.InterpretResults.OK,
                };

                if (result == core.InterpretResults.OK) {
                    break;
                } else if (result == core.InterpretResults.RUNTIME_ERROR) {
                    return core.CompilerError.RuntimeError;
                }
            }
        }

        pub fn operation(self: *Self, op: core.OperationFn, ins: ?core.OpCode) core.InterpretResults {
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

        pub fn destroy(self: Self) void {
            self.chunk.destroy();
            std.heap.page_allocator.destroy(self.chunk);
        }
    };
}
