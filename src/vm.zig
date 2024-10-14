const std = @import("std");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

pub fn VM() type {
    return struct {
        const Self = @This();

        chunk: *chunk.Chunk(),
        stack: std.ArrayList(core.Value()),

        /// Creates a new VM instance. The `destroy` function should be run in order to free
        /// up memory. `defer myVm.destroy()` is possible.
        pub fn init(verbose: bool) !Self {
            _ = verbose;
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
                        if (core.readConstant(self.chunk, offset)) |constant| {
                            offset += 2;
                            constant.print();

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
                    .NEGATE => blk: {
                        const optionalConstant = self.pop();

                        if (optionalConstant) |eConstant| {
                            var constant = eConstant;

                            // if (constant.vType != core.ValueType.NUMBER) {
                            //     break :blk core.InterpretResults.RUNTIME_ERROR;
                            // }

                            if (constant.vType != core.ValueType.NUMBER) {
                                break :blk core.InterpretResults.UNEXPECTED_VALUE;
                            }

                            constant.val.number *= -1;
                            // constant.number *= -1;

                            self.push(constant) catch |err| {
                                std.debug.print("ERROR: {?}\n", .{err});
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            };

                            constant.print();
                            offset += 1;

                            break :blk core.InterpretResults.CONTINUE;
                        }

                        break :blk core.InterpretResults.RUNTIME_ERROR;
                    },
                    .ADD => blk: {
                        const res = self.operation(core.addOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .SUB => blk: {
                        const res = self.operation(core.subOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .MUL => blk: {
                        const res = self.operation(core.mulOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .DIV => blk: {
                        const res = self.operation(core.divOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .MOD => blk: {
                        const res = self.operation(core.divOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
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

        pub fn operation(self: *Self, op: core.OperationFn) core.InterpretResults {
            const bOptional = self.pop();

            if (bOptional) |b| {
                const aOptional = self.pop();

                if (aOptional) |a| {
                    const newValue = op(a, b) catch |err| {
                        if (err == core.CompilerError.RuntimeError) {
                            return core.InterpretResults.RUNTIME_ERROR;
                        }

                        return core.InterpretResults.COMPILE_ERROR;
                    };
                    self.push(newValue) catch |err| {
                        std.debug.print("ERROR: {?}\n", .{err});
                        return core.InterpretResults.RUNTIME_ERROR;
                    };
                    newValue.print();

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
