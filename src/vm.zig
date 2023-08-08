const std = @import("std");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

const Stack = std.atomic.Stack;

pub fn VM() type {
    return struct {
        const Self = @This();

        chunk: *chunk.Chunk(),
        stack: Stack(core.Value()),

        /// Creates a new VM instance. The `destroy` function should be run in order to free
        /// up memory. `defer myVm.destroy()` is possible.
        pub fn init(verbose: bool) !Self {
            _ = verbose;
            // TODO: To be used to print the stack trace and disassemble as we run through bytecode.
            const allocator = std.heap.page_allocator;

            if (allocator.create(chunk.Chunk())) |memory| {
                var newChunk = chunk.Chunk().init(allocator);
                memory.* = newChunk;
                var stack = Stack(core.Value()).init();

                return Self{
                    .chunk = memory,
                    .stack = stack,
                };
            } else |err| {
                return err;
            }
        }

        pub fn push(self: *Self, constant: core.Value()) void {
            const node = std.heap.page_allocator.create(Stack(core.Value()).Node) catch unreachable;
            node.* = Stack(core.Value()).Node{
                .next = undefined,
                .data = constant,
            };
            self.stack.push(node);
        }

        pub fn pop(self: *Self) ?core.Value() {
            const node = self.stack.pop();

            if (node == null) {
                return null;
            }

            return node.?.data;
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
                            self.push(constant);
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
                        var optionalConstant = self.pop();

                        if (optionalConstant) |eConstant| {
                            var constant = eConstant;

                            if (!constant.isNumber) {
                                break :blk core.InterpretResults.RUNTIME_ERROR;
                            }

                            constant.number *= -1;
                            self.push(constant);
                            constant.print();
                            offset += 1;

                            break :blk core.InterpretResults.CONTINUE;
                        }

                        break :blk core.InterpretResults.RUNTIME_ERROR;
                    },
                    .ADD => blk: {
                        var res = self.operation(core.addOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .SUB => blk: {
                        var res = self.operation(core.subOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .MUL => blk: {
                        var res = self.operation(core.mulOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .DIV => blk: {
                        var res = self.operation(core.divOp);

                        if (res == core.InterpretResults.CONTINUE) {
                            offset += 1;
                        }

                        break :blk res;
                    },
                    .MOD => blk: {
                        var res = self.operation(core.divOp);

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
            var bOptional = self.pop();

            if (bOptional) |b| {
                var aOptional = self.pop();

                if (aOptional) |a| {
                    var newValue = op(a, b);
                    self.push(newValue);
                    newValue.print();

                    return core.InterpretResults.CONTINUE;
                }
            }

            return core.InterpretResults.RUNTIME_ERROR;
        }

        pub fn resetStack(self: *Self) void {
            self.stack = Stack(core.Value()).init();
        }

        pub fn destroy(self: Self) void {
            self.chunk.destroy();
            std.heap.page_allocator.destroy(self.chunk);
        }
    };
}
