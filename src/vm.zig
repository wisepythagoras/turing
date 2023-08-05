const std = @import("std");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

pub fn VM() type {
    return struct {
        const Self = @This();

        chunk: *chunk.Chunk(),

        /// Creates a new VM instance. The `destroy` function should be run in order to free
        /// up memory. `defer myVm.destroy()` is possible.
        pub fn init() !Self {
            const allocator = std.heap.page_allocator;

            if (allocator.create(chunk.Chunk())) |memory| {
                var newChunk = chunk.Chunk().init(allocator);
                memory.* = newChunk;

                return Self{
                    .chunk = memory,
                };
            } else |err| {
                return err;
            }
        }

        /// Runs the bytecode in the chunk.
        pub fn run(self: Self) core.CompilerError!void {
            var offset: usize = 0;

            while (offset < self.chunk.code.items.len) {
                const byte = self.chunk.code.items[offset][0];

                const result = switch (byte) {
                    .CONSTANT => blk: {
                        const constant = core.readConstant(self.chunk, offset);
                        offset += 2;
                        std.debug.print("{d:.6}\n", .{constant});
                        break :blk core.InterpretResults.CONTINUE;
                    },
                    .CONSTANT_16 => blk: {
                        // TODO: Implement!
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

        pub fn destroy(self: Self) void {
            self.chunk.destroy();
            std.heap.page_allocator.destroy(self.chunk);
        }
    };
}
