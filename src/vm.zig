const std = @import("std");
const chunk = @import("chunk.zig");

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

        pub fn destroy(self: Self) void {
            self.chunk.destroy();
            std.heap.page_allocator.destroy(self.chunk);
        }
    };
}
