const std = @import("std");
const object = @import("object.zig");
const chunk = @import("chunk.zig");

pub fn Function() type {
    return struct {
        const Self = @This();

        // obj: *object.Object(),
        chunk: *chunk.Chunk(),
        name: *object.String(),
        arity: u32,

        /// Create a new instance of a function object.
        pub fn init(
            // o: *object.Object(),
            ck: *chunk.Chunk(),
            name: *object.String(),
            arity: u32,
        ) Self {
            return Self{
                // .obj = o,
                .chunk = ck,
                .name = name,
                .arity = arity,
            };
        }

        pub fn newFunction(verbose: bool) !*Self {
            const allocator = std.heap.c_allocator;

            const ckPtr = try allocator.create(chunk.Chunk());
            ckPtr.* = chunk.Chunk().init(allocator, verbose);

            const func = try allocator.create(Self);
            func.* = Function().init(ckPtr, null, 0);

            return func;
        }

        pub fn destroy(self: *Self) void {
            const allocator = std.heap.c_allocator;
            allocator.destroy(self.chunk);
            allocator.destroy(self.name);
        }

        pub fn isEqual(self: Self, func: *Function()) bool {
            return self.name.isEqual(func.name);
        }
    };
}
