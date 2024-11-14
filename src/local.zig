const std = @import("std");
const token = @import("token.zig");

pub const LocalTuple = std.meta.Tuple(&.{ *Local(), usize });

pub fn Local() type {
    return struct {
        const Self = @This();

        name: *token.Token(),
        depth: usize,
        immutable: bool,

        /// Create a new instance of a local variable.
        pub fn new(name: *token.Token(), depth: usize, isConst: bool) Self {
            return Self{
                .name = name,
                .depth = depth,
                .immutable = isConst,
            };
        }

        /// Setter for the depth field.
        pub fn setDepth(self: *Self, depth: usize) void {
            self.depth = depth;
        }
    };
}
