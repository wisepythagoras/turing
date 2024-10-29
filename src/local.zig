const token = @import("token.zig");

pub fn Local() type {
    return struct {
        const Self = @This();

        name: *token.Token(),
        depth: usize,

        /// Create a new instance of a local variable.
        pub fn new(name: *token.Token(), depth: usize) Self {
            return Self{
                .name = name,
                .depth = depth,
            };
        }
    };
}
