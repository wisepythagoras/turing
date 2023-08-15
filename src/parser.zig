const std = @import("std");
const token = @import("token.zig");

pub fn Parser() type {
    return struct {
        const Self = @This();

        previous: ?token.Token(),

        pub fn init() Self {
            return Self{
                .previous = null,
            };
        }
    };
}
