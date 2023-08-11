const std = @import("std");

pub fn Scaner() type {
    return struct {
        const Self = @This();

        start: []u8,
        current: []u8,
        line: i32,

        pub fn init(source: []u8) Self {
            return Self{
                .start = source,
                .current = source,
                .line = 1,
            };
        }
    };
}
